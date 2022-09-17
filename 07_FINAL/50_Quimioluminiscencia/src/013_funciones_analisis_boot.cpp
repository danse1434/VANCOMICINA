#include <math.h>
#include <RcppArmadillo.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

//'------------------------------------------------------------------------------
//' Función de signo de convergencia
//'
//' @param A vector con indicador de convergencia vs iteración
//' @param j posición
//' @param l longitud del vector
//' @export
//' @return 
//' vector con SE de estimación de vectores
//' @examples
//' slop_signC(A, i, 100)
//'
// [[Rcpp::export()]]
arma::vec slop_signC(arma::vec A, int j, int l){
  // Crear vector con tamaño l+1
  arma::vec w(l+1);
  // LLenar el vector con valores desde 1 hasta l+1
  std::iota(w.begin(), w.end(), 1);
  
  // Crear una matriz de l+1 x 2 (matriz de diseño)
  arma::mat x(l+1,2);
  // Todos los elementos de la primera columna son uno (intercepto)
  x.col(0).ones();
  
  // Llenar los elementos de la primera columna con el vector *w* (pendiente)
  for(int i = 0; i < (l+1); i++){
    x(i,1) = w[i];
  }
  
  // Crear otro vector con tamaño l+1 (observaciones)
  arma::vec y(l+1);
  
  // Llenar el vector con las observaciones
  for(int i = 0; i < (l+1); i++){
    y[i] = A[j - l - 1 + i];
  }
  // Realizar una inversión de la matriz
  arma::vec theta = inv(x.t()*x)*x.t()*y;
  // Grados de libertad
  int n = x.n_rows;
  int p = theta.n_rows;
  // Residuales
  arma::vec D = y - (x*theta);
  // 
  arma::vec M = (D.t() * D)/(n-p);
  // mse
  double MSE = M(0,0);
  //
  arma::vec S = arma::sqrt(MSE*arma::diagvec(inv(x.t()*x)) );
  
  return abs(theta)/S;
}

//'------------------------------------------------------------------------------
//' Extraer pendiente de regresión 
//'
//' @param A vector con indicador de convergencia vs iteración
//' @param j posición
//' @param l longitud del vector
//' @export
//' @return 
//' retorna double con valor de estadístico T para pendiente o NA_REAL
//' @examples
//' slop_signC1(A, i, 100)
//'
double slop_signC1(arma::vec A, int j, int l){
  if(j > l){
    /* Extraer vector de T */
    arma::vec parametros = slop_signC(A, j, l);
    double T = parametros[1];
    return T; 
  }
  return NA_REAL;
}

//'--------------------------------------------------------------------------------
//' Calculo punto a punto de convergencia para el algoritmo SAEM
//' 
//' @param vectConverg un objeto de tipo vector numérico con resultados de 
//' indicador de convergencia para cada iteración. 
//' @export 
//' @return 
//' retorna DataFrame con columna de iteración y estadisticoT
//' @examples

// [[Rcpp::export()]]
DataFrame evalConvergencia(SEXP vectConverg, int l){
  // Convertir de objeto auto
  Rcpp::NumericVector VC(vectConverg);
  // Convertir VC (tipo vec) a VCvector (tipo arma)
  arma::vec VCvector = as<arma::vec>(VC);
  // Tamaño de longitud
  int Nrow = VC.length();
  // Crear vectores con tamaño Nrow
  arma::vec a1(Nrow), a2(Nrow);
  // Calcular la convergencia en cada iteración
  for(int i=0; i < Nrow; i++){
    a1[i] = i + 1;
    a2[i] = slop_signC1(VCvector, i, l);
  }
  DataFrame Y = DataFrame::create(
    _["iteracion"] = a1,
    _["estadistico_T"] = a2
  );
  return Y;
}

//'------------------------------------------------------------------------------
//' Calcular parámetros secundarios
//'
//' @param D dosis
//' @param Cl aclaramiento renal
//' @param Q aclaramiento intercompartimental
//' @param V1 volumen central
//' @param V2 volumen periférico
//' @export
//' @examples
//' constants_fun(2000, 10.65, 13.61, 21.21, 23.02)
//'
// [[Rcpp::export()]]
DataFrame constants_fun(double D, double Cl, double Q, double V1, double V2){
  // Constantes de velocidad
  double k10, k12, k21;
  k10 = Cl / V1;
  k12 = Q / V1;
  k21 = Q / V2;
  
  // Microconstantes
  double alpha, beta;
  double r1, r2;
  r1 = k10 + k12 + k21;
  r2 = 4 * k10 * k21;
  
  alpha = (r1 + sqrt(pow(r1,2) - r2)) / 2;
  beta =  (r1 - sqrt(pow(r1,2) - r2)) / 2;
  
  // Tiempos de vida media
  double t_alpha, t_beta;
  t_alpha = log(2)/alpha;
  t_beta  = log(2)/beta;
  
  // Macroconstantes
  double A, B;
  double denom = V1 * (alpha - beta);
  A = D * (alpha - k21) / denom;
  B = D * (k21 - beta)  / denom;
  
  // Definición de data.frame final
  DataFrame Y;
  Y = DataFrame::create(
    Named("k10")     = k10,
    Named("k12")     = k12,
    Named("k21")     = k21,
    Named("alpha")   = alpha,
    Named("beta")    = beta, 
    Named("t_alpha") = t_alpha,
    Named("t_beta")  = t_beta,
    Named("A")       = A,
    Named("B")       = B);
  return Y;
}


// [[Rcpp::export()]]
double division(int a, int b) {
  try{
    if( b == 0 ) {
      throw std::range_error("Division por cero!");
      return NA_REAL;
    } 
    double z = a/b;
    return z;
  } catch(std::exception &ex) {
    forward_exception_to_r(ex);
  } catch(...){
    ::Rf_error("C++ excepcion (razón desconocida)");
  }
  return NA_REAL;
}

//'-----------------------------------------------------------------------------
//' Función de estimación de Jacknife
//'  @param vector de parámetros
// [[Rcpp::export()]]
arma::mat jacknife_estim(arma::vec param){
  int n = param.size();
  //Matriz sin el elemento correspondiente
  arma::mat p1(n, n);
  
  for(int i=0; i < n; i++){
    for(int j=0; j < n; j++){
      if(i == j){
        p1(i, j) = 0;
      } else {
        p1(i, j) = param[i];
      }
    }
  }
  
  // Vector con estimadores Jack 
  arma::vec p2(n);
  
  for(int i = 0; i < n; i++){
    p2[i] = sum( p1.col(i) / (n - 1));
  }
  return p2;
}

//----------------------------------------------------------------------------#
//' Factor de aceleración BCa
//' @param param vector de parámetros estimados
//'
// [[Rcpp::export()]]
double accel_f(arma::vec param){
  // Tamaño del vector
  int n = param.size();
  // Crear un vector con estimaciones de media con Jacknife
  arma::vec p1_jack(n);
  p1_jack = jacknife_estim(param);
  
  // Numerador de factor de aceleración
  double num = 0;
  double p1_mean = arma::mean(p1_jack);
  
  for(int i=0; i<n; i++){
    num += pow(p1_mean - p1_jack[i], 3);
  }
  // Denominador de factor de aceleración
  double den1;
  double den0 = 0;
  
  for(int i=0; i<n; i++){
    den0 += pow(p1_mean - p1_jack[i], 2);
  }
  den1 = pow(6 * pow(den0, 3), 3/2);
  
  // Factor de aceleración
  double a = num/den1;
  return a;
}

//---------------------------------------------------------------------------#
//' Factor de sesgo BCa
//' @param param vector de parámetros 
//' @param t0 parámetro estimado en el set orioginal
//' 
// [[Rcpp::export()]]
double bias_f(arma::vec param, double t0){
  int n = param.size();
  // Vector auxiliar que permite conocer no. de casos inferiores
  arma::vec p_aux(n);
  
  for(int i=0; i<n; i++){
    if(param[i] < t0){
      p_aux[i] = 1;
    } else {
      p_aux[i] = 0;
    }
  }
  
  double z = R::qnorm5(sum(p_aux) / n, 0, 1, 1, false);
  return(z);
  
}

//' Intervalos de confianza para Bootstrap
//' 
//' @param param vector de resultados de Bootstap
//' @param t0 parámetro estimado en el set original
//' @param alpha nivel de significancia
//' 
//' @return
//' @example
//' 
// [[Rcpp::export()]]
DataFrame confints(arma::vec param, double t0, double alpha){
  double n;
  n = param.n_rows;
  
  // Percentiles
  arma::vec p;
  p = {alpha/2, 1-(alpha/2)};
  
  // Estadísticos del vector
  double mean, desv;
  mean = arma::mean(param);
  desv = arma::stddev(param);
  
  // Intervalos clásicos
  List IC_classic;
  IC_classic = List::create(
    _["LI"] = mean - (R::qt(p[1], n-1, 1, false) * desv/sqrt(n)), 
    _["LS"] = mean - (R::qt(p[0], n-1, 1, false) * desv/sqrt(n))
  );
  
  // Intervalos percentil
  List IC_percent;
  arma::vec Q = arma::quantile(param, p);
  IC_percent = List::create(
    _["LI"] = Q[0],
    _["LS"] = Q[1]);
  
  // Intervalos normales
  List IC_normal;
  IC_normal = List::create(
    _["LI"] = (2*t0 - mean) - (R::qnorm5(p[1], 0, 1, 1, false) * desv/sqrt(n)),
    _["LS"] = (2*t0 - mean) - (R::qnorm5(p[0], 0, 1, 1, false) * desv/sqrt(n))
  );
  
  // Intervalos con pivote
  double IC_percent_LS = IC_percent["LS"];
  double IC_percent_LI = IC_percent["LI"];
  
  List IC_pivote;
  IC_pivote = List::create(
    _["LI"] = (2 * t0) - IC_percent_LS,
    _["LS"] = (2 * t0) - IC_percent_LI
  );
  
  // Intervalos BCAa
  double alpha1_fac, alpha2_fac, alpha1, alpha2;
  double Z_a  = R::qnorm5(alpha,0,1,1,0);
  double Z_1a = R::qnorm5(1-alpha,0,1,1,0);
  double bhat = bias_f(param, t0);
  double ahat = accel_f(param);
  
  alpha1 = bhat + ( (bhat + Z_a) / (1 - (ahat * (bhat + Z_a))) );
  alpha2 = bhat + ( (bhat + Z_1a) / (1 - (ahat * (bhat + Z_1a))) );
  
  alpha1_fac = R::pnorm5(alpha1, 0,1,1,0);
  alpha2_fac = R::pnorm5(alpha2, 0,1,1,0);
  
  arma::vec alpha_vec = {alpha1_fac, alpha2_fac};
  
  //' La función quantile no acepta valores NaN, por esto se adicionó este 
  //' condicional para eliminar el error mortal. 
  if(std::isnan(alpha_vec[0]) | std::isnan(alpha_vec[1])){
    alpha_vec[0] = 0.5;
    alpha_vec[1] = 0.5;
  }
  
  arma::vec Q_BCa = arma::quantile(param, alpha_vec);
  
  List IC_BCa;
  IC_BCa = List::create(
    _["LI"] = Q_BCa[0],
    _["LS"] = Q_BCa[1]
  );
  
  // DataFrame Final
  DataFrame Y = DataFrame::create(
    _["classic"] = IC_classic,
    _["percent"] = IC_percent,
    _["normal"]  = IC_normal, 
    _["pivote"]  = IC_pivote,
    _["BCa"]     = IC_BCa
  );
  return Y;
}

// [[Rcpp::export]]
NumericVector verificadorContinuidad(LogicalVector pvalLog){
  
  Rcpp::IntegerVector x(pvalLog.size());

  for(int i=0; i < pvalLog.size(); i++){
    if(pvalLog[i] == true){
      x[i] = 1;
    } else {
      x[i] = 0;
    }
  }
  
  Rcpp::NumericVector y(x.length());
  int acumulador = 0;
  
  for(int i=0; i < x.length(); i++){
    if(x[i] == 0){
      acumulador = 0;
    }
    acumulador += x[i];
    y[i] = acumulador;
  }
  return y;
}


// [[Rcpp::export()]]
SEXP foo( SEXP x, SEXP y){
  Rcpp::NumericVector xx(x), yy(y) ;
  int n = xx.size() ;
  Rcpp::NumericVector res( n ) ;
  double x_ = 0.0, y_ = 0.0 ;
  for( int i=0; i<n; i++){
    x_ = xx[i] ;
    y_ = yy[i] ;
    if( x_ < y_ ){
      res[i] = x_ * x_ ;
    } else {
      res[i] = -( y_ * y_) ;
    }
  }
  return res ;
}






















