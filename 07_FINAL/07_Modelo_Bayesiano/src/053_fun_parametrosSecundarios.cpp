#include <math.h>
#include <RcppArmadillo.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

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
  k21 = Cl / V2;
  
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