/**
 Modelo de Dos Compartimentos con Infusión y MultiDosis
*/

functions{
  
  /**
  * Modelo de dos compartimentos con capacidad de recibir historial de 
  * dosificación.
  * 
  * @param time vector con tiempos de dosificación (en orden)
  * @param dose vector de dosis (en orden) 
  * @param tinf vector de t. de infusión (en orden)
  * @param dt tiempo
  * @param CL Clearance
  * @param Q Clearance intercompartimental
  * @param V1 Volumen de distribución
  * @param V2 Volumen periférico
  *
  */
  real twoCPTM_model_simple(real[] time, real[] dose, real[] tinf, real dt, 
        real CL, real Q, real V1, real V2){
    // Longitud total de matriz de historial de dosis
    int n_bolus = size(time);
    // Longitud filtrada
    int nb = 0;
    
    // Parámetros secundarios
    real k10;
    real k12;
    real k21;
    real ksum;
    vector[2] A;
    vector[2] alpha;
    // Predicciones
    real Cp = 0;
    // Variables auxiliares
    real D = 0;
    real t0 = 0;
    real t_inf = 0;
    
    // Acumula el N. de dosis administradas a dt
    for(n in 1:n_bolus){
      if(!(time[n]>dt)){
        nb += 1;
      }
    }
    
    k10 = CL / V1;
    k12 = Q / V1;
    k21 = Q / V2;
    ksum = k10 + k12 + k21;
    
    alpha[1] = (ksum + sqrt((ksum^2) - (4.0 * k10 * k21)))/2.0;
    alpha[2] = (ksum - sqrt((ksum^2) - (4.0 * k10 * k21)))/2.0;

    A[1] = (1/V1)*(alpha[1]-k21)/(alpha[1]-alpha[2]);
    A[2] = (1/V1)*(k21-alpha[2])/(alpha[1]-alpha[2]);
    
    if( !((dt - time[nb]) > (tinf[nb])) ){
      // Predomina la acumulación del fármaco
      
      for(i in 1:nb){
        D = dose[i];
        t0 = time[i];
        t_inf = tinf[i];
        
        if(i==nb){
          // en la última dosis
          Cp += (D/t_inf) * 
          ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*(dt-t0))) + 
           (A[2]/alpha[2]) * (1 - exp(-alpha[2]*(dt-t0))));
         } else {
         Cp += (D/t_inf) * 
         ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*t_inf)) * exp(-alpha[1]*(dt-t0-t_inf)) + 
         (A[2]/alpha[2]) * (1 - exp(-alpha[2]*t_inf)) * exp(-alpha[2]*(dt-t0-t_inf))
         );
          }
      }
    } else {
      // Predomina la eliminación del fármaco
      for(i in 1:nb){
        D = dose[i];
        t0 = time[i];
        t_inf = tinf[i];
        
        Cp += (D/t_inf) * 
            ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*t_inf)) * exp(-alpha[1]*(dt-t0-t_inf)) + 
               (A[2]/alpha[2]) * (1 - exp(-alpha[2]*t_inf)) * exp(-alpha[2]*(dt-t0-t_inf)) ); 
      }
    }
    
    return (max([Cp, 1e-6]));
  }
}

data{
  // Eventos de Observación
  int nObs;
  vector[nObs] cObs;
  vector[nObs] time;
  
  int nSubjects;
  /** En este caso permite definir donde empieza y 
      terminan los datos para cada ID */
  int start[nSubjects]; 
  int end[nSubjects];
  int subject[nObs];
  
  // Eventos de Dosificación
  int nAdmEv;
  int startAdmEv[nSubjects];
  int endAdmEv[nSubjects];
  
  real timeAdm[nAdmEv];
  real dose[nAdmEv];
  real tinf[nAdmEv];
  
  // Hiperparámetros
  real muClHat;
  real<lower=0> sdClHat;
  real muQHat;
  real<lower=0> sdQHat;
  real muV1Hat;
  real<lower=0> sdV1Hat;
  real muV2Hat;
  real<lower=0> sdV2Hat;
  
  real muOmega[4];
  real<lower=0> sdOmega[4];
  real etaRho;
  real<lower=0> muSigma; 
  real<lower=0> sdSigma;
}

transformed data{
  vector[nObs] logcObs = log(cObs);
  int nRandom = 4;
}

parameters{
  real<lower = 0> CLHat;
  real<lower = 0> QHat;
  real<lower = 0> V1Hat;
  real<lower = 0> V2Hat;
  corr_matrix[nRandom] rho;
  vector<lower = 0>[nRandom] omega;
  real<lower = 0> sigma;
  vector[nRandom] logtheta[nSubjects];
}

transformed parameters{
  vector<lower = 0>[nRandom] thetaHat;
  cov_matrix[nRandom] Omega;
  
  real<lower = 0> CL[nSubjects];
  real<lower = 0> Q[nSubjects];
  real<lower = 0> V1[nSubjects];
  real<lower = 0> V2[nSubjects];
  vector<lower = 0>[nObs] cHat;

  thetaHat[1] = CLHat;
  thetaHat[2] = QHat;
  thetaHat[3] = V1Hat;
  thetaHat[4] = V2Hat;

  Omega = quad_form_diag(rho, omega);
  /*Omega = diag_matrix(omega); (para forma diagonal)*/
  
  CL = exp(logtheta[,1]);
  Q  = exp(logtheta[,2]);
  V1 = exp(logtheta[,3]);
  V2 = exp(logtheta[,4]);
  
  /*
  for(j in 1:nSubjects){
    // CL[j] = exp(logtheta[j, 1]);
    // Q[j] = exp(logtheta[j, 2]);
    // V1[j] = exp(logtheta[j, 3]);
    // V2[j] = exp(logtheta[j, 4]);
  }*/
  
  for(i in 1:nObs){
    cHat[i] = twoCPTM_model_simple(
      timeAdm[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      dose[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      tinf[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      time[i],
      CL[subject[i]], Q[subject[i]], 
      V1[subject[i]], V2[subject[i]] );
  }
}

model{
    CLHat ~ normal(muClHat, sdClHat);
    QHat  ~ normal(muQHat, sdQHat);
    V1Hat ~ normal(muV1Hat, sdV1Hat);
    V2Hat ~ normal(muV2Hat, sdV2Hat);
    omega ~ cauchy(muOmega, sdOmega);
    
    rho ~ lkj_corr(etaRho); 
    sigma ~ cauchy(muSigma, sdSigma);

    // Variabilidad Inter-individual
    logtheta ~ multi_normal(log(thetaHat), Omega);
    // Variabilidad Intra-individual
    logcObs ~ normal(log(cHat), sigma);
}

generated quantities{
  vector[nRandom] logthetaPred[nSubjects];
  real<lower = 0> cHatPred[nObs];
  real<lower = 0> cObsPred[nObs];
  real<lower = 0> cObsCond[nObs];
  real<lower = 0> CLPred[nSubjects];
  real<lower = 0> QPred[nSubjects];
  real<lower = 0> V1Pred[nSubjects];
  real<lower = 0> V2Pred[nSubjects];

  for(j in 1:nSubjects){
    logthetaPred[j] = multi_normal_rng(log(thetaHat), Omega);
    //print(multi_normal_rng(log(thetaHat), Omega));
    CLPred[j] = exp(logthetaPred[j, 1]);
    QPred[j]  = exp(logthetaPred[j, 2]);
    V1Pred[j] = exp(logthetaPred[j, 3]);
    V2Pred[j] = exp(logthetaPred[j, 4]);
  }
  
  
  for(i in 1:nObs){
    cHatPred[i] = twoCPTM_model_simple(
      timeAdm[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      dose[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      tinf[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      time[i],
      CLPred[subject[i]],  QPred[subject[i]], 
      V1Pred[subject[i]], V2Pred[subject[i]] );
      
      cObsCond[i] = exp(normal_rng(log(cHat[i]), sigma)); // Predicciones individuales
      cObsPred[i] = exp(normal_rng(log(cHatPred[i]), sigma)); // Predicciones poblacionales
  }
}


