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
  */
  real twoCPTM_model_covariable(real[] time, real[] dose, real[] tinf, real dt, 
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
  
  int nOBS;
  vector[nOBS] OBS;
  vector[nOBS] time;
  
  int nSubjects;
  
  int start[nSubjects]; 
  int end[nSubjects];
  int subject[nOBS];
  
  // Eventos de Dosificación
  int nAdmEv;
  int startAdmEv[nSubjects];
  int endAdmEv[nSubjects];
  
  real timeAdm[nAdmEv];
  real dose[nAdmEv];
  real tinf[nAdmEv];
  
  // Hiperparámetros
  real<lower=0> min_ClHat;
  real<lower=0> max_ClHat;
  real<lower=0> min_QHat;
  real<lower=0> max_QHat;
  real<lower=0> min_V1Hat;
  real<lower=0> max_V1Hat;
  real<lower=0> min_V2Hat;
  real<lower=0> max_V2Hat;
  
  real min_beta_Cl_logtCLCRMLMIN;
  real max_beta_Cl_logtCLCRMLMIN;
  
  /*Parámetros para CL y V1*/
  real mu_Omega_12[2];
  real<lower=0> sd_Omega_12[2];
  
  real mu_Omega_3;
  real<lower=0> sd_Omega_3;
  
  real etaRho_12;
  real<lower=0> mu_b; 
  real<lower=0> sd_b;
  
  // Relación covariables
  real logtCLCRMLMIN[nSubjects];
}

transformed data{
  vector[nOBS] logOBS = log(OBS);
  int nRandom = 3;
  int nRandom_12 = 2;
  int nFixed  = 4;
}

parameters{
  real<lower = 0> CLHat;
  real<lower = 0> QHat;
  real<lower = 0> V1Hat;
  real<lower = 0> V2Hat;
  corr_matrix[nRandom_12] corr_12;
  
  real beta_Cl_logtCLCRMLMIN;
  
  vector<lower = 0>[nRandom_12] omega_12;
  real<lower = 0> omega_3;
  real<lower = 0> b;
  vector[nFixed] logtheta[nSubjects];
}

transformed parameters{
  vector<lower = 0>[nFixed] thetaHat;
  cov_matrix[nRandom_12] Omega_12;
  real<lower = 0> Omega_3;
  
  real<lower = 0> CL[nSubjects];  
  real<lower = 0> Q[nSubjects];
  real<lower = 0> V1[nSubjects];
  real<lower = 0> V2[nSubjects];
  vector<lower = 0>[nOBS] cHat;
  
  // Vector y matrices reales
  
  thetaHat[1] = CLHat;
  thetaHat[2] = V1Hat;
  thetaHat[3] = QHat;
  thetaHat[4] = V2Hat;
  
  Omega_12 = quad_form_diag(corr_12, omega_12);
  Omega_3  = omega_3;
  
  for(i in 1:nSubjects){
    CL[i] = exp(logtheta[i ,1] + beta_Cl_logtCLCRMLMIN * logtCLCRMLMIN[i]);
  }
  V1 = exp(logtheta[,2]);
  Q  = exp(logtheta[,3]);
  
  for(i in 1:nSubjects){
   V2[i] = thetaHat[4];
  }
  
  for(i in 1:nOBS){
    cHat[i] = twoCPTM_model_covariable(
      timeAdm[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      dose[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      tinf[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      time[i],
      CL[subject[i]], Q[subject[i]], 
      V1[subject[i]], V2[subject[i]] );
  }
  
}

model{
    CLHat ~ uniform(min_ClHat, max_ClHat);
    V1Hat ~ uniform(min_V1Hat, max_V1Hat);
    QHat  ~ uniform(min_QHat, max_QHat);
    V2Hat ~ uniform(min_V2Hat, max_V2Hat);
    
    beta_Cl_logtCLCRMLMIN ~ uniform(min_beta_Cl_logtCLCRMLMIN, max_beta_Cl_logtCLCRMLMIN);
    
    omega_12 ~ cauchy(mu_Omega_12, sd_Omega_12);
    corr_12 ~ lkj_corr(etaRho_12);
    omega_3 ~ cauchy(mu_Omega_3, sd_Omega_3);
    
    b ~ cauchy(mu_b, sd_b);
    
    // Variabilidad Inter-individual
    
    logtheta[,1:2] ~ multi_normal(log(thetaHat[1:2]), Omega_12);
    logtheta[,3] ~ normal(log(thetaHat[3]), Omega_3);
    
    // Variabilidad Intra-individual
    // Se cambia a un modelo de error residual de tipo proporcional con distribucion log normal
    for(i in 1:nOBS){
      logOBS[i] ~ normal(log(cHat[i]), sqrt(pow((log(cHat[i]) * b), 2)));
    }
    
}

generated quantities{
  vector[nFixed] logthetaPred[nSubjects];
  real<lower = 0> CLPred[nSubjects];
  real<lower = 0> QPred[nSubjects];
  real<lower = 0> V1Pred[nSubjects];
  real<lower = 0> V2Pred[nSubjects];

  real<lower = 0> cHatPred[nOBS];
  real<lower = 0> cObsPred[nOBS];
  real<lower = 0> cObsCond[nOBS];

  for(j in 1:nSubjects){
    logthetaPred[j,1:2] = multi_normal_rng(log(thetaHat[1:2]), Omega_12);
    logthetaPred[j,3]   = normal_rng(log(thetaHat[3]), Omega_3);
    
    CLPred[j] = exp(logthetaPred[j, 1] + beta_Cl_logtCLCRMLMIN * logtCLCRMLMIN[j]);
    QPred[j]  = exp(logthetaPred[j, 2]);
    V1Pred[j] = exp(logthetaPred[j, 3]);
    V2Pred[j] = thetaHat[4];
  }

  for(i in 1:nOBS){
    cHatPred[i] = twoCPTM_model_covariable(
      timeAdm[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      dose[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      tinf[ startAdmEv[subject[i]]:endAdmEv[subject[i]] ],
      time[i],
      CLPred[subject[i]],  QPred[subject[i]],
      V1Pred[subject[i]], V2Pred[subject[i]]);

      cObsCond[i] = exp(normal_rng(log(cHat[i]),     sqrt(pow(log(cHat[i]),2))));     // Predicciones individuales
      cObsPred[i] = exp(normal_rng(log(cHatPred[i]), sqrt(pow(log(cHatPred[i]),2)))); // Predicciones poblacionales
  }
  
}


