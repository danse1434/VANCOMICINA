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
  int nObs1;
  vector[nObs1] cObs1;
  vector[nObs1] time1;
  
  int nObs2;
  vector[nObs2] cObs2;
  vector[nObs2] time2;
  
  int nSubjects1;
  int nSubjects2;
  /** En este caso permite definir donde empieza y 
      terminan los datos para cada ID */
  int start1[nSubjects1]; 
  int end1[nSubjects1];
  int subject1[nObs1];
  
  int start2[nSubjects2]; 
  int end2[nSubjects2];
  int subject2[nObs2];
  
  // Eventos de Dosificación
  int nAdmEv;
  int startAdmEv[nSubjects1];
  int endAdmEv[nSubjects1];
  
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
  
  real muOmega[4];
  real<lower=0> sdOmega[4];
  real etaRho;
  real<lower=0> mua1; 
  real<lower=0> sda1;
  real<lower=0> mua2; 
  real<lower=0> sda2;
  
  // Relación covariables
  real logtCLCRMLMIN[nSubjects1];
}

transformed data{
  vector[nObs1] logcObs1 = log(cObs1);
  vector[nObs2] logcObs2 = log(cObs2);
  int nRandom = 4;
}

parameters{
  real<lower = 0> CLHat;
  real<lower = 0> QHat;
  real<lower = 0> V1Hat;
  real<lower = 0> V2Hat;
  corr_matrix[nRandom] rho;
  
  real beta_Cl_logtCLCRMLMIN;
  
  vector<lower = 0>[nRandom] omega;
  real<lower = 0> a1;
  real<lower = 0> a2;
  vector[nRandom] logtheta[nSubjects1];
}

transformed parameters{
  vector<lower = 0>[nRandom] thetaHat;
  cov_matrix[nRandom] Omega;
  
  real<lower = 0> CL[nSubjects1];  
  real<lower = 0> Q[nSubjects1];
  real<lower = 0> V1[nSubjects1];
  real<lower = 0> V2[nSubjects1];
  vector<lower = 0>[nObs1] cHat1;
  vector<lower = 0>[nObs2] cHat2;
  
  matrix[nRandom, nRandom] rho1;
  
  thetaHat[1] = CLHat;
  thetaHat[2] = QHat;
  thetaHat[3] = V1Hat;
  thetaHat[4] = V2Hat;
  
  rho1 = rho;
  rho1[1,2] = 0;
  rho1[1,3] = 0;
  rho1[1,4] = 0;
  rho1[2,1] = 0;
  rho1[2,3] = 0;
  rho1[2,4] = 0;

  rho1[3,1] = 0;
  rho1[3,2] = 0;
  rho1[4,1] = 0;
  rho1[4,2] = 0;
  
  Omega = quad_form_diag(rho1, omega);
  
  for(i in 1:nSubjects1){
    CL[i] = exp(logtheta[i ,1] + beta_Cl_logtCLCRMLMIN * logtCLCRMLMIN[i]);
    Q[i]  = exp(logtheta[i,2]);
    V1[i] = exp(logtheta[i,3]);
    V2[i] = exp(logtheta[i,4]);
  }
  
  for(i in 1:nObs1){
    cHat1[i] = twoCPTM_model_covariable(
      timeAdm[ startAdmEv[subject1[i]]:endAdmEv[subject1[i]] ],
      dose[ startAdmEv[subject1[i]]:endAdmEv[subject1[i]] ],
      tinf[ startAdmEv[subject1[i]]:endAdmEv[subject1[i]] ],
      time1[i],
      CL[subject1[i]], Q[subject1[i]], 
      V1[subject1[i]], V2[subject1[i]] );
  }
  
  for(i in 1:nObs2){
    cHat2[i] = twoCPTM_model_covariable(
      timeAdm[ startAdmEv[subject2[i]]:endAdmEv[subject2[i]] ],
      dose[ startAdmEv[subject2[i]]:endAdmEv[subject2[i]] ],
      tinf[ startAdmEv[subject2[i]]:endAdmEv[subject2[i]] ],
      time2[i],
      CL[subject2[i]], Q[subject2[i]], 
      V1[subject2[i]], V2[subject2[i]] );
  }
}

model{
    CLHat ~ uniform(min_ClHat, max_ClHat);
    QHat  ~ uniform(min_QHat, max_QHat);
    V1Hat ~ uniform(min_V1Hat, max_V1Hat);
    V2Hat ~ uniform(min_V2Hat, max_V2Hat);
    
    beta_Cl_logtCLCRMLMIN ~ uniform(min_beta_Cl_logtCLCRMLMIN, max_beta_Cl_logtCLCRMLMIN);
    
    omega ~ cauchy(muOmega, sdOmega);
    rho ~ lkj_corr(etaRho);
    
    a1 ~ cauchy(mua1, sda1);
    a2 ~ cauchy(mua2, sda2);

    // Variabilidad Inter-individual
    logtheta ~ multi_normal(log(thetaHat), Omega);
    // Variabilidad Intra-individual
    // Se cambia a un modelo de error Residual con distribucion Normal
    logcObs1 ~ normal(log(cHat1), a1);
    logcObs2 ~ normal(log(cHat2), a2);
}

generated quantities{
  vector[nRandom] logthetaPred[nSubjects1];
  real<lower = 0> CLPred[nSubjects1];
  real<lower = 0> QPred[nSubjects1];
  real<lower = 0> V1Pred[nSubjects1];
  real<lower = 0> V2Pred[nSubjects1];
  
  real<lower = 0> cHatPred1[nObs1];
  real<lower = 0> cObsPred1[nObs1];
  real<lower = 0> cObsCond1[nObs1];
  real<lower = 0> cHatPred2[nObs2];
  real<lower = 0> cObsPred2[nObs2];
  real<lower = 0> cObsCond2[nObs2];

  for(j in 1:nSubjects1){
    logthetaPred[j] = multi_normal_rng(log(thetaHat), Omega);
    //print(multi_normal_rng(log(thetaHat), Omega));
    CLPred[j] = exp(logthetaPred[j, 1] + beta_Cl_logtCLCRMLMIN * logtCLCRMLMIN[j]);
    QPred[j]  = exp(logthetaPred[j, 2]);
    V1Pred[j] = exp(logthetaPred[j, 3]);
    V2Pred[j] = exp(logthetaPred[j, 4]);
  }
  
  for(i in 1:nObs1){
    cHatPred1[i] = twoCPTM_model_covariable(
      timeAdm[ startAdmEv[subject1[i]]:endAdmEv[subject1[i]] ],
      dose[ startAdmEv[subject1[i]]:endAdmEv[subject1[i]] ],
      tinf[ startAdmEv[subject1[i]]:endAdmEv[subject1[i]] ],
      time1[i],
      CLPred[subject1[i]],  QPred[subject1[i]], 
      V1Pred[subject1[i]], V2Pred[subject1[i]]);
      
      cObsCond1[i] = exp(normal_rng(log(cHat1[i]),     fabs(a1))); // Predicciones individuales
      cObsPred1[i] = exp(normal_rng(log(cHatPred1[i]), fabs(a2))); // Predicciones poblacionales
  }
  
  for(i in 1:nObs2){
    cHatPred2[i] = twoCPTM_model_covariable(
      timeAdm[ startAdmEv[subject2[i]]:endAdmEv[subject2[i]] ],
      dose[ startAdmEv[subject2[i]]:endAdmEv[subject2[i]] ],
      tinf[ startAdmEv[subject2[i]]:endAdmEv[subject2[i]] ],
      time2[i],
      CLPred[subject2[i]],  QPred[subject2[i]], 
      V1Pred[subject2[i]], V2Pred[subject2[i]]);
      
      cObsCond2[i] = exp(normal_rng(log(cHat2[i]),     fabs(a2))); // Predicciones individuales
      cObsPred2[i] = exp(normal_rng(log(cHatPred2[i]), fabs(a2))); // Predicciones poblacionales
  }
  
}


