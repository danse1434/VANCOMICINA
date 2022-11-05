/**
 Simulación
 Modelo de dos compartimentos con infusión y multiDosis
 Efecto de CLCR en CL
 IIV en Cl, V1, Q, con correlación entre Cl y V1
 RSV proporcional
*/

functions{
  
  /**
  * Modelo de dos compartimentos con capacidad de recibir historial de 
  * dosificación.
  * 
  * @param time vector con tiempos de dosificación (en orden)
  * @param doseAdmEv vector de dosis (en orden) 
  * @param tinf vector de t. de infusión (en orden)
  * @param dt tiempo
  * @param CL Clearance
  * @param Q Clearance intercompartimental
  * @param V1 Volumen de distribución
  * @param V2 Volumen periférico
  */
  real twoCPTM_model_covariable(real[] time, real[] doseAdmEv, real[] tinf, real dt, 
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
        D = doseAdmEv[i];
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
        D = doseAdmEv[i];
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
  int nobsSim;
  vector[nobsSim] time;
  
  int nSubjects;
  
  int start[nSubjects]; 
  int end[nSubjects];
  int subjectID[nobsSim];
  
  // Eventos de Dosificación
  int nAdmEv;
  int startAdmEv[nSubjects];
  int endAdmEv[nSubjects];
  
  real timeAdmEv[nAdmEv];
  real doseAdmEv[nAdmEv];
  real tinfAdmEv[nAdmEv];
  
  // Relación covariables
  real logtCLCR[nSubjects];
  
  // Modelo entrenado
  int nSimulaciones;
  
  int nRandom;
  int nFixed;
  
  
  
  
  
  // Caso especial para Omega de Q (este parámetro no tiene correlación con el resto en IIV)
  real mu_Omega_Q;
  real<lower=0> sd_Omega_Q;
  
  real<lower=0> mu_b; 
  real<lower=0> sd_b;
  
  
  // Parámetros simulados
  vector<lower = 0>[nFixed] thetaHat;
  
  matrix[nRandom, nRandom] Omega;
  
  real<lower = 0> omega_Q;
  real beta_Cl_logtCLCR;
  
  real<lower = 0> b;
  
  vector<lower = 0>[nobsSim] cHat;
}

transformed data{
}

parameters{
}

transformed parameters{
}


generated quantities{
  vector[nFixed-2] logthetaPred[nSubjects];
  real logthetaQPred[nSubjects];
  real<lower = 0> CLPred[nSubjects];
  real<lower = 0> V1Pred[nSubjects];
  real<lower = 0> QPred[nSubjects];
  real<lower = 0> V2Pred[nSubjects];

  real<lower = 0> cHatPred[nobsSim];
  real<lower = 0> cObsPred[nobsSim];
  real<lower = 0> cObsCond[nobsSim];

  for(j in 1:nSubjects){
    logthetaPred[j,] = multi_normal_rng(log( segment(thetaHat, 1, 2) ), Omega);
    
    logthetaQPred[j] = normal_rng(log(thetaHat[3]), omega_Q);
  }
  
  CLPred = to_array_1d(exp(to_vector(logthetaPred[, 1]) + (to_vector(logtCLCR) * beta_Cl_logtCLCR)));
  V1Pred = exp(logthetaPred[, 2]);
  QPred  = exp(logthetaQPred);
  V2Pred = rep_array(thetaHat[4], nSubjects);
  
  for(i in 1:nobsSim){
    cHatPred[i] = twoCPTM_model_covariable(
      timeAdmEv[ startAdmEv[subjectID[i]]:endAdmEv[subjectID[i]] ],
      doseAdmEv[ startAdmEv[subjectID[i]]:endAdmEv[subjectID[i]] ],
      tinfAdmEv[ startAdmEv[subjectID[i]]:endAdmEv[subjectID[i]] ],
      time[i],
      CLPred[subjectID[i]],  QPred[subjectID[i]],
      V1Pred[subjectID[i]], V2Pred[subjectID[i]]);

      cObsCond[i] = exp(normal_rng(log(cHat[i]),     fabs(b * log(cHat[i]))));     // Predicciones individuales
      cObsPred[i] = exp(normal_rng(log(cHatPred[i]), fabs(b * log(cHatPred[i])))); // Predicciones poblacionales
  }
  
}


