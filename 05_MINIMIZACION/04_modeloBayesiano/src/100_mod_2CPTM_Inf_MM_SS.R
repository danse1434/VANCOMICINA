require(tidyverse)

setwd('F:/Documentos')
source('./PoliExponencial.R')

twoCPTM_model1 <- function(parameters, initial, rate, dt) {
  
  CL = parameters[1]
  Q  = parameters[2]
  V1 = parameters[3]
  V2 = parameters[4]
  ka = parameters[5]
  
  k10 = CL / V1
  k12 = Q / V1
  k21 = Q / V2
  ksum = k10 + k12 + k21
  
  a = vector(length = 3)
  
  alpha = vector(length = 3)
  alpha[1] = (ksum + sqrt(ksum ^ 2 - 4 * k10 * k21)) / 2
  alpha[2] = (ksum - sqrt(ksum ^ 2 - 4 * k10 * k21)) / 2
  alpha[3] = ka
  
  pred = c(0, 0, 0)
  
  if ((initial[1] != 0) | (rate[1] != 0)) {
    pred[1] = initial[1] * exp(-ka * dt)
    pred[1] = pred[1] + rate[1] * (1 - exp(-ka * dt)) / ka
    
    a[1] = ka * (k21 - alpha[1]) / ((ka - alpha[1]) * (alpha[2] - alpha[1]))
    a[2] = ka * (k21 - alpha[2]) / ((ka - alpha[2]) * (alpha[1] - alpha[2]))
    a[3] = -(a[1] + a[2])
    
    pred[2] = pred[2] +
      PoliExponencial(dt, initial[1], 0, 0, 0, FALSE, a, alpha, 3L) +
      PoliExponencial(dt, 0, rate[1], dt, 0, FALSE, a, alpha, 3L)
    
    a[1] = ka * k12 / ((ka - alpha[1]) * (alpha[2] - alpha[1]))
    a[2] = ka * k12 / ((ka - alpha[2]) * (alpha[1] - alpha[2]))
    a[3] = -(a[1] + a[2])
    
    pred[3] = pred[3] +
      PoliExponencial(dt, initial[1], 0, 0, 0, FALSE, a, alpha, 3L) +
      PoliExponencial(dt, 0, rate[1], dt, 0, FALSE, a, alpha, 3L)
  }
  
  if ((initial[2] != 0.0) | (rate[2] != 0.0)) {
    a[1] = (k21 - alpha[1]) / (alpha[2] - alpha[1])
    a[2] = (k21 - alpha[2]) / (alpha[1] - alpha[2])
    
    pred[2] = pred[2] +
      PoliExponencial(dt, initial[2], 0, 0, 0, FALSE, a, alpha, 2L) +
      PoliExponencial(dt, 0, rate[2], dt, 0, FALSE, a, alpha, 2L)
    
    a[1] = k12 / (alpha[2] - alpha[1])
    a[2] = -a[1]
    
    pred[3] = pred[3] +
      PoliExponencial(dt, initial[2], 0, 0, 0, FALSE, a, alpha, 2L) +
      PoliExponencial(dt, 0, rate[2], dt, 0, FALSE, a, alpha, 2L)
    
  }
  
  if ((initial[3] != 0.0) | (rate[3] != 0.0)) {
    a[1] = k21 / (alpha[2] - alpha[1])
    a[2] = -a[1]
    
    pred[2] = pred[2] +
      PoliExponencial(dt, initial[3], 0, 0, 0, FALSE, a, alpha, 2L) +
      PoliExponencial(dt, 0, rate[3], dt, 0, FALSE, a, alpha, 2L)
    
    a[1] = (k10 + k12 - alpha[1]) / (alpha[2] - alpha[1])
    a[2] = (k10 + k12 - alpha[2]) / (alpha[1] - alpha[2])
    
    pred[3] = pred[3] +
      PoliExponencial(dt, initial[3], 0, 0, 0, FALSE, a, alpha, 2L) +
      PoliExponencial(dt, 0, rate[3], dt, 0, FALSE, a, alpha, 2L)
    
  }
  
  return(pred)
}
