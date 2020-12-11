require(tidyverse)
source('src/050_fun_PoliExponencial.R')

#' Modelo de Dos Compartimentos Estado Estacionario
#'
#' @param parameters 
#' @param amt 
#' @param rate 
#' @param ii 
#' @param cmt 
#' @param dt 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
twoCPTM_model2 <- function(parameters, amt, rate, ii, cmt, dt) {
  CL = parameters[1]
  Q = parameters[2]
  V1 = parameters[3]
  V2 = parameters[4]
  ka = parameters[5]
  
  # ASSERT((CL > 0) & (Q > 0) & (V1 > 0) & (V2 > 0) & (ka > 0) , 20);
  k10 = CL / V1
  k12 = Q / V1
  k21 = Q / V2
  ksum = k10 + k12 + k21
  
  a = vector(length = 3)
  
  alpha = vector(length = 3)
  
  alpha[1] = (ksum + sqrt(ksum * ksum - 4.0 * k10 * k21)) / 2.0
  alpha[2] = (ksum - sqrt(ksum * ksum - 4.0 * k10 * k21)) / 2.0
  alpha[3] = ka
  
  pred = c(0,0,0)
  pred[1] = 0
  pred[2] = 0
  pred[3] = 0
  
  
  if (rate == 0) { # bolus dose *)
    
    if (cmt == 1) { # Compartimento 1
      a[1] = 0.0
      a[2] = 0.0
      a[3] = 1.0
      pred[1] = PoliExponencial(dt, amt, 0, 0, ii, TRUE, a, alpha, 3L)
      
      a[1] = ka * (k21 - alpha[1]) / ((ka - alpha[1]) * (alpha[2] - alpha[1]))
      a[2] = ka * (k21 - alpha[2]) / ((ka - alpha[2]) * (alpha[1] - alpha[2]))
      a[3] = -(a[1] + a[2])
      pred[2] = PoliExponencial(dt, amt, 0, 0, ii, TRUE, a, alpha, 3L)
      
      a[1] = ka * k12 / ((ka - alpha[1]) * (alpha[2] - alpha[1]))
      a[2] = ka * k12 / ((ka - alpha[2]) * (alpha[1] - alpha[2]))
      a[3] = -(a[1] + a[2])
      pred[3] = PoliExponencial(dt, amt, 0, 0, ii, TRUE, a, alpha, 3L)
    }
    else if (cmt == 2) {# Compartimento 2
      a[1] = (k21 - alpha[1]) / (alpha[2] - alpha[1])
      a[2] = (k21 - alpha[2]) / (alpha[1] - alpha[2])
      pred[2] = PoliExponencial(dt, amt, 0, 0, ii, TRUE, a, alpha, 2L)
      
      a[1] = k12 / (alpha[2] - alpha[1])
      a[2] = -a[1]
      pred[3] = PoliExponencial(dt, amt, 0, 0, ii, TRUE, a, alpha, 2L)
      
    }
    else{ # Compartimento 3
      a[1] = k21 / (alpha[2] - alpha[1])
      a[2] = -a[1]
      pred[2] = PoliExponencial(dt, amt, 0, 0, ii, TRUE, a, alpha, 2L)
      
      a[1] = (k10 + k12 - alpha[1]) / (alpha[2] - alpha[1])
      a[2] = (k10 + k12 - alpha[2]) / (alpha[1] - alpha[2])
      pred[3] = PoliExponencial(dt, amt, 0, 0, ii, TRUE, a, alpha, 2L)
    }
  }
  
  else if (ii > 0) { # Infusiones múltiples truncadas
    
    if (cmt == 1) {
      a[1] = 0.0
      a[2] = 0.0
      a[3] = 1.0
      
      pred[1] = PoliExponencial(dt, 0, rate, amt / rate, ii, TRUE, a, alpha, 3L)
      
      a[1] = ka * (k21 - alpha[1]) / ((ka - alpha[1]) * (alpha[2] - alpha[1]))
      a[2] = ka * (k21 - alpha[2]) / ((ka - alpha[2]) * (alpha[1] - alpha[2]))
      a[3] = -(a[1] + a[2])
      
      pred[2] = PoliExponencial(dt, 0, rate, amt / rate, ii, TRUE, a, alpha, 3L)
      
      a[1] = ka * k12 / ((ka - alpha[1]) * (alpha[2] - alpha[1]))
      a[2] = ka * k12 / ((ka - alpha[2]) * (alpha[1] - alpha[2]))
      a[3] = -(a[1] + a[2])
      
      pred[3] = PoliExponencial(dt, 0, rate, amt / rate, ii, TRUE, a, alpha, 3L)
    }
    else if (cmt == 2) {
      a[1] = (k21 - alpha[1]) / (alpha[2] - alpha[1])
      a[2] = (k21 - alpha[2]) / (alpha[1] - alpha[2])
      
      pred[2] = PoliExponencial(dt, 0, rate, amt / rate, ii, TRUE, a, alpha, 2L)
      
      a[1] = k12 / (alpha[2] - alpha[1])
      a[2] = -a[1]
      
      pred[3] = PoliExponencial(dt, 0, rate, amt / rate, ii, TRUE, a, alpha, 2L)
    }
    else{ # cmt = 3 *)
      a[1] = k21 / (alpha[2] - alpha[1])
      a[2] = -a[1]
      
      pred[2] = PoliExponencial(dt, 0, rate, amt / rate, ii, TRUE, a, alpha, 2L)
      
      a[1] = (k10 + k12 - alpha[1]) / (alpha[2] - alpha[1])
      a[2] = (k10 + k12 - alpha[2]) / (alpha[1] - alpha[2])
      
      pred[3] = PoliExponencial(dt, 0, rate, amt / rate, ii, TRUE, a, alpha, 2L)
      
    }
  }
  
  else{ # Infusión constante
    if (cmt == 1) { # Compartimento 1
      a[1] = 0.0
      a[2] = 0.0
      a[3] = 1.0
      
      pred[1] = PoliExponencial(dt, 0, rate, Inf, 0, TRUE, a, alpha, 3L)
      
      a[1] = ka * (k21 - alpha[1]) / ((ka - alpha[1]) * (alpha[2] - alpha[1]))
      a[2] = ka * (k21 - alpha[2]) / ((ka - alpha[2]) * (alpha[1] - alpha[2]))
      a[3] = -(a[1] + a[2])
      
      pred[2] = PoliExponencial(dt, 0, rate, Inf, 0, TRUE, a, alpha, 3L)
      
      a[1] = ka * k12 / ((ka - alpha[1]) * (alpha[2] - alpha[1]))
      a[2] = ka * k12 / ((ka - alpha[2]) * (alpha[1] - alpha[2]))
      a[3] = -(a[1] + a[2])
      
      pred[3] = PoliExponencial(dt, 0, rate, Inf, 0, TRUE, a, alpha, 3L)
      
    }
    else if (cmt == 2) { # Compartimento 2
      a[1] = (k21 - alpha[1]) / (alpha[2] - alpha[1])
      a[2] = (k21 - alpha[2]) / (alpha[1] - alpha[2])
      
      pred[2] = PoliExponencial(dt, 0, rate, Inf, 0, TRUE, a, alpha, 2L)
      
      a[1] = k12 / (alpha[2] - alpha[1])
      a[2] = -a[1]
      
      pred[3] = PoliExponencial(dt, 0, rate, Inf, 0, TRUE, a, alpha, 2L)
    }
    else{ # Compartimento 3
      a[1] = k21 / (alpha[2] - alpha[1])
      a[2] = -a[1]
      
      pred[2] = PoliExponencial(dt, 0, rate, Inf, 0, TRUE, a, alpha, 2L)
      
      a[1] = (k10 + k12 - alpha[1]) / (alpha[2] - alpha[1])
      a[2] = (k10 + k12 - alpha[2]) / (alpha[1] - alpha[2])
      
      pred[3] = PoliExponencial(dt, 0, rate, Inf, 0, TRUE, a, alpha, 2L)
      
    }
  }
  return(pred)
}