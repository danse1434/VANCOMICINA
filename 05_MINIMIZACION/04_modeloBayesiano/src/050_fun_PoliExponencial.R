#' Función PoliExponencial
#'
#' @param x 
#' @param dose 
#' @param rate 
#' @param xinf 
#' @param tau 
#' @param ss 
#' @param a 
#' @param alpha 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
PoliExponencial <- function(x, dose, rate, xinf, tau, ss, a, alpha, n) {
  stopifnot(is.numeric(x) & is.numeric(dose) & is.numeric(rate) & is.numeric(tau))
  stopifnot(is.logical(ss))
  stopifnot(is.vector(a) & is.vector(alpha));
  stopifnot(is.integer(n))
  
  stopifnot(length(alpha)>=n & length(a)>=n);
  
  
  nIntv = 0
  result = 0;
  dx = 0
  
  
  if (dose > 0) {
    # Dosis en Bolos
    if (tau <= 0) {
      if (x >= 0) {
        for (i in 1:n) {
          result = result + a[i] * exp(-alpha[i] * x)
        }
      }
    }
    else if (!ss) {
      nIntv = round(x / tau) + 1
      
      dx = x - round(x / tau) * tau
      
      for (i in 1:n) {
        result = result + a[i] * exp(-alpha[i] * x) *
          (1 - exp(-nIntv * alpha[i] * tau)) / (1 - exp(-alpha[i] * tau))
      }
    }
    else{
      dx = x - round(x / tau) * tau
      
      for (i in 1:n) {
        result =
          result + a[i] * exp(-alpha[i] * x) / (1 - exp(-alpha[i] * tau))
      }
    }
  }
  
  bolus_result = dose * result
  result = 0
  
  
  if ((rate > 0) & (xinf < Inf)) {
    # Infusión truncada
    if (tau <= 0) {
      if (x >= 0) {
        if (x <= xinf) {
          for (i in 1:n) {
            result = result + a[i] * (1 - exp(-alpha[i] * x)) / alpha[i]
          }
        }
        else{
          for (i in 1:n) {
            result =
              result + a[i] * (1 - exp(-alpha[i] * xinf)) * exp(-alpha[i] * (x - xinf)) / alpha[i]
          }
        }
      }
    }
    else if (!ss) {
      # ASSERT(xinf <= tau, 20); 
      # Se debe adicionar otro caso después
      dx = x - round(x / tau) * tau
      nIntv = round(x / tau) + 1
      
      if (dx <= xinf) {
        for (i in 1:n) {
          if (n > 1) {
            result =
              result + a[i] * (1 - exp(-alpha[i] * xinf)) * exp(-alpha[i] * (dx - xinf +
                                                                               tau)) *
              (1 - exp(-(nIntv - 1) * alpha[i] * tau)) / (1 - exp(-alpha[i] *
                                                                    tau)) / alpha[i]
          }
          result =
            result + a[i] * (1 - exp(-alpha[i] * dx)) / alpha[i]
        }
      }
      else{
        for (i in 1:n) {
          result =
            result + a[i] * (1 - exp(-alpha[i] * xinf)) * exp(-alpha[i] * (dx - xinf)) *
            (1 - exp(-nIntv * alpha[i] * tau)) / (1 - exp(-alpha[i] * tau)) / alpha[i]
        }
      }
    }
    else{
      # ASSERT(xinf <= tau, 20);
      # Se debe adicionar otro caso después
      dx = x - round(x / tau) * tau
      nIntv = round(x / tau) + 1
      if (dx <= xinf) {
        for (i in 1:n) {
          result =
            result + a[i] * (1 - exp(-alpha[i] * xinf)) * exp(-alpha[i] * (dx - xinf +
                                                                             tau)) /
            (1 - exp(-alpha[i] * tau)) / alpha[i] + a[i] * (1 - exp(-alpha[i] *
                                                                      dx)) / alpha[i]
        }
      }
      else{
        for (i in 1:n) {
          result =
            result + a[i] * (1 - exp(-alpha[i] * xinf)) * exp(-alpha[i] * (dx - xinf)) /
            (1 - exp(-alpha[i] * tau)) / alpha[i]
        }
      }
    }
  }
  
  else{
    # Infusión contina (xinf = Inf). Se ignora tau
    if (!ss) {
      if (x >= 0) {
        for (i in 1:n) {
          result = result + a[i] * (1 - exp(-alpha[i] * x)) / alpha[i]
        }
      }
      else{
        for (i in 1:n) {
          result = result + a[i] / alpha[i]
        }
      }
    }
  }
  
  return(bolus_result + rate*result)
}

