funcion2Cptm <- function(parameters, amt, cmt, dt) {
  stopifnot(length(amt$time)==length(amt$dose))
  
  n_bolus = length(amt$time)
  # amt1 <- list('time' = c(0), 'dose' = c(0), 'tinf' = c(0))
  
  if (!'tinf' %in% names(amt)){
    amt$tinf = rep(0, n_bolus)
  }
  nb <- 0
  for (i in 1:n_bolus) {
    if (amt$time[i]<=dt) {
      # amt1$time[i]=amt$time[i]
      # amt1$dose[i]=amt$dose[i]
      # amt1$tinf[i]=amt$tinf[i]
      nb = nb + 1
    }
  }
  # amt <- amt1
  n_bolus = nb
  # n_bolus = length(amt$time)
  
  CL = parameters[1]
  Q  = parameters[2]
  V1 = parameters[3]
  V2 = parameters[4]
  
  k10 = CL / V1
  k12 = Q / V1
  k21 = Q / V2
  ksum = k10 + k12 + k21
  
  alpha = vector(length = 2)
  alpha[1] = (ksum + sqrt(ksum^2L - 4.0 * k10 * k21)) / 2.0
  alpha[2] = (ksum - sqrt(ksum^2L - 4.0 * k10 * k21)) / 2.0
  
  A = vector(length = 2)
  A[1] = 1/V1 * (alpha[1] - k21) / (alpha[1] - alpha[2])
  A[2] = 1/V1 * (k21 - alpha[2]) / (alpha[1] - alpha[2])
  
  Cp = 0
  # print(paste0('Cp0 <- ', Cp))
  # branch = vector()
  
  if((dt - (amt$time[n_bolus])) < (amt$tinf[n_bolus])){
    
    
    for (i in 1:n_bolus) {
      t0 = amt$time[i]
      t_inf = amt$tinf[i]
      D = amt$dose[i]
      
      if (i==n_bolus){
        Cp = Cp + (D/t_inf) * 
          ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*(dt-t0))) + 
             (A[2]/alpha[2]) * (1 - exp(-alpha[2]*(dt-t0))))
        
        # branch <- append(branch, paste0(i,'B2A'))
        
      } else {
        Cp = Cp + (D/t_inf) * 
          ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*t_inf)) * exp(-alpha[1]*(dt-t0-t_inf)) + 
             (A[2]/alpha[2]) * (1 - exp(-alpha[2]*t_inf)) * exp(-alpha[2]*(dt-t0-t_inf))
          )
        # branch <- append(branch, paste0(i,'B2B'))
        # print(c(i, dt, t0, Cp, D/t_inf, dt-t0-t_inf), digits = 3)
        
      }
    }
  } else {
    for (i in 1:n_bolus) {
      t0 = amt$time[i]
      t_inf = amt$tinf[i]
      D = amt$dose[i]
      
      Cp = Cp + (D/t_inf) * 
        ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*t_inf)) * exp(-alpha[1]*(dt-t0-t_inf)) + 
           (A[2]/alpha[2]) * (1 - exp(-alpha[2]*t_inf)) * exp(-alpha[2]*(dt-t0-t_inf)) )
      # branch <- append(branch, paste0(i,'B3'))
    }
  }
  
  # print(paste0('nb <- ', nb))
  # print(paste0('Cp <- ', Cp))
  return(Cp)
}
