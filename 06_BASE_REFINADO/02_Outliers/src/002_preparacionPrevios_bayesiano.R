#-------------------------------------------------------------------------------#
# 1. Creación de inputs modelo Bayesiano -------------------
#-------------------------------------------------------------------------------#

hiperparametros <- list(
  muClHat = 10.0,
  sdClHat = 3,
  muQHat  = 10.0,
  sdQHat  = 3,
  muV1Hat = 45,
  sdV1Hat = 3,
  muV2Hat = 50,
  sdV2Hat = 3,
  
  muOmega = rep(0, 4),
  sdOmega = rep(1, 4),
  mub = 0,
  sdb = 1
)


# Lista general con parámetros
stan_d <- list()
stan_d <- append(stan_d, discard(eventoObservacion, is.data.frame))
stan_d <- append(stan_d, eventoDosificacion)
stan_d <- append(stan_d, hiperparametros)
stan_d

# Inicializador
init <- function(){
  list(CLHat = exp(rnorm(1, log(hiperparametros$muClHat), 0.2)),
       QHat  = exp(rnorm(1, log(hiperparametros$muQHat), 0.2)),
       V1Hat = exp(rnorm(1, log(hiperparametros$muV1Hat), 0.2)),
       V2Hat = exp(rnorm(1, log(hiperparametros$muV2Hat), 0.2)),
       
       omega = exp(rnorm(4, log(0.2), 0.5)),
       sigma = runif(1, 0.5, 2),
       
       logtheta = matrix(rep(log(c(10,10,45,50)), 
                             ea = eventoObservacion$nSubjects), 
                         nrow = eventoObservacion$nSubjects),
       cHat = rep(0.1, eventoObservacion$nObs))
}
