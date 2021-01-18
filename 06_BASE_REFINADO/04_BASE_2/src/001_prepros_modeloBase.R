require(tidyverse)
require(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(20201209)


#-------------------------------------------------------------------------------#
# 1. Preprocesamiento de datos ----------
#-------------------------------------------------------------------------------#
data <- read_csv('data/data_TAD.csv')

# > 1.1. Eventos de administración
nb_doses <- 5 # Dosis adicionales

dataDosificacion <- data %>% 
  filter(EVID==1) %>% 
  mutate(ADDL = 5) %>% 
  rowwise() %>% 
  do(as.data.frame(.)[rep(1, .$ADDL),]) %>% 
  group_by(ID) %>%
  mutate(TAD = TAD + c(0:(ADDL[1]-1))*as.numeric(as.character(II)),
         ADDL = 0,
         II = 0) %>% 
  arrange(ID, TAD)


dataDosificacion

# > 1.2. Eventos de observación
seleccionUltimaDosis <- function(x, data) {
  data %>%              # Datos
    slice_tail() %>%    # Seleccionar última dosis
    select(ID, TAD)%>%  # Seleccionar ID y sujeto
    filter(ID == x) %>% # Filtrar por sujeto 
    `$`(TAD)            # Seleccionar TAD
}


dataObservacion <- data %>% 
  filter(EVID==0) %>% 
  mutate(
    TAD = map2_dbl(ID, TAD, ~(seleccionUltimaDosis(.x, dataDosificacion) + .y))
    )


dataObservacion

#-------------------------------------------------------------------------------#
# 2. Creación de inputs modelo Bayesiano -------------------
#-------------------------------------------------------------------------------#
eventoObservacion <- dataObservacion %>% 
  filter(YTYPE==1) %>% list(
  nObs      = dim(.)[1],
  cObs      = as.numeric(.$DV),
  time      = as.numeric(.$TAD),
  nSubjects = length(unique(.$ID)),
  
  start     = group_by(., ID) %>% 
    rownames_to_column() %>% 
    slice_head() %>% 
    `$`(rowname) %>% 
    as.integer(),
  
  end       = group_by(., ID) %>% 
    rownames_to_column() %>% 
    slice_tail() %>% 
    `$`(rowname) %>% 
    as.integer(),
  
  subject   = as.integer(.$ID)
)

eventoDosificacion <- list(
  nAdmEv     = dim(dataDosificacion)[1],
  startAdmEv = group_by(dataDosificacion, ID) %>%
    rownames_to_column() %>%
    slice_head() %>%
    `$`(rowname) %>%
    as.integer(),
  endAdmEv   = group_by(dataDosificacion, ID) %>%
    rownames_to_column() %>%
    slice_tail() %>%
    `$`(rowname) %>%
    as.integer(),
  timeAdm    = dataDosificacion$TAD,
  dose       = as.numeric(as.character(dataDosificacion$AMT)),
  tinf       = as.numeric(as.character(dataDosificacion$TINF))
)

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
