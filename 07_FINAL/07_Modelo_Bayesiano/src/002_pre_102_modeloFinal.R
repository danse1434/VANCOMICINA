require(tidyverse)
require(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(20201209)


#-------------------------------------------------------------------------------#
# 1. Preprocesamiento de datos ----------
#-------------------------------------------------------------------------------#
data <- read_csv(file.path('data', 'data_TAD.csv'), na = '.', 
                 locale = locale(encoding = 'UTF-8'))

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
eventoObservacion1 <- dataObservacion %>%
  filter(YTYPE == 1) %>%
  list(
    nObs1      = dim(.)[1],
    cObs1      = as.numeric(.$DV),
    time1      = as.numeric(.$TAD),
    nSubjects1 = length(unique(.$ID)),
    
    start1     = group_by(., ID) %>%
      rownames_to_column() %>%
      slice_head() %>%
      `$`(rowname) %>%
      as.integer(),
    
    end1       = group_by(., ID) %>%
      rownames_to_column() %>%
      slice_tail() %>%
      `$`(rowname) %>%
      as.integer(),
    
    subject1   = as.integer(.$ID)
  )

eventoObservacion2 <- dataObservacion %>%
  filter(YTYPE == 2) %>%
  list(
    nObs2      = dim(.)[1],
    cObs2      = as.numeric(.$DV),
    time2      = as.numeric(.$TAD),
    nSubjects2 = length(unique(.$ID)),
    
    start2     = group_by(., ID) %>%
      rownames_to_column() %>%
      slice_head() %>%
      `$`(rowname) %>%
      as.integer(),
    
    end2       = group_by(., ID) %>%
      rownames_to_column() %>%
      slice_tail() %>%
      `$`(rowname) %>%
      as.integer(),
    
    subject2   = as.integer(.$ID)
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
  tinf       = as.numeric(as.character(dataDosificacion$TINF)),
  logtWTKG  = filter(data, EVID == 1) %>% pull(WTKG) %>% {log(./62)},
  tCLCRMLMIN = filter(data, EVID == 1) %>% pull(CLCRMLMIN) %>% {./144}
)

hiperparametros <- list(
  min_ClHat = 0,
  max_ClHat = 50,
  min_QHat  = 0,
  max_QHat  = 50,
  min_V1Hat = 0,
  max_V1Hat = 250,
  min_V2Hat = 0,
  max_V2Hat = 250,
  
  min_beta_Cl_logtWTKG = -1.0, 
  max_beta_Cl_logtWTKG = +2.5,
  
  min_beta_Cl_tCLCRMLMIN = -1.0,
  max_beta_Cl_tCLCRMLMIN = +2.0,
  
  muOmega = rep(0, 4),
  sdOmega = rep(1, 4),
  etaRho = 2,
  mua1 = 0,
  sda1 = 1,
  mua2 = 0,
  sda2 = 1
)

# Lista general con parámetros
stan_d <- list()
stan_d <- append(stan_d, discard(eventoObservacion1, is.data.frame))
stan_d <- append(stan_d, discard(eventoObservacion2, is.data.frame))
stan_d <- append(stan_d, eventoDosificacion)
stan_d <- append(stan_d, hiperparametros)
stan_d

# Inicializador
init <- function() {
  list(
    CLHat = runif(1, hiperparametros$min_ClHat, hiperparametros$max_ClHat),
    QHat  = runif(1, hiperparametros$min_QHat, hiperparametros$max_QHat),
    V1Hat = runif(1, hiperparametros$min_V1Hat, hiperparametros$max_V1Hat),
    V2Hat = runif(1, hiperparametros$min_V2Hat, hiperparametros$max_V2Hat),
    
    beta_Cl_logtWTKG = runif(1,
                             hiperparametros$min_beta_Cl_logtWTKG,
                             hiperparametros$max_beta_Cl_logtWTKG),
    beta_Cl_tCLCRMLMIN = runif(1,
                               hiperparametros$min_beta_Cl_tCLCRMLMIN,
                               hiperparametros$max_beta_Cl_tCLCRMLMIN),
    
    omega = exp(rnorm(4, log(0.2), 0.5)),
    sigma = runif(1, 0.5, 2),
    
    logtheta = matrix(
      rep(log(c(10, 10, 45, 50)),
          ea = eventoObservacion1$nSubjects),
      nrow = eventoObservacion1$nSubjects
    ),
    cHat1 = rep(0.1, eventoObservacion1$nObs),
    cHat2 = rep(0.1, eventoObservacion1$nObs)
  )
  
}
