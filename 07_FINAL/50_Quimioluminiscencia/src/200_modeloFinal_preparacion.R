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
eventoObservacion2 <- dataObservacion %>%
  filter(YTYPE == 2) %>%
  list(
    nOBS       = dim(.)[1],
    OBS        = as.numeric(.$DV),
    time       = as.numeric(.$TAD),
    nSubjects  = length(unique(.$ID)),
    
    start      = group_by(., ID) %>%
      rownames_to_column() %>%
      slice_head() %>%
      `$`(rowname) %>%
      as.integer(),
    
    end        = group_by(., ID) %>%
      rownames_to_column() %>%
      slice_tail() %>%
      `$`(rowname) %>%
      as.integer(),
    
    subject    = as.integer(.$ID)
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
  logtWTKG   = filter(data, EVID == 1) %>% pull(WTKG) %>% {log(./62)},
  logtCLCRMLMIN = filter(data, EVID == 1) %>% pull(CLCRMLMIN) %>% {log(./144)}
)

hiperparametros <- list(
  min_ClHat = 0,
  max_ClHat = 50,
  min_QHat  = 0,
  max_QHat  = 50,
  min_V1Hat = 0,
  max_V1Hat = 150,
  min_V2Hat = 0,
  max_V2Hat = 150,
  
  min_beta_Cl_logtCLCRMLMIN = -1.0,
  max_beta_Cl_logtCLCRMLMIN = +2.0,
  
  mu_Omega_12 = rep(0, 2),
  sd_Omega_12 = rep(1, 2),
  
  mu_Omega_3 = 0,
  sd_Omega_3 = 1,
  
  etaRho_12 = 1,
  mu_b = 0.5,
  sd_b = 1.0
)

# Lista general con parámetros
stan_d <- list()
stan_d <- append(stan_d, discard(eventoObservacion2, is.data.frame))
stan_d <- append(stan_d, eventoDosificacion)
stan_d <- append(stan_d, hiperparametros)
stan_d

# Función de inicio de parámetros
init <- function() {
  list(
    CLHat = runif(1, hiperparametros$min_ClHat, hiperparametros$max_ClHat),
    V1Hat = runif(1, hiperparametros$min_V1Hat, hiperparametros$max_V1Hat),
    QHat  = runif(1, hiperparametros$min_QHat, hiperparametros$max_QHat),
    V2Hat = runif(1, hiperparametros$min_V2Hat, hiperparametros$max_V2Hat),
    
    beta_Cl_logtCLCRMLMIN = runif(1,
                               hiperparametros$min_beta_Cl_logtCLCRMLMIN,
                               hiperparametros$max_beta_Cl_logtCLCRMLMIN),
    
    omega_12 = exp(rnorm(2, log(0.2), 0.5)),
    omega_3  = exp(rnorm(1, log(0.2), 0.5)),
    
    b = runif(1, 0.5, 2),
    
    logtheta = matrix(
      rep(log(c(10, 10, 45, 50)),
          ea = eventoObservacion2$nSubjects),
      nrow = eventoObservacion2$nSubjects
    ),
    cHat = rep(0.1, eventoObservacion2$nOBS)
  )
  
}
