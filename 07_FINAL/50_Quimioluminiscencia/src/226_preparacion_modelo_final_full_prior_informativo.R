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
  logtCLCR = filter(data, EVID == 1) %>% pull(CLCRMLMIN) %>% {log(./144)}
)

#' 
#' Parámetros basados en run200 de esquema de modelamiento
#' 
hiperparametros <- list(
  mu_ClHat = 5.2,
  sd_ClHat = 0.31,
  mu_QHat  = 4.99,
  sd_QHat  = 1.08,
  mu_V1Hat = 21.22,
  sd_V1Hat = 1.55,
  mu_V2Hat = 28.28,
  sd_V2Hat = 3.82,
  
  mu_beta_Cl_logtCLCR = 0.62,
  sd_beta_Cl_logtCLCR = 0.22,
  
  mu_Omega = c(0.2205951,  0.2205901,  0.6787494, 0.2205901),
  sd_Omega = c(0.02768673, 0.03685763, 0.1221565, 0.03685763),
  
  # mu_Omega = rep(0, 4),
  # sd_Omega = rep(2.5, 4),
  mu_b = 0.02501576,
  sd_b = 0.0016227417,
  rho_prior = 1
)

# Lista general con parámetros
stan_d <- list()
stan_d <- append(stan_d, discard(eventoObservacion2, is.data.frame))
stan_d <- append(stan_d, eventoDosificacion)
stan_d <- append(stan_d, hiperparametros)
stan_d

# Función de inicio de parámetros

init <- function() {
  hp <- hiperparametros
  
  CLHat <- rnorm(1, hp$mu_ClHat, hp$sd_ClHat)
  V1Hat <- rnorm(1, hp$mu_V1Hat, hp$sd_V1Hat)
  QHat  <- rnorm(1, hp$mu_QHat,  hp$sd_QHat)
  V2Hat <- rnorm(1, hp$mu_V2Hat, hp$sd_V2Hat)
  beta_Cl_logtCLCR <- rnorm(1, hp$mu_beta_Cl_logtCLCR, hp$sd_beta_Cl_logtCLCR)
  
  omega <- mapply(\(x, y) abs(rcauchy(1, x, y)),
                  hp$mu_Omega, hp$sd_Omega)
  b <- abs(rcauchy(1, hp$mu_b, hp$sd_b))
  
  logtheta <- matrix(rep(c(CLHat, V1Hat, QHat, V2Hat),
                        ea = eventoObservacion2$nSubjects),
                    nrow = eventoObservacion2$nSubjects)
  
  rho_prior <- hp$rho_prior
  
  logtheta <- log(logtheta)
  
  cHat <- rep(0.1, eventoObservacion2$nOBS)
  
  ls_total <- list(
    CLHat = CLHat,
    V1Hat = V1Hat,
    QHat  = QHat,
    V2Hat = V2Hat,
    beta_Cl_logtCLCR = beta_Cl_logtCLCR,
    omega = omega,
    rho_prior = rho_prior,
    b = b,
    logTheta = logtheta,
    cHat = cHat
  )
  return(ls_total)
}
