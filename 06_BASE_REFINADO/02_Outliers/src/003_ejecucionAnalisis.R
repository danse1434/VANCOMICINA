##------------------------------------------------------------------------------#
## Nombre del Script: Ejecución de estimación de modelo bayesiano  --------------
##  
## Propósito del Script: Ejecución de estimación de modelo bayesiano para cada 
## set de datos creado con delección-1
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 23-12-2020 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
require(tidyverse)
require(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(20201209)

# Fuente
source('./src/010_funciones_procesamiento.R', encoding = 'UTF8')

# Nombre del modelo
modelName <- '104_modeltwoCptmDiagProp_errResNor'


#-------------------------------------------------------------------------------#
# Estimación bayesiana para cada set de datos

for (i in 1:14) {
  data <- read_csv(file.path('data', paste0('data_TAD_del',i,'.csv')), na = '.')
  datosProcesados_1  <- procesamientoDatos(data)
  datosProcesados_2  <- formacionInputs(datosProcesados)
  eventoObservacion  <- datosProcesados_2$evOBS
  eventoDosificacion <- datosProcesados_2$evDosis
  
  source('./src/002_preparacionPrevios.R', encoding = 'UTF8')
  
  # Ejecución Stan
  if(!file.exists(file.path('models', paste0(modelName, '_del', i, "Fit.Rsave")))){
    # Ejecución de pruebas
    test <- stan(
      file.path('src', paste0(modelName, '.stan')), # Modelo Stan
      data = stan_d, # Datos
      chains = 1, # Cadenas
      init = init,
      iter = 10 # Iteraciones
    )
    d <- Sys.time()
    nChains <- 4
    nPost <- 1000 ## Número de muestras de cadenas
    nBurn <- 500  ## Número de muestras Burn-In
    nThin <- 10   ## Gráfico ACF no muestra correlación con 10
    
    nIter <- (nPost + nBurn) * nThin
    nBurnin <- nBurn * nThin
    
    fit <- stan(file = file.path('src', paste0(modelName, ".stan")),
                data = stan_d,
                iter = nIter,
                warmup = nBurnin,
                thin = nThin, 
                init = init,
                chains = nChains)
    
    d <- d- Sys.time(); print(d)
    
    save(fit, file = file.path('models', paste0(modelName, '_del', i, "Fit.Rsave")))
    
  } else {
    load(file = file.path('models', paste0(modelName, '_del', i, "Fit.Rsave")))
  }
}