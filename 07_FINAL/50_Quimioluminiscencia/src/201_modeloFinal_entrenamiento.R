require(bayesplot)
require(shinystan)
require(patchwork)

source(file.path('src', '200_modeloFinal_preparacion.R'), encoding = 'UTF8')
source(file.path('src', '280_fun_funcion2Cptm.R'), encoding = 'UTF8')

modelName <- '205_modeloFinal'
run_file <- "run200"
fig_path <- file.path(run_file, "figures")

#-------------------------------------------------------------------------------#
# Modelamiento bayesiano ----------------
#-------------------------------------------------------------------------------#

# Ejecución Stan
if(!file.exists(file.path('models', paste0(modelName, "Fit.Rsave")))){
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
  nPost <- 2000 ## Número de muestras de cadenas (2000)
  nBurn <- 500  ## Número de muestras Burn-In (500)
  nThin <- 10   ## Gráfico ACF no muestra correlación con 10 (1)
  
  nIter <- (nPost + nBurn) * nThin
  nBurnin <- nBurn * nThin
  
  fit <- stan(file    = file.path('src', paste0(modelName, ".stan")),
              data    = stan_d,
              iter    = nIter,
              warmup  = nBurnin,
              thin    = nThin, 
              init    = init,
              chains  = nChains, 
              control = list(adapt_delta=0.95))
  
  print(Sys.time() - d)
  
  save(fit, file = file.path(run_file, str_glue("{modelName}_fit.Rsave")))
  
} else {
load(file = file.path(run_file, str_glue("{modelName}_fit.Rsave")))
}












