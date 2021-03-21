##------------------------------------------------------------------------------#
## Nombre del Script: Comparación de resultados de análisis bivariado de LL con
## resultado de muestreo bayesiano.
##  
## Propósito del Script: Se realiza una comparación de los resultados del análisis 
## del perfil de verosimilitud en dos dimensiones con los resultados del análisis 
## bayesiano para el modelo base.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  17-12-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

# Carga de paquetes
require(patchwork)
require(rlang)
require(tidyverse)
require(gt)
require(plotly)

source(file.path('src', '001_pre_101_modeloFinal.R'), encoding = 'UTF8')
source(file.path('src', '051_fun_resultados.R'), encoding = 'UTF8')
source(file.path('src', '052_fun_comparacionML.R'), encoding = 'UTF8')

modelName <- '107_modeloBaseArreglado'
load(file = file.path('models', paste0(modelName, "Fit.Rsave")))

#-------------------------------------------------------------------------------#
# 1. Lectura de archivos -------
#-------------------------------------------------------------------------------#
# 1.1. Modelo Stan
modelName <- '102_modeloFinal'
load(file = file.path('models', paste0(modelName, "Fit.Rsave")))

# 1.2. Perfil LL bivariado
pLL_dir <- file.path('..',  '06_Minimizacion', '03_mapeoFuncionLL')
results_dir <- file.path(pLL_dir, 'results')

# Perfil 1
df_Cl_pop_beta_Cl_tCLCRMLMIN <-
  extractor('Cl_pop_beta_Cl_tCLCRMLMIN', results_dir, n = 900)

# Perfil 2
df_beta_Cl_logtWTKG_beta_Cl_tCLCRMLMIN <-
  extractor('beta_Cl_logtWTKG_beta_Cl_tCLCRMLMIN', results_dir, n = 900)

# 1.3. Lectura de archivo poblacional
popParam_dir <- file.path(pLL_dir, 'modeloFinal')

popParam <-
  read_csv(file.path(popParam_dir, "populationParameters.txt")) %>% 
  select(parameter, value) %>% 
  pivot_wider(names_from = parameter, values_from = value)

#-------------------------------------------------------------------------------#
# 2. Crear gráfico con inserto ----------
#-------------------------------------------------------------------------------#
fit_mcmc <- As.mcmc.list(fit)

ls_opt_1 <- list()
ls_opt_2 <- list(left = 0.6, bottom = 0.6, right = 1, top = 1, clip = TRUE)

blank_theme <- theme_minimal() + theme(
  legend.position = 'none',
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.background = element_rect(fill =
                                   NA, colour = NA)
)

lista_2D <- list()


# > 2.1. Generación de gráficos -------------------------------------------
grafico_2D_LS <- list()

grafico_2D_LS[[1]] <- 
  generarGrafico2D(df_Cl_pop_beta_Cl_tCLCRMLMIN, popParam, 
                   'Cl_pop', 'beta_Cl_tCLCRMLMIN', 'LL1', 
                   expression(theta[0]~'TVCL (L/h)'), bquote(theta[1]~'Cl-ClCr'), n_bins = 20, 
                   barheight = 6)

grafico_2D_LS[[2]] <- 
  generarGrafico2D(df_beta_Cl_logtWTKG_beta_Cl_tCLCRMLMIN, popParam, 
                   'beta_Cl_logtWTKG', 'beta_Cl_tCLCRMLMIN', 'LL1', 
                   bquote(theta[2]~'Cl-logtWT'), bquote(theta[1]~'Cl-tClCr'), n_bins = 20,
                   barheight = 6)


lista_2D[[1]] <- (grafico_2D_LS[[1]] + 
                    theme(panel.background = element_rect(fill = 'gray'))) %>% 
  modificarInserto(., fit_mcmc, 
                   'CLHat', 'beta_Cl_logtWTKG', 
                   xlim = c(3.5, 10), ylim = c(0.4, 2))

lista_2D[[2]] <- (grafico_2D_LS[[2]] + 
                    theme(panel.background = element_rect(fill = 'gray'))) %>% 
  modificarInserto(., fit_mcmc, 
                   'beta_Cl_logtWTKG', 'beta_Cl_tCLCRMLMIN', 
                   xlim = c(0.4, 1.8), ylim = c(0.35, 1.2))

# Creación de composición de gráficos
(lista_2D %>% 
    reduce(., `+`)+
    plot_layout(ncol=2)) %>% 
  ggsave(paste0(modelName, '_CompML.pdf'), plot = ., 'pdf', path = 'figures',
         width = 8, height = 4)
