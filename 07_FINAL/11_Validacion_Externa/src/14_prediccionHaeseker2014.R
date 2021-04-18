##------------------------------------------------------------------------------#
## Nombre del Script: Chequeo de redimiento predictivo de modelo de Al Kofide --------
##  
## Propósito del Script: En este script se cargan las simulaciones realizadas en 
## '04_simulacionHaeseker2014.R'
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 08-03-2021
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

source(file.path('src', '69_cargaPaquetes.R'), encoding = 'UTF-8')
source(file.path('src', '70_performance_fun.R'), encoding = 'UTF-8')
source(file.path('src', '80_funConversionLog.R'), encoding = 'UTF-8')
source(file.path('src', '81_fun_organizacionSimulaciones.R'), encoding = 'UTF-8')

# Definición de directorios de trabajo
wdir <- file.path('04_Haeseker')
mdir <- file.path('model', 'FINAL.mlxtran')
rdir <- file.path('model', 'reference_model.mlxtran')

#-------------------------------------------------------------------------------#
# 1. Especificación de simulación ------------------------------------
#-------------------------------------------------------------------------------#
# N. pacientes en estudio
nIndiv <- 56 # N. pacientes en estudio
# Dosificación
param  <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)
# Desenlace
Cc  <- list(name = 'Cc', time = c(2, 10, 12, 14, 25))
y1  <- list(name = 'y1', time = c(2, 10, 12, 14, 25))
ind <- list(name = c('CLCRMLMIN'))
# Simulación de parámetros poblacionales desde FIM
dP <- mlxR::simpopmlx(n = nPob, project = mdir)

# Simulación de covariables
# 
# Para cada población simulada (n = 50), se simulan covariables para 31 
# individuos en cada población.
# 
covSim <- tibble(pop = 1:nPob) %>%
  mutate(parameter = list(data.frame(
    CLCRMLMIN = paramMoments(113, 57^2) %>% 
    {rlnorm(nIndiv, .$mu, sqrt(.$sigma2))}
    # WTKG = paramMoments(66.8, 17.1^2) %>% 
    # {rlnorm(nIndiv, .$mu, sqrt(.$sigma2))}
  ))) %>% 
  unnest(parameter)

# paramMoments(113, 57^2) %>%
#   {graficoDistr(113, .$mu, 57, sqrt(.$sigma2), c(0, 250))}

dP_DF <- tibble(dP) %>% 
  right_join(covSim, by = 'pop')

# Lista de control de simulación (N poblaciones con n individuos por población)
dP_ls <- lapply(1:dim(dP_DF)[1], function(x) {
    list(
      parameter = as.vector(as.data.frame(dP_DF)[x, ]),
      treatment = param,
      output    = list(y1, ind),
      size = c(1, 1),
      level = c('individual', 'longitudinal')
    )
  })

res <- simulx(
  model    = file.path(rdir),
  group    = dP_ls,
  settings = list(seed = 123456)
)

# Manipulación de datos de referencia
ref_data <- separarGruposRes(res$y1, nIndiv)  %>%
  rename(ytest = y1)

# ggplot(ref_data, aes(x=time, y=ytest, color=poblacion))+
#   geom_point()

#-------------------------------------------------------------------------------#
# 2. Carga de predicciones modelo externo ----------------------------------
#-------------------------------------------------------------------------------#
test_data <- read_csv(file.path(wdir, 'simulated_OBS.csv')) %>% 
  separarGruposRes(nIndiv)

total_data <- ref_data %>%
  left_join(test_data, by = c('poblacion', 'individuo', 'time')) %>% 
  group_by(poblacion) %>% 
  nest() %>% 
  mutate(
    MAE  = map_dbl(data, ~MAE( .x$ytest, .x$Cc)),
    RMSE = map_dbl(data, ~RMSE(.x$ytest, .x$Cc)),
    MAPE = map_dbl(data, ~MAPE(.x$ytest, .x$Cc))
  ) %>% 
  select(-data)


total_data %>% 
  ggplot(aes(x = MAPE * 100, y = 1)) +
  geom_boxplot(fill = alpha('gray', 0.4)) +
  geom_vline(xintercept = c(-20, +20), lty = 'dashed', col = 'red3') +
  coord_cartesian(xlim = c(-25, 60)) +
  xlab('Error de predicción (%)') +
  theme(axis.title.y = element_blank())

total_data %>% 
  write_csv(., file.path(wdir, 'resRendimientoPred.csv'))
