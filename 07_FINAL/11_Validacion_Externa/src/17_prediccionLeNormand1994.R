##------------------------------------------------------------------------------#
## Nombre del Script: Chequeo de redimiento predictivo de modelo de LeNormand --------
##  
## Propósito del Script: En este script se cargan las simulaciones realizadas en 
## '07_simulacionLeNormand1994.R'
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
source(file.path('src', '81_fun_organizacionSimulaciones.R'), encoding = 'UTF-8')

# Definición de directorios de trabajo
wdir <- file.path('07_LeNormand')
mdir <- file.path('model', 'FINAL.mlxtran')
rdir <- file.path('model', 'reference_model.mlxtran')

#-------------------------------------------------------------------------------#
# 1. Especificación de simulación ------------------------------------
#-------------------------------------------------------------------------------#
# N. pacientes en estudio
nIndiv <- 10 # N. pacientes en estudio
nPob <- 50
# Dosificación
param <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)
# Desenlace
Cc  <- list(name = 'Cc', time = c(2, 10, 12, 14, 25))
y1  <- list(name = 'y1', time = c(2, 10, 12, 14, 25))
ind <- list(name = c('CLCRMLMIN', 'WTKG'))
# Simulación de parámetros poblacionales desde FIM
dP <- mlxR::simpopmlx(n = nPob, project = mdir)

# Covariables individuales a comparar
ClCr_ls <- c(163L, 114L, 152L, 87L, 112L, 157L, 191L, 102L, 190L, 144L)
TBW_ls <- c(56, 55, 72.5, 65, 76.5, 55, 84.5, 56.5, 67, 58)

# Generación de tabla con simulaciones de 
dP_DF <- dP %>%
  add_column(covariates = list(data.frame(
    CLCRMLMIN = as.numeric(ClCr_ls), WTKG = TBW_ls
  ))) %>%
  tibble() %>%
  unnest(covariates)

# Lista de control de simulación (N poblaciones con n individuos por población)
dP_ls <-
  lapply(1:dim(dP_DF)[1], function(x) {
    list(
      parameter = as.vector(as.data.frame(dP_DF)[x, ]),
      treatment = param,
      output    = list(y1, ind),
      size = 1,
      level = 'longitudinal'
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

# ggplot(aes(x=time, y=y1, color=poblacion))+
#   geom_point()

#-------------------------------------------------------------------------------#
# 2. Carga de predicciones modelo externo ----------------------------------
#-------------------------------------------------------------------------------#
test_data <- read_csv(file.path(wdir, 'simulated_OBS.csv'))

total_data <- ref_data %>%
  left_join(test_data, by = c('individuo' = 'id', 'time' = 'time')) %>% 
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
  coord_cartesian(xlim = c(-200, 400)) +
  xlab('Error de predicción (%)') +
  theme(axis.title.y = element_blank())

total_data %>% 
  write_csv(., file.path(wdir, 'resRendimientoPred.csv'))
