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
library(progress)

source(file.path('src', '69_cargaPaquetes.R'), encoding = 'UTF-8')
source(file.path('src', '70_performance_fun.R'), encoding = 'UTF-8')
source(file.path('src', '80_funConversionLog.R'), encoding = 'UTF-8')
source(file.path('src', '81_fun_organizacionSimulaciones.R'), encoding = 'UTF-8')

# Definición de directorios de trabajo
wdir <- file.path('02_SantosBuelga')
mdir <- file.path('model', 'FINAL.mlxtran')
rdir <- file.path('model', 'reference_model.mlxtran')

#-------------------------------------------------------------------------------#
# 1. Especificación de simulación ------------------------------------
#-------------------------------------------------------------------------------#
# N. pacientes en estudio
nIndiv <- 215 # N. pacientes en estudio
# Dosificación
param  <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)
# Desenlace
Cc  <- list(name = 'Cc', time = c(2, 10, 12, 14, 25))
y1  <- list(name = 'y1', time = c(2, 10, 12, 14, 25))
ind <- list(name = c('CLCRMLMIN', 'WTKG'))
# Simulación de parámetros poblacionales desde FIM
dP <- mlxR::simpopmlx(n = nPob, project = mdir)

# Simulación de covariables
# 
# Para cada población simulada (n = 50), se simulan covariables para 31 
# individuos en cada población.
# 
covTab <- read_csv(file.path(wdir, 'parameters.csv')) %>% 
  separarGruposRes(nIndiv) %>% 
  select(pop = poblacion, CLCRMLMIN = ClCr, WTKG = TBW) %>% 
  # Reconversión a mL/min
  mutate(CLCRMLMIN = CLCRMLMIN * 1000/60)

dP_DF <- tibble(dP) %>% 
  right_join(covTab, by = 'pop')

# Lista de control de simulación (N poblaciones con n individuos por población)
# Bins
c <- dim(dP_DF)[1]/nPob
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent) eta: (:eta)", total = nPob)
# 
ylist <- vector(mode = 'list', length = nPob)

for (i in 1:nPob) {
  # print(paste((c * (i-1)) + 1, c * i, sep = ' - ')) 
  pb$tick()
  a <- (c * (i-1)) + 1
  b <- c * i
  
  dP_ls <- lapply(a:b, function(x) {
      list(
        parameter = as.vector(as.data.frame(dP_DF)[x, ]),
        treatment = param,
        output    = list(y1, ind),
        size = 1, level = 'individual'
      )
    })
  
  res <- simulx(
    model    = file.path(rdir),
    group    = dP_ls,
    settings = list(seed = 123456)
  )
  
  ylist[[i]] <- res$y1
}

# Manipulación de datos de referencia
ref_data <- map_df(ylist, ~ .x, .id = 'poblacion') %>%
  tibble() %>%
  rename(ytest = y1) %>% 
  mutate(across(c(id, poblacion), ~ as.numeric(as.character(.x))))

# ggplot(ref_data, aes(x=time, y=ytest, color=poblacion))+
#   geom_point()

#-------------------------------------------------------------------------------#
# 2. Carga de predicciones modelo externo ----------------------------------
#-------------------------------------------------------------------------------#
test_data <- read_csv(file.path(wdir, 'simulated_OBS.csv')) %>% 
  separarGruposRes(nIndiv)

total_data <- ref_data %>%
  left_join(test_data, by = c('poblacion', 'id' = 'individuo', 'time')) %>% 
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
  coord_cartesian(xlim = c(-25, 100)) +
  xlab('Error de predicción (%)') +
  theme(axis.title.y = element_blank())

total_data %>% 
  write_csv(., file.path(wdir, 'resRendimientoPred.csv'))
