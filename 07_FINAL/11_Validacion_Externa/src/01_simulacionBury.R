##------------------------------------------------------------------------------#
## Nombre del Script: Simulación de muestras de estudio de Bury 2019 que 
## comparó individuos sanos con pacientes con neutropenia.
##  
## Propósito del Script: La definición de NF es diferente para este tipo.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 04 - marzo - 2021
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

# Carga de paquetes
require(tidyverse)
require(gt)
monolix2019R2.path <-  "C:/ProgramData/Lixoft/MonolixSuite2019R2"
require(lixoftConnectors, lib.loc = monolix2019R2.path )
require(mlxR)
initMlxR(path = monolix2019R2.path)

# Carga de funciones
source(file.path('src', '80_funciones.R'), encoding = 'UTF-8')

# Definición de directorios de trabajo
wdir <- file.path('01_Bury')
mdir <- file.path(wdir, '01_Bury.mlxtran')
n <- 116 # N. pacientes en estudio

#-------------------------------------------------------------------------------#
# 1. Especificación de parámetros   ---------------------
#-------------------------------------------------------------------------------#
p <- list()

# Poblacionales
# Parámetros Estructurales
p['mCl'] = log(3.22) # L/h
p['gCl'] = lognormalSD(3.22, 2.97, 3.48, n) # L/h
p['mTheta_2'] = log(0.00834)
p['gTheta_2'] = lognormalSD(0.00834, 7.37E-3, 9.28E-3, n)

p['mTheta_3'] = log(1.277)
p['gTheta_3'] = lognormalSD(1.277, 1.102, 1.462, n)
p['mQ'] = log(4.03)
p['gQ'] = lognormalSD(4.03, 2.81, 5.22, n)
p['mV1'] = log(45.8)
p['gV1'] = lognormalSD(45.8, 40.8, 51.1, n)
p['mV2'] = log(51.7)
p['gV2'] = lognormalSD(51.7, 37.2, 66.2, n)

# Variabilidad interindividual
p['mOmega_Cl'] = log(31/100)
p['gOmega_Cl'] = lognormalSD(0.31, 0.251, 0.353, n)
p['mOmega_V1'] = log(35.2/100)
p['gOmega_V1'] = lognormalSD(0.352, 0.25, 0.438, n)
p['mOmega_V2'] = log(97.8/100)
p['gOmega_V2'] = lognormalSD(0.978, 0.609, 1.24, n)

# Variabilidad residual
p['mA'] = log(2.07)
p['gA'] = lognormalSD(2.07, 1.70, 2.44, n)
p['mB'] = log(16.7/100)
p['gB'] = lognormalSD(0.167, 0.147, 0.184, n)

# Covariables
p['lnClCr_pop'] = 4.53
p['sd_lnClCr']  = 0.69
p['FFM_pop']    = 59.1
p['omega_FFM']  = 5.7
p['ANC_pop']    = log(4200)
p['omega_ANC']  = log(9900)

#-------------------------------------------------------------------------------#
# 2. Definición de dosis y t. administración ----------------------
#-------------------------------------------------------------------------------#
y     <- list(name = 'y', time = c(2, 10, 12, 14, 25))
Cc    <- list(name = 'Cc', time=seq(1, 30, length.out =100))
ind   <- list(name = c('Cl', 'Q', 'V1', 'V2', 'a', 'b', 
                       'ANC', 'ClCr', 'FFM', 
                       'Cl_pop', 'theta_2', 'theta_3', 
                       'Q_pop', 'V1_pop', 'V2_pop', 
                       'omega_Cl', 'omega_V1', 'omega_V2'))

param <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)

#-------------------------------------------------------------------------------#
# 3. Simulación -------------------------------------------
#-------------------------------------------------------------------------------#

res <- simulx(model     = file.path(mdir), 
              parameter = unlist(p), 
              output    = list(y, ind),
              group     = list(size = c(n, 50), level = c('covariate', 'population')),
              treatment = param,
              settings  = list(seed = 123456))

#-------------------------------------------------------------------------------#
# 4. Gráficos y almacenamiento  -----------------------------------
#-------------------------------------------------------------------------------#
theme_set(theme_bw())

dataOBS <- res$y %>%
  mutate(
    id = as.numeric(as.character(id)),
    poblacion = (id - 1) %/% n,
    covariable = id - (poblacion * n)
  )

# Perfil plasmático de gOBS

gOBS <- ggplot(dataOBS) +
  geom_point(aes(x = time, y = y, col = covariable), 
             alpha = 0.5) +
  theme(legend.position = 'none') + 
  xlab('Tiempo tras dosis (h)') + 
  ylab('Concentración plasmática (mg/L)')

tablaParam <- res$parameter %>%
  summarise(across(!contains('id'), list(
    mn = mean,
    sd = sd,
    mi = min,
    q1 = function(x) quantile(x, probs = 0.25),
    q2 = function(x) quantile(x, probs = 0.50),
    q3 = function(x) quantile(x, probs = 0.75),
    ma = max
  ))) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(
    col = name,
    into = c('parameter', 'statistic'),
    sep = '\\_(?=\\w(\\d|\\w)$)'
  ) %>% 
  pivot_wider(names_from = parameter,
              values_from = value)

# Tabla de resumen de parámetros
gt(tablaParam) %>%
  tab_header(title = 'Parámetros muestreados') %>%
  fmt_number(columns = c(4), decimals = 0) %>% 
  fmt_number(columns = c(2,3,6,7, 9:11, 13, 14:19), decimals = 2) %>%
  fmt_number(columns = c(12), decimals = 3) %>% 
  tab_options(table.font.size = '12px') %>% 
  gtsave(., 'plotOBS.html', file.path(wdir) %>% normalizePath())

# res$parameter %>%
#   pivot_longer(cols = !contains('id')) %>% 
#   mutate(value = ifelse(is.na(value), 0, value)) %>% 
#   ggplot(.) +
#   geom_histogram(aes(x = value)) +
#   facet_wrap(. ~ name, scales = 'free')

# res$y %>%
#   # {pull(., 'y') %>% quantile(probs = c(0.025, 0.975), na.rm = T)}
#   ggplot(aes(x = y)) +
#   geom_density() + 
#   coord_cartesian(xlim = c(-8, 48))

# Almacenamiento de figuras y tablas
ggsave(file.path(wdir, 'plot_OBS.pdf'), gOBS, 'pdf', width = 8, height = 6)
write_csv(dataOBS, file.path(wdir, 'simulated_OBS.csv'))
write_csv(res$parameter, file.path(wdir, 'parameters.csv'))
