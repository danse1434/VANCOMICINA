##------------------------------------------------------------------------#
## Nombre del Script: Estudio de tamizaje de covariables ------------------
##  
## Propósito del Script: realizar un estudio de la relación entre covariables 
##  con desviaciones eta con medios gráficos y regresión.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  19-03-2020
## Fecha de modificación:  03-02-2021
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
# Carga de paquetes
require(rlang)
require(grid)
require(tidyverse)
require(patchwork)

source(file.path('src', '010_funcionesEvaluacion.R'), encoding = 'UTF-8')

baseDir <- file.path('..', '..', '06_BASE_REFINADO', '05_BASE_REFINADO')

#-------------------------------------------------------------------------------#
# 1. Introducción --------------------------
#-------------------------------------------------------------------------------#
data_ori <- read_csv('./data/data_TAD.csv', na = '.', col_types = cols())

#-------------------------------------------------------------------------------#
# Apertura de archivos con parámetros individuales 
#................................................................................
#' 1 Apertura de archivo con parámetros estimados *indiv_param*
#' 2 Apertura de archivo con parámetros estimados *indiv_eta*
#' 3 Selección de las variables id y cualquiera que contenga "SAEM"
#................................................................................

indiv_param <- read_csv(
  file.path(baseDir, 'BASE', 'IndividualParameters', 'estimatedIndividualParameters.txt'), 
  col_types = cols())

indiv_eta <- read_csv(
  file.path(baseDir, 'BASE', 'IndividualParameters', 'estimatedRandomEffects.txt'),
  col_types = cols())

indiv_param <- indiv_param %>% 
  select(id, matches("SAEM"))

indiv_eta <- indiv_eta %>% 
  select(id, matches("SAEM"))

#-------------------------------------------------------------------------------#
# 2. Procesamiento de tabla -----------------------------------------
#-------------------------------------------------------------------------------#
# Unión de tablas
#................................................................................
#' 1 Unir tabla indiv_param a la tabla **data**
#' 2 Unir tabla indiv_eta a la tabla **data** 
#................................................................................

data <- data_ori %>%
  left_join(indiv_param, by = c('ID' = 'id')) %>%
  left_join(indiv_eta, by = c('ID' = 'id'))

#-------------------------------------------------------------------------------#
# Procesamiento de tabla
#................................................................................
#' 1 Seleccionar tabla completa
#' 2 Seleccionar eventos de administración y que tengan tiempo en cero, 
#' esto deja una tabla con las covariables y el valor de los parámetros 
#' (etas) individuales estimados.
#' 3 Eliminar variables innecesarias
#' 4 Eliminar variables que empiezen por valor de parámetro (par. individ) 
#................................................................................

data1 <- data %>%
  filter(EVID == '1' & TAD == 0) %>%
  select(-DV, -MDV, -EVID, -AMT, -TINF, -ADDL, -II, -SS, -TAD, -YTYPE) %>% 
  select(-matches("^(Cl\\_|V1|Q|V2)"))

write_csv(data,
          file.path('data', 'processed', '001_parametrosIndividuales.csv'))
write_csv(data1,
          file.path('data', 'processed', '002_etas.csv'))

#-------------------------------------------------------------------------------#
# 3. Creación de gráficos de manera programática -----------------------------
#-------------------------------------------------------------------------------#
# Creación de listas para cada tipo de desviación eta con colores diferentes
Cl_ls <- list_object(data1, eta_Cl_SAEM, 'Cl', col = 'blue')
Q_ls  <- list_object(data1, eta_V1_SAEM, 'Q',  col = 'green')
V1_ls <- list_object(data1, eta_V1_SAEM, 'V1', col = 'red')
V2_ls <- list_object(data1, eta_V2_SAEM, 'V2', col = 'purple')

#-------------------------------------------------------------------------------#
# Creación de documentos con gráficas en forma de layout

(reduce(Cl_ls, `+`) + plot_layout(ncol = 4)) %>% 
  {ggsave('001_corrEtaCl.pdf', (.), 'pdf', 'figures', 1, 6*1.7, 4*1.7)}

(reduce(Q_ls, `+`) + plot_layout(ncol = 4)) %>% 
  ggsave('002_corrEtaQ.pdf',  ., 'pdf', 'figures', 1, 6*1.7, 4*1.7)

(reduce(V1_ls, `+`) + plot_layout(ncol = 4)) %>% 
  ggsave('003_corrEtaV1.pdf', ., 'pdf', 'figures', 1, 6*1.7, 4*1.7)

(reduce(V2_ls, `+`) + plot_layout(ncol = 4)) %>% 
  ggsave('004_corrEtaV2.pdf', ., 'pdf', 'figures', 1, 6*1.7, 4*1.7)
