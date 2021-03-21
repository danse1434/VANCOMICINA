##------------------------------------------------------------------------------#
## Nombre del Script: IP de parámetros secundarios - Modelo Bayesiano ----------------
##  
## Propósito del Script: Extraer parámetros del modelo estimación bayesiana.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 10-01-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
require(MASS, exclude = 'filter')
require(gt)
require(Rcpp)
require(RcppArmadillo)
require(tidyverse)

# Compilar archivo C++
sourceCpp(file.path('src', '053_fun_parametrosSecundarios.cpp'))

#-------------------------------------------------------------------------------#
# 1. Lectura de archivo con datos bayesiano -------------------
#-------------------------------------------------------------------------------#
source(file.path('src', '002_pre_102_modeloFinal.R'), encoding = 'UTF-8')

modelName <- '102_modeloFinal'
load(file = file.path('models', paste0(modelName, "Fit.Rsave")))

#-------------------------------------------------------------------------------#
# 2. Extracción parámetros poblacionales   -----------------------
#-------------------------------------------------------------------------------#
# Detección de nombres de parámetros
parameters <- as.matrix(fit) %>% 
  colnames() %>% 
  keep(~ str_detect(.x, '(CL|Q|V1|V2)Hat|omega|rho1\\[3,4\\]|rho\\[3,4\\]|^a|beta')) #%>%
  # discard(~ str_detect(.x, 'rho1\\[1,1\\]|rho1\\[2,2\\]|rho1\\[3,3\\]|rho1\\[4,4\\]'))%>% 
  # discard(~ str_detect(.x, 'rho1\\[2,1\\]|rho1\\[3,1\\]|rho1\\[4,1\\]|rho1\\[3,2\\]|rho1\\[4,2\\]|rho1\\[4,3\\]'))

# Definición de estadísticos a calcular
fun_list <- list(
  mn = mean, 
  sd = sd, 
  med = median, 
  q2.5 = ~ quantile(.x, 0.025),
  q97.5 = ~ quantile(.x, 1-0.025))

# Generación de tabla
popParameters1 <- as.data.frame(fit, pars = parameters) %>% 
  mutate(
    List = pmap(list(1000, CLHat, QHat, V1Hat, V2Hat), constants_fun)
  ) %>%
  unnest(cols = c(List))


model_Specs <- popParameters1 %>%
  summarise(across(everything(), fun_list)) %>%
  pivot_longer(cols = everything(), names_to = c('parameter', 'statistic'),
               names_pattern = "(.*)_(.*)", values_to = 'vals') %>% 
  pivot_wider(id_cols='parameter', names_from = 'statistic', values_from='vals')

# Almacenamiento de modelo 
write_csv(model_Specs, file.path('data', 'processed', 'secondaryParameters.csv'))

# Creación tabla de Resumen en HTML
gt_model_Specs <- model_Specs %>% 
  gt(rowname_col = 'parameter') %>% 
  gt::fmt_number(columns = 2:6, decimals = 2) %>%
  gt::tab_spanner(label = 'Percentiles', columns = vars(q2.5, q97.5)) %>%
  gt::tab_header(
    title = md('**Resumen de parámetros estimados**'),
    subtitle = md('**Modelo Final con estimación bayesiana**')
  ) %>% 
  gt::cols_label(
    mn = 'Media',
    sd = 'Desv. Est.',
    med = 'Mediana',
    q2.5 = '2.5%',
    q97.5 = '97.5%') %>% 
  gt::tab_options(
    table.font.size = '16px',
    heading.title.font.weight = 'bold', 
    column_labels.font.weight = 'bold'
  )

# Almacenamiento de gráfico
gtsave(gt_model_Specs, '003_Parametros_Secundarios.html', 
       file.path('reports') %>% normalizePath())

