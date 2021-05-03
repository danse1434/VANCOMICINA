##------------------------------------------------------------------------------#
## Nombre del Script: Análisis de datos de convergencia SAEM --------------------
##  
## Propósito del Script: se realiza un análisis de convergencia del algoritmo 
## SAEM con diversos valores iniciales de los parámetros del modelo. Esto para el 
## modelo final con covariables Cl-SCR.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 13-06-2020 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
require(rlang)
require(glue)
require(ggrepel)
require(scales)
require(patchwork)
require(tidyverse)
require(directlabels) # Rectas al final de gráficos

source(file.path('src', '010_funciones_PCA_R.R'), encoding = 'UTF-8')

# Vector con orden de parámetros
level_par <- c('Cl_pop', 'beta_Cl_logtCLCRMLMIN', 
               'V1_pop', 'Q_pop', 'V2_pop', 
               'omega_Cl', 'corr_V2_V1', 'omega_V1', 'omega_V2', 'omega_Q', 
               'a1', 'a2')

# Selección directorio auxiliar
aux_dir <- file.path(getwd(), 'evaluacion')

#-------------------------------------------------------------------------------#
# 1 Resumen Parámetros de Diseño Factorial --------------------------------------
#-------------------------------------------------------------------------------#
# Lista vacía *d_fct_ls*
d_fct_ls <- vector('list', 81)
LLfct_ls <- vector('list', 81)
# Lectura de datos de parámetros estimados
for (i in 1:81) {
  subdir <- file.path(aux_dir, paste0('E', i), 'modeloFinal')
  # Asignación a vector
  d_fct_ls[[i]] <-
    read_csv(file.path(subdir, 'populationParameters.txt'),
             col_types = cols())
  
  LLfct_ls[[i]] <-
    read_csv(file.path(subdir, 'ChartsData', 'Saem', 'CvParam.txt'),
             col_types = cols())
  
}
# Conversión en data.frame unificado
d_fct_ls1 <- d_fct_ls %>%
  map_dfr( ~ as_tibble(.x), .id = 'ID') %>%
  mutate(ID = as.double(ID),
         parameter = factor(parameter, levels = level_par)) %>% 
  # Indicador de outlier
  group_by(parameter) %>% 
  mutate(
    outlier_1 = abs(value - mean(value)) > 3*sd(value), 
    outlier_2 = abs((value - median(value))/mad(value)) > 6, 
  ) %>% 
  ungroup()


d_fct_ls2 <- d_fct_ls1 %>% 
  pivot_wider(id_cols='ID', names_from = 'parameter', values_from = 'value') %>% 
  add_column(
    LL = map_dbl(LLfct_ls, ~tail(.x, 1) %>% pull(convergenceIndicator))
  )



g_fct_ls2 <- d_fct_ls2 %>%
  select(-ID) %>%
  pivot_longer(!matches('LL')) %>%
  ggplot(aes(x = value, y = LL)) +
  geom_point() +
  theme_bw() +
  facet_wrap(. ~ name, ncol = 4, scales = 'free')


ggsave('./10_relacion_parametros.pdf', g_fct_ls2, 'pdf', 'figures', 1, 8, 6)
