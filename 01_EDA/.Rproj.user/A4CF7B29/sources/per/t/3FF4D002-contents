##------------------------------------------------------------------------------#
## Nombre del Script: EDA de Vancomicina ----------
##  
## Propósito del Script:  Análisis Exploratorio de Datos (EDA) de vancomicina.
##                        Variables Categóricas
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  15-07-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(lubridate)
require(rlang)
require(tidyverse)
require(PerformanceAnalytics)
require(patchwork)
require(gt)
require(readxl)

# Lectura de datos
df_VAN <- read_xlsx('../00_Datos/01_Originales/original_200219.xlsx')

#' Preparación de df inicial
#................................................................................
#' 1 Eliminar el individuo 15
#' 2 Eliminar variables no utilizadas
#' 3 Renombrar variables específicas
#................................................................................

df_VAN_selected <- df_VAN %>% 
  filter(record_id != 15) %>% 
  select(-matches("^antibioticos_recibidos_")) %>% 
  select(-matches("^cual_antibiotico_combino_")) %>% 
  select(-matches("^cual_microorganismo_")) %>% 
  select(-matches("^Resultado")) %>% 
  select(-matches("^fecha_hora")) %>% 
  select(-matches("^esquema")) %>% 
  select(-matches("complete$")) %>% 
  select(-matches("^cedula|ra")) %>% 
  select(-matches("207")) %>% 
  select(-matches("iniciales|Codigo")) %>%
  select(-contains("fecha")) %>% 
  select(-matches("tipo_de_infeccion_")) %>% 
  rename(
    ID        = record_id,
    SEXF      = sexo,
    ANTU      = uso_antibiotico_mes_previo
  )


df_VAN_selected_1 <- df_VAN_selected %>%
  mutate(
    SEXF = if_else(SEXF == 1, 'Hombre', 'Mujer'),
    tipo_neoplasia = case_when(
      tipo_neoplasia == 1 ~ 'Linfoma',
      tipo_neoplasia == 2 ~ 'Leucemia Linfoide',
      tipo_neoplasia == 3 ~ 'Leucemia Mieloide',
      TRUE ~ NA_character_
    ),
    numero_ultimo_ciclo = factor(numero_ultimo_ciclo),
    ANTU = if_else(ANTU == 1, 'Si', 'No')
  )

df_VAN_selected_2 <- df_VAN_selected_1 %>% 
  select(ID, SEXF, ANTU, tipo_neoplasia, numero_ultimo_ciclo) %>% 
  pivot_longer(cols      = -ID,
               names_to  = 'Variable',
               values_to = 'Valor') %>%
  count(Variable, Valor) %>%
  group_by(Variable) %>%
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(Variable =
           factor(
             Variable,
             levels = c('SEXF', 'ANTU', 'tipo_neoplasia', 'numero_ultimo_ciclo')
           )) %>%
  arrange(Variable)

#-------------------------------------------------------------------------------#
# Almacenamiento de gt()

gt_VAN_desc_categor <- df_VAN_selected_2 %>% 
  gt(groupname_col = 'Variable') %>%
  tab_header(title = html('<b>&#x2605;Estadísticos descriptivos &#x2605;</b>'),
             subtitle = md('*Variables categóricas*')) %>% 
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  ) 

gt_VAN_desc_categor %>% 
  gtsave(filename = 'gt_desc_categor.html', path = file.path(getwd(), 'output/tabs/'))
