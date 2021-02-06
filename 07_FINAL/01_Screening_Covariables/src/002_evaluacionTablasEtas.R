##------------------------------------------------------------------------#
## Nombre del Script: análisis estadístico de tamizaje covariables de estudio
##  
## Propósito del Script:  
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  21-03-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
require(tidyverse)
require(gt)
require(glue)

source(file.path('src', '011_funcionesTablas.R'), encoding = 'UTF-8')
source(file.path('src', '012_funcionesCorrelacion.R'), encoding = 'UTF-8')
source(file.path('src', '013_funcionesGAM.R'), encoding = 'UTF-8')
source(file.path('src', '014_formatoCondicionalGT.R'), encoding = 'UTF-8')

#-------------------------------------------------------------------------------#
# 1. Introducción ------------------------------------------------------------
#-------------------------------------------------------------------------------#
data_ori <- read_csv(
  file.path('data', 'data_TAD.csv'), na = '.', col_types = cols())

# Transformación de variables categóricas en factor
data_ori <- data_ori %>%
  mutate(across(c(SEXF, ANTU), ~factor(.x)))

data1 <- read_csv(file.path('data', 'processed', '002_etas.csv'), 
                  col_types = cols())

# Definición de covariables
covariates <- c('SEXF', 'AGEA', 'WTKG', 'HCM', 'SCRMGDL', 
                'CLCRMLMIN', 'PROGDL', 'ALBGDL', 'RAL', 'RAN', 'ANTU')

#-------------------------------------------------------------------------------#
# 2. Tablas con resultados de regresión por eta ------------------------------
#-------------------------------------------------------------------------------#
vectorPars <- c('Cl', 'Q', 'V1', 'V2')
vectorEtas <- c('eta_Cl_SAEM', 'eta_Q_SAEM', 'eta_V1_SAEM', 'eta_V2_SAEM')


# Tabla para regresión glm() con pendiente e intercepto
df1 <- map(vectorEtas, ~glm_df(data1, .x, TRUE))
df1 <- map2(df1, vectorPars, ~ add_column(.x, 'eta' = .y, .before = 'tipo'))

df1 <- map_dfr(df1, ~.x) %>% 
  select(eta, tipo, term, p.value) %>% 
  filter(term != '(Intercept)') %>% 
  mutate(interp = ifelse(p.value < 0.05, 'Signif.', 'NS'))

# Tabla para regresión glm() con pendiente 
df2 <- map(vectorEtas, ~glm_df(data1, .x, FALSE))
df2 <- map2(df2, vectorPars, ~ add_column(.x, 'eta' = .y, .before = 'tipo'))

df2 <- map_dfr(df2, ~.x) %>% 
  select(eta, tipo, term, p.value) %>% 
  filter(term != '(Intercept)') %>% 
  mutate(interp = ifelse(p.value < 0.05, 'Signif.', 'NS'))

# Tabla para regresión lm() con pendiente e intercepto
df3 <- map(vectorEtas, ~glm_df(data1, .x, TRUE, 'lm'))
df3 <- map2(df3, vectorPars, ~ add_column(.x, 'eta' = .y, .before = 'tipo'))

df3 <- map_dfr(df3, ~.x) %>% 
  select(eta, tipo, term, p.value) %>% 
  filter(term != '(Intercept)') %>% 
  mutate(interp = ifelse(p.value < 0.05, 'Signif.', 'NS'))

# Tablas correlación Spearman
df4 <- map(vectorEtas, ~ corr_table(.x, covariates, data1))
df4 <- map2(df4, vectorPars,
            ~ add_column(.x, 'eta' = .y, .before = 'tipo')) %>%
  map_dfr( ~ .x)

# Modelos GAM (sólo variables continuas)
df5 <- map(vectorEtas, ~ gam_df(.x, covariates[-c(1, 11)], data1))

df5 <- map2(df5, vectorPars,
            ~ add_column(.x, 'eta' = .y, .before = 'tipo')) %>%
  map_dfr( ~ .x)

#-------------------------------------------------------------------------------#
# Creación de tabla compilada

df <- bind_rows(
  df1 %>% add_column(modelo = 'GLM con intercepto'),
  df2 %>% add_column(modelo = 'GLM sin intercepto'),
  df3 %>% add_column(modelo = 'LM con intercepto')
) %>%
  pivot_wider(
    id_cols = term,
    names_from = c(eta, modelo),
    values_from = p.value
  )

df_cols <- select(df,-term) %>%
  colnames()

df_cols <- df_cols %>%
  setNames(str_replace(., "(?<=(Cl|Q|V1|V2)).+", ''), .)

gt_linear <- df  %>% 
  gt() %>% 
  tab_spanner(label = 'LM con intercepto', columns=contains('LM con intercepto')) %>%
  tab_spanner(label = 'GLM con intercepto', columns=contains('GLM con intercepto')) %>%
  tab_spanner(label = 'GLM sin intercepto', columns=contains('GLM sin intercepto')) %>%  
  fmt_number(columns = 2:13, decimals=3) %>%
  cols_label(term = 'Parámetro') %>% 
  cols_label(.list = df_cols) %>% 
  tab_header(title = md('**Resultados de evaluación modelos lineales**'), 
             subtitle = 'Variable dependiente: eta') %>% 
  tab_source_note(source_note = 
    glue("Nota:  
         Se muestran valores p para estadísticos t en el parámetro pendiente ",
         "de los modelos.")) %>% 
  formatoCondicional(param=names(df_cols), alpha = 0.5) %>% 
  tab_options(table.font.size = '14px', 
              column_labels.font.weight = "bold", 
              heading.title.font.weight = "bold")

gtsave(gt_linear, '005_resumen_modelosLineales_eta.html', 
       file.path('figures') %>% normalizePath())


# Tabla de correlación spearman
gt_spearman <- df4 %>% 
  pivot_wider(id_cols=tipo, names_from=eta, values_from=p.value) %>% 
  gt() %>% 
  tab_header(title = md('**Pruebas de correlación Spearman**'), 
             subtitle = 'Variable dependiente: eta') %>% 
  fmt_number(columns = 2:5, decimals=3) %>% 
  cols_label(tipo = 'Tipo') %>% 
  formatoCondicional(param=c('Cl', 'Q', 'V1', 'V2'), alpha = 0.5) %>% 
  tab_source_note(source_note = 
  glue("Nota: 
       Se muestran valores p para prueba de Spearman.")) %>% 
  tab_options(table.font.size = '14px', 
              column_labels.font.weight = "bold", 
              heading.title.font.weight = "bold")

gtsave(gt_spearman, '006_resumen_Spearman_eta.html', 
       file.path('figures') %>% normalizePath())

# Tabla de modelo GAM
gt_GAM <- df5 %>% 
  pivot_wider(id_cols=tipo, names_from=eta, values_from=p.value) %>% 
  gt() %>% 
  tab_header(title = md('**Resultados de evaluación modelos GAM**'), 
             subtitle = 'Variable dependiente: eta') %>% 
  fmt_number(columns = 2:5, decimals=3) %>% 
  cols_label(tipo = 'Tipo') %>% 
  formatoCondicional(param=c('Cl', 'Q', 'V1', 'V2'), alpha = 0.5) %>% 
  tab_source_note(source_note = 
                    glue("Nota: 
       Se muestran valores p el término (s) del parámetro.")) %>% 
  tab_options(table.font.size = '14px', 
              column_labels.font.weight = "bold", 
              heading.title.font.weight = "bold")

gtsave(gt_GAM, '007_resumen_GAM_eta.html', 
       file.path('figures') %>% normalizePath())
