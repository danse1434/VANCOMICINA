##------------------------------------------------------------------------------#
## Nombre del Script:  análisis de individuos atípicos mediante jacknife
##  construcción de set de datos ------------------------------------------
##
## Proposito del Script: preparación de carpetas para análisis por Jacknife 
##  con el fin de evaluar la influencia de cada paciente en la estimación de 
##  parámetros poblacionales.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  23-12-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(rlang)
require(tidyverse)
require(glue)

#-------------------------------------------------------------------------------#
# Lectura y creación de tablas --------
#-------------------------------------------------------------------------------#
data <- read_csv("data/data_TAD.csv", na = c('.'),
                   trim_ws = TRUE)

id_vector <- unique(data$ID)


for (i in 1:length(id_vector)) {
  
  jack_data <- data %>%
    filter(ID != id_vector[i])
  
  n_unique <- (unique(jack_data$ID))
  
  jack_data <- jack_data %>% 
    mutate(
      OLD_ID = ID,
      ID = factor(ID),
      ID = fct_relabel(ID, ~match(.x, n_unique) %>% as.character())
      ) 
  
  write_delim(x = jack_data,
              file.path('data', paste0('data_TAD_del',i,'.csv')),
              delim = ",", na = '.')
}

#-------------------------------------------------------------------------------#
# 2. Creación de proyectos Monolix -----------------------------
#-------------------------------------------------------------------------------#
fileName <- file.path('models', 'M2CPTM_nobs_2_Aditv_corr2.mlxtran')
Z = readChar(fileName, file.info(fileName)$size)


Z1 <-
  # Sólo ejecutar la estimación de parámetros por SAEM
  str_replace(
    Z,
    regex('(?<=\\[TASKS\\]).+(?=\\[SETTINGS\\])', dotall = TRUE),
    '\r\npopulationParameters()\r\n'
  ) %>%
  # Cambio de configuración del algoritmo SAEM
  str_replace('(?<=burniniterations\\s\\=\\s)\\d{1,6}', "50") %>%
  str_replace('(?<=smoothingiterations\\s\\=\\s)\\d{1,6}', "2000") %>%
  str_replace('(?<=exploratoryiterations\\s\\=\\s)\\d{1,6}', "2000") %>%
  str_replace('(?<=simulatedannealingiterations\\s\\=\\s)\\d{1,6}', "100") %>%
  str_replace('(?<=exploratoryinterval\\s\\=\\s)\\d{1,6}', "1000") %>%
  str_replace('(?<=smoothinginterval\\s\\=\\s)\\d{1,6}', "1000") %>% 
  str_replace(regex('(?<=\\[COMMENTS\\]\\r\\n\\;).+(?=\\r\\n\\r\\n)', dotall = TRUE), 
              "Modelo Base: detección influencia ID**")
Z1

for (i in 1:length(id_vector)) {
  Y <- Z1 %>% 
    str_replace('(?<=\\.\\./data/data\\_TAD)\\.(?=csv)', glue('_del{i}.')) %>% 
    str_replace("(?<=exportpath\\s\\=\\s\\')M2CPTM\\_nobs\\_2\\_aditv\\_corr2(?=\\')", 
                glue('evalJack{i}')) %>% 
    str_replace('(?<=ID)\\*{2}', glue('{i}'))
  # Escribir archivos
  write_lines(Y, file.path('models', glue('evalJack{i}.mlxtran')), sep = '\n')
}








