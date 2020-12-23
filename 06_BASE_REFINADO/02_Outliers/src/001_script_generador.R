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
