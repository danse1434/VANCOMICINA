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

  write_delim(x = jack_data, 
              file = file.path('data', paste0('data_TAD_del',i,'.csv')),
              delim = ",", na = '.')
}

