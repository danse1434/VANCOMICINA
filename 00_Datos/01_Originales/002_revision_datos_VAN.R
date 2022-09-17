##------------------------------------------------------------------------------#
## Nombre del Script: Revisión de datos de vancomicina -----------
##  
## Propósito del Script:  
##  
## Autor: Daniel S. Parra González 
##
## Fecha de creación: 14/09/2022
##  
## Copyright (c) Daniel S. Parra, 2022 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(readxl)

# Lectura de archivo con datos original
dataVAN <- read_csv(file.path("results", "data_TAD.csv"), na = ".")

dataVAN_sum <- dataVAN %>% 
  filter(EVID != 1) %>% 
  group_by(ID) %>% 
  summarise(across(DV, .fns = list(min = min, max = max)))

dataVAN_sum %>% summary()
