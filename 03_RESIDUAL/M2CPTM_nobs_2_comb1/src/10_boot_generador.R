##------------------------------------------------------------------------------#
## Nombre del Script: Creación de data sets con instrucciones de bootstrap ------
## y almacenamiento en carpetas correspondientes
##  
## Propósito del Script: creación de muestreos sin reemplazo para obtener 
## estimación de intervalos de confianza de parámetros poblacionales.  
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  12-02-2020
## Fecha de modificación:  12-08-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# Introducción -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(rlang)
require(glue)

#-------------------------------------------------------------------------------#
auxdir  <- file.path('.', 'M2CPTM_nobs_2_comb1')
bootdir <- file.path(auxdir, 'results', 'boot')
project.file <- file.path(auxdir, 'M2CPTM_nobs_2_comb1.mlxtran')
paramet.file <- file.path(auxdir, 'M2CPTM_nobs_2_comb1')
dir.create(file.path(auxdir, 'results'))
dir.create(file.path(bootdir))

#-------------------------------------------------------------------------------#
# Generación de subdirectorios de trabajos
set.seed(12356)

data <- read_csv(file.path(auxdir, 'data', 'data_TAD.csv'), na = '.')

data.ls <- data %>%
  group_by(ID) %>%
  nest()

table.ls = vector(mode = 'list', 1000L)

for (i in 1:1000) {
  table.ls[[i]] <- data.ls %>% 
    ungroup() %>% 
    sample_n(14, replace = TRUE) %>% 
    add_column(new_ID = 1:14) %>% 
    unnest(cols = c(data))
}

for (i in 1:1000) {
  dir.create(file.path(bootdir, glue('B{i}/')))
  
  write_csv(x = table.ls[[i]],
    path = file.path(bootdir, glue('B{i}/data_{i}.csv')),
    na = '.')
}

for (i in 1:1000) {
  file.copy(file.path(auxdir, glue('M2CPTM_nobs_2_comb1.properties')),
            file.path(bootdir, glue('B{i}/')))
}

#-------------------------------------------------------------------------------#
# Apertura del archivo de control
fileName <- file.path(auxdir, 'M2CPTM_nobs_2_comb1.mlxtran')
A = readChar(fileName, file.info(fileName)$size)

# Modificación de efectos aleatorios para mejorar la estimación SAEM
# Con esto se modifican los valores iniciales se dejan en 1.00 y esto permite una 
# mejor búsqueda en el espacio muestral. 

A1 <- A %>% 
  str_replace('(?<=ANTU)\\}', ', new_ID}') %>% 
  str_replace('ID(?=\\s\\=\\s)', 'new_ID') %>% 
  str_replace_all('(?<=a\\d\\s\\=\\s\\{value\\=)(\\-0|.)\\.\\d+(?=\\,)','0.3') %>%
  str_replace_all('(?<=b\\d\\s\\=\\s\\{value\\=)(\\-0|.)\\.\\d+(?=\\,)','0.3') %>%
  str_replace_all('(?<=omega_Cl\\s\\=\\s\\{value\\=)(\\-0|.)\\.\\d+(?=\\,)','1.00') %>%
  str_replace_all('(?<=omega_Q\\s\\=\\s\\{value\\=)(\\-0|.)\\.\\d+(?=\\,)','1.00') %>%
  str_replace_all('(?<=omega_V1\\s\\=\\s\\{value\\=)(\\-0|.)\\.\\d+(?=\\,)','1.00') %>%
  str_replace_all('(?<=omega_V2\\s\\=\\s\\{value\\=)(\\-0|.)\\.\\d+(?=\\,)','1.00') %>% 
  str_replace_all('(?<=omega_f\\s\\=\\s\\{value\\=)(\\-0|.)\\.\\d+(?=\\,)','1.00') %>% 
  str_replace_all('data\\/', '') %>% 
  # Selección de tarea a ejecutar en archivo de control
  str_replace(regex("(?<=\\[TASKS\\]).+(?=\\[SETTINGS\\])", dotall = TRUE),
              '\r\npopulationParameters()\r\nplotResult(method = {saemresults})\r\n\r\n')

# str_view(A1, regex("(?<=\\[TASKS\\]).+(?=\\[SETTINGS\\])", dotall = TRUE))

for (i in 1:1000) {
  B <- A1 %>%
    str_replace("(?<=\\_)TAD", glue('{i}'))
  
  write_lines(B,
              file.path(bootdir, glue('B{i}/M2CPTM_nobs_2_comb1.mlxtran')), sep = '\n')
}
