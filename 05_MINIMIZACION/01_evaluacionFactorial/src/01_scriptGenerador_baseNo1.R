##------------------------------------------------------------------------------#
## Nombre del Script: Generación de directorios de trabajos para análisis -------
##      factorial de convergencia del modelo 
##
## Propósito del Script: realizar una estimación de la influencia de valores 
##      iniciales en la convergencia del modelo base
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  25-08-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(rlang)
require(tidyverse)
#
# Leer archivo *csv con parámetros de modelo base 
parametrosPoblacionales <- read_csv(file.path(
    "..", "..", "03_RESIDUAL", "M2CPTM_nobs_1_prop", "M2CPTM_nobs_1_prop", 
    "populationParameters.txt"
  ))

pop_par <- parametrosPoblacionales %>% 
  mutate(minvalue = 0.5 * value,
         maxvalue = 1.5 * value) %>% 
  filter(parameter %in% c('Cl_pop', 'V1_pop', 'Q_pop', 'V2_pop')) %>%
  select(parameter, value, minvalue, maxvalue) %>%
  t(.)

A <- apply(pop_par[-1,], 2, as.numeric)
colnames(A) <- pop_par[1,]
A

B <- expand.grid(
  Factor1 = A[, 1],
  Factor2 = A[, 2],
  Factor3 = A[, 3],
  Factor4 = A[, 4]
)

colnames(B) <- pop_par[1,]
# Matriz con re-arreglos
B
write_csv(B, 'results/01_arregloOrigen.csv')
# 
#-------------------------------------------------------------------------------#
# Creación de archivos de -------------------------------------------------
#-------------------------------------------------------------------------------#
fileName <- 'M2CPTM_nobs_1_prop.mlxtran'
Z = readChar(fileName, file.info(fileName)$size)

Z <-
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
  str_replace('(?<=smoothinginterval\\s\\=\\s)\\d{1,6}', "1000")

# Creación de directorio principal
dir.create('evaluacion')

for (i in 1:dim(B)[1]) {
  # Directorio auxiliar
  directorioAux <- file.path('evaluacion', paste0('evaluacion', i))
  # Crear directorio auxiliar
  dir.create(directorioAux)
  # Crear carpeta de datos
  dir.create(file.path(directorioAux, 'data'))
  # Cambiar los valores iniciales de la función
  Y <- Z %>%
    str_replace("(?<=Cl\\_pop\\s\\=\\s\\{value\\=)\\d+\\.\\d+(?=\\,\\smethod)",
                paste(B[i, 'Cl_pop'])) %>%
    str_replace("(?<=V1\\_pop\\s\\=\\s\\{value\\=)\\d+\\.\\d+(?=\\,\\smethod)",
                paste(B[i, 'V1_pop'])) %>%
    str_replace("(?<=Q\\_pop\\s\\=\\s\\{value\\=)\\d+\\.\\d+(?=\\,\\smethod)",
                paste(B[i, 'Q_pop'])) %>%
    str_replace("(?<=V2\\_pop\\s\\=\\s\\{value\\=)\\d+\\.\\d+(?=\\,\\smethod)",
                paste(B[i, 'V2_pop']))
  # Escribir archivos
  write_lines(Y, file.path(directorioAux, fileName), sep = '\n')
  # Copiar archivo con datos originales
  file.copy(from = file.path('data/data_TAD.csv'),
            to = file.path(directorioAux, 'data'))
}
