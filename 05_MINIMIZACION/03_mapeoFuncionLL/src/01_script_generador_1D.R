##------------------------------------------------------------------------------#
## Nombre del Script: Perfilamiento de función de verosimilitud (-2LL) -------
##  
## Propósito del Script: este script tiene como fin realizar una preparación 
##  de archivos y carpetas para la evaluación de mapeo de verosimilitud 
##  univariada.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 29-11-2020  
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

# Carga de paquetes
require(rlang)
require(tidyverse)
require(glue)

# Directorio externo donde se ubica el modelo base 1
dirModeloBase <- file.path(
  "..", "..", "03_RESIDUAL", "M2CPTM_nobs_1_prop"
)

# Leer archivo *csv con parámetros de modelo base 
parametrosPoblacionales <-
  read_csv(file.path(
    dirModeloBase,
    'M2CPTM_nobs_1_prop',
    'populationParameters.txt'
  ))

#-------------------------------------------------------------------------------#
# Selección de parámetro a evaluar
par_eval = 'b'

# Cálculo de valor mínimo (50%) y valor máximo (500%) respecto al valor 
# nominal estimado en el modelo base.
pop_par <- 
  parametrosPoblacionales %>% 
  filter(parameter == par_eval) %>% 
  select(value) %>% 
  magrittr::use_series(value)

pop_vector <- pop_par * seq(0.5, 5.0, length.out = 100)

#-------------------------------------------------------------------------------#
# Preparación de carpetas
dir.create(file.path('results', par_eval), showWarnings = FALSE)

#-------------------------------------------------------------------------------#
# Creación de archivos individuales ---------------------------------------
#-------------------------------------------------------------------------------#
# Apertura del archivo de control
#................................................................................
#  1 Abrir archivo de texto de control de Monolix.
#  2 Modificar el archivo para ser leído como una string de R.
#  3 Reemplazar el nombre de archivo a leer por el control de Monolix por
#  el correspondiente en cada carpeta.
#  Se cambian los valores de parámetro inicial, se conserva la misma semilla
#  de simulación para cada iteración.
#  4 Crear carpetas para contener los archivos.
#  5 Almacenar el archivo en el directorio correspondiente con el nombre
#  de "BASE_NEW.mlxtran", con formato de texto.
#................................................................................
fileName <- file.path(dirModeloBase, 'M2CPTM_nobs_1_prop.mlxtran')
Z = readChar(fileName, file.info(fileName)$size)

global_task <- glue(
  "populationParameters()\r\n",
  "individualParameters(method = {conditionalMean})\r\n",
  "fim(run = false,method = StochasticApproximation)\r\n",
  "logLikelihood(method = ImportanceSampling)\r\n",
  "plotResult(method = likelihoodresults)\r\n\r\n",
  .open='<<', .close='>>'
)

global_conf <- glue(
  "burniniterations = 0\r\n",
  "smoothingiterations = 0\r\n",
  "exploratoryiterations = 0\r\n",
  "simulatedannealingiterations = 0\r\n",
  "exploratoryinterval = 0\r\n",
  "smoothinginterval = 0\r\n\r\n",
  "LL:\r\n",
  "fixedsimulations = 30000\r\n\r\n"
)

Z1 <- Z %>% 
  str_replace(regex(
    '(?<=\\[TASKS\\]\\r\\n).+(?=\\[SETTINGS\\])', dotall=TRUE
    ), global_task) %>%
  str_replace_all('data/(?=1_data)', '../')  %>%
  # Lee el archivo de datos en el mismo directorio
  str_replace("data/data_TAD.csv", "data_TAD.csv") %>% 
  # Reemplazar en [SETTINGS] las configuraciones del algoritmo SAEM
  str_replace(
    regex(
      '(?<=POPULATION\\:\\r\\n).+(?=\\r\\n\\r\\n\\[COMMENTS\\])', dotall=TRUE
    ), global_conf)

# Creación de directorios para cada item
for (i in 1:length(pop_vector)) {
  dir.create(file.path('results', par_eval, paste0('A', i)), showWarnings = FALSE)
}

# Copiar el archivo de datos desde el modelo Base
for (i in 1:length(pop_vector)) {
  file.copy(
    from = file.path(dirModeloBase, 'data', 'data_TAD.csv'),
    to = file.path('results', par_eval, paste0('A', i))
  )
}


for (i in 1:length(pop_vector)) {
  Y <- Z1 %>%
    str_replace(
      glue("(?<={{par_eval}}\\s\\=\\s\\{value\\=)\\d+\\.\\d+(?=\\,\\smethod)",
           .open='{{', .close='}}'),
      paste(pop_vector[i])
    )
  
  # Escribir archivo de control SAEM con cambio de parámetro seleccionado
  write_lines(Y,
              file.path('results', par_eval, paste0('A', i),
                        'M2CPTM_nobs_1_prop.mlxtran'),
              sep = '\n')
}

