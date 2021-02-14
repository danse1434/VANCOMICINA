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
dirModeloBase <- file.path(".")

# Leer archivo *csv con parámetros de modelo base
parametrosPoblacionales <-
  read_csv(file.path(dirModeloBase,
                     'modeloFinal',
                     'populationParameters.txt'))

#-------------------------------------------------------------------------------#
# Selección de parámetros a evaluar
par_eval1 = 'Cl_pop'
par_eval2 = 'beta_Cl_tCLCRMLMIN'

#-------------------------------------------------------------------------------#
# Cálculo de valor mínimo (50%) y valor máximo (150%) respecto al valor 
# nominal estimado en el modelo base.
pop_par <- 
  parametrosPoblacionales %>% 
  filter(parameter %in% c(par_eval1, par_eval2)) %>% 
  pull(value) 

pop_vector_1 <- pop_par[1] * seq(0.5, 5.0, length.out = 30)
pop_vector_2 <- pop_par[2] * seq(0.5, 5.0, length.out = 30)

# Malla expandida con valores de referencia para los parámetros
pop_vec_df <- expand.grid(pop_vector_1, pop_vector_2)

aux_dir <- file.path('results', glue('{par_eval1}_{par_eval2}'))

# Preparación de carpetas
dir.create(file.path(aux_dir), showWarnings = FALSE)

#-------------------------------------------------------------------------------#
# Creación de archivos individuales ---------------------------------------
#-------------------------------------------------------------------------------#
# Apertura del archivo de control
#................................................................................
##  1 Abrir archivo de texto de control de Monolix.
##  2 Modificar el archivo para ser leído como una string de R.
##  3 Reemplazar el nombre de archivo a leer por el control de Monolix por
##  el correspondiente en cada carpeta.
##  Se cambian los valores de parámetro inicial, se conserva la misma semilla
##  de simulación para cada iteración.
##  4 Crear carpetas para contener los archivos.
##  5 Almacenar el archivo en el directorio correspondiente con el nombre
##  de "BASE_NEW.mlxtran", con formato de texto.
#................................................................................
fileName <- file.path(dirModeloBase, 'modeloFinal.mlxtran')
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
  "fixedsimulations = 5000\r\n\r\n"
)

# Quitar tareas innecesarias, sólo estimación de parámetros
Z1 <- Z %>% 
  str_replace(regex(
    '(?<=\\[TASKS\\]\\r\\n).+(?=\\[SETTINGS\\])', dotall=TRUE
  ), global_task) %>%
  str_replace_all('data/(?=1_data)', '../')  %>%
  # Lee el archivo de datos en el mismo directorio
  str_replace("data/data_TAD.csv", "../../../data/data_TAD.csv") %>% 
  str_replace_all('model/model.txt', '../../../model/model.txt')  %>%
  # Reemplazar en [SETTINGS] las configuraciones del algoritmo SAEM
  str_replace(
    regex(
      '(?<=POPULATION\\:\\r\\n).+(?=\\r\\n\\r\\n\\[COMMENTS\\])', dotall=TRUE
    ), global_conf)

str1 <- glue('(?<={{par_eval1}}\\s\\=\\s\\{value\\=)(\\d+\\.\\d+|\\d)(?=\\,\\smethod)',
             .open='{{', .close='}}')
str2 <- glue('(?<={{par_eval2}}\\s\\=\\s\\{value\\=)(\\d+\\.\\d+|\\d)(?=\\,\\smethod)',
             .open='{{', .close='}}')

# Creación de directorios para cada item
for (i in 1:dim(pop_vec_df)[1]) {
  dir.create(file.path(aux_dir, glue('A{i}')), showWarnings = FALSE)
}

for (i in 1:dim(pop_vec_df)[1]) {
  Y <- Z1 %>%
    str_replace(str1,
                paste(pop_vec_df[i, 1])) %>%
    str_replace(str2,
                paste(pop_vec_df[i, 2]))
  
  write_lines(Y,
              file.path(aux_dir, glue('A{i}'), 'modeloFinal.mlxtran'),
              sep = '\n')
}
