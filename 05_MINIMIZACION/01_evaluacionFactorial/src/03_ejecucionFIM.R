##------------------------------------------------------------------------------#
## Nombre del Script: Ejecución de algoritmo FIM para SE de estimaciones -------
##  
## Propósito del Script:  calcular FIM para SE en estimaciones
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  22-08-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
require(glue)
require(tidyverse)
require(lixoftConnectors)
initializeLixoftConnectors()

# Selección directorio auxiliar
aux_dir <- file.path(getwd(), 'evaluacion')

# Modificación de 
for (i in 1:81) {
  subDir = glue('{aux_dir}/evaluacion{i}/M2CPTM_nobs_1_prop.mlxtran')
  A = readChar(subDir, file.info(subDir)$size)
  # Modificar el archivo de control - adicionar gráficos con observaciones
  B <- A %>%
    str_replace_all(
      "(?<=populationParameters\\(\\))\\r\\n",
      '\r\nplotResult(run = false, method = {saemresults})\r\n'
    )
  # Sobrescribir el archivo de control
  write_lines(B, subDir)
}

# # Eliminación de carpetas para compatibilidad proyecto
# for (i in 1:10) {
#   subdir = glue('{aux_dir}/evaluacion{i}/M2CPTM_nobs_1_prop')
#   unlink(glue('{subdir}/FisherInformation'), TRUE, TRUE)
#   unlink(glue('{subdir}/IndividualParameters'), TRUE, TRUE)
#   unlink(glue('{subdir}/Tests'), TRUE, TRUE)
# }

for (i in 6:81) {
  loadProject(glue('{aux_dir}/evaluacion{i}/M2CPTM_nobs_1_prop.mlxtran'))
  setStandardErrorEstimationSettings(minIterations = 500, maxIterations = 10000)
  runStandardErrorEstimation()
  computeChartsData()
  print(glue::glue("Lista la carpeta {i}"))
}
