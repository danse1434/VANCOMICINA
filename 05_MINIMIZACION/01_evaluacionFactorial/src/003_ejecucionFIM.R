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
monolix2019R2.path <-  "C:/ProgramData/Lixoft/MonolixSuite2019R2"
require(lixoftConnectors, lib.loc = monolix2019R2.path )
# require(mlxR)
# initMlxR(path = monolix2019R2.path)   #(Adapte si es necesario).
initializeLixoftConnectors()
require(glue)
require(tidyverse)
require(progress)

# Selección directorio auxiliar
aux_dir <- file.path(getwd(), 'evaluacion')

# Modificación de 
for (i in 1:81) {
  subDir = glue('{aux_dir}/E{i}/Base_corr2.mlxtran')
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

N = 81
ptm <- proc.time()

pb <-
  progress_bar$new(total = N, format = "[:bar] :percent :percent in :elapsed eta: :eta")

for (i in 1:N) {
  pb$tick()
  loadProject(glue('{aux_dir}/E{i}/Base_corr2.mlxtran'))
  runPopulationParameterEstimation()
  runStandardErrorEstimation()
  scenario = getScenario()
  scenario$plotList = c("saemresults")
  setScenario(scenario)
  computeChartsData()
  
  # print(glue::glue("Lista la carpeta {i}"))
}

print(proc.time() - ptm)
