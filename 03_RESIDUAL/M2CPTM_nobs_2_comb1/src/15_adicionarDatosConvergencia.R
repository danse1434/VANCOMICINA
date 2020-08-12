# Carga de paquetes 
require(glue)
require(tidyverse)
require(lixoftConnectors) # API Monolix
initializeLixoftConnectors(software="monolix")

#-------------------------------------------------------------------------------#
# Directorios de trabajo
auxdir  <- file.path('.', 'M2CPTM_nobs_2_comb')
bootdir <- file.path(auxdir, 'results', 'boot')

# Asignación de lambda y ejecución
for (i in 1:1000) {
# Ruta a directorio de trabajo
dis <- glue('{bootdir}/B{i}')
# Ruta al archivo de control
filename <- file.path(dis, 'M2CPTM_nobs_2_comb.mlxtran')
# Lectura y asignación de archivo de control en envir.
A = readChar(filename, file.info(filename)$size) 
# Modificar el archivo de control
# Se adicionan gráficos con observaciones
B <- A %>%
  str_replace_all(pattern = "plotResult\\(method\\s\\=\\s\\{saemresults\\}\\)",
                  replacement = 'plotResult(run = false, method = {saemresults})')

# Sobrescribir el archivo de control
write_lines(B, filename)

# Carga proyecto con API
loadProject(projectFile = filename)
# Computar datos de gráficos
computeChartsData()
# Mensaje Terminación
print(glue("Lista la tarea {i}!"))
}
