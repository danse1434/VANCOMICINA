# Directorio principal
parent.folder <- file.path('.')
# Lista con todos los subdirectorios
sub.folders <- list.dirs(parent.folder, recursive = TRUE)[-1]
# Seleccionar todos los archivos que contienen el patrón
pattern = "script_graficos.+R$"
scripts <- dir(sub.folders, pattern, full.names = TRUE)

for (i in scripts) {
  print(i)
  source(i, encoding = 'UTF-8')
}
