##------------------------------------------------------------------------------#
## Nombre del Script: Creación de data sets con instrucciones de bootstrap ------
## y almacenamiento en carpetas correspondientes
##  
## Propósito del Script: creación de muestreos sin reemplazo para obtener 
## estimación de intervalos de confianza de parámetros poblacionales.  
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  12-02-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# Introducción -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Carga de paquetes
require(rlang)
require(tidyverse)

#-------------------------------------------------------------------------------#
rundir    <- "run200"
bootdir   <- file.path(rundir, 'boot_results')
modeldir  <- file.path(".")
modelfile <- str_glue("{rundir}.mlxtran")

dir.create(file.path(bootdir))

#-------------------------------------------------------------------------------#
# Generación de subdirectorios de trabajos
set.seed(12356)

df_data_TAD <- read_csv(file.path('data', 'data_TAD.csv'), na = '.')

data_ls <- df_data_TAD %>%
  filter(YTYPE == 2 | is.na(YTYPE)) %>% 
  group_by(ID) %>%
  nest()

table_ls <- vector(mode = 'list', 1000L)

for (i in 1:1000) {
  table_ls[[i]] <- data_ls %>% 
    ungroup() %>% 
    sample_n(14, replace = TRUE) %>% 
    add_column(new_ID = 1:14) %>% 
    unnest(cols = c(data))
}

for (i in 1:1000) {
  
  id <- sprintf("%03d", i)
  
  dir.create(file.path(bootdir, str_glue('B{id}')), showWarnings = FALSE)
  
  write_csv(x = table_ls[[i]],
    file = file.path(bootdir, str_glue("B{id}"), str_glue('data_{id}.csv')),
    na = '.', progress = FALSE)
}

for (i in 1:1000) {
  id <- sprintf("%03d", i)
  
  file.copy(file.path(modeldir, str_glue('{rundir}.mlxproperties')),
            file.path(bootdir, str_glue('B{id}')))
}

#-------------------------------------------------------------------------------#
# Apertura del archivo de control

model_read_char <- readChar(modelfile, file.info(modelfile)$size)


#' Configuración de resultados para correr cada muestreo de bootstrap no paramétrico
tasks <- paste(
  "\r\npopulationParameters()",
  "plotResult(method = {saemresults})",
  "individualParameters(run = false,method = none )",
  "fim(run = false,method = StochasticApproximation)",
  "logLikelihood(run = false,method = ImportanceSampling)",
  "plotResult(run = false, method = none)",
  "\r\n",
  sep = '\r\n'
)

settings <- paste('\r\nGLOBAL\\:',
                  "exportpath = 'run200'",
                  "nbchains = 50\r\n", sep = '\r\n') 

#' 
#' Selección de parámetros de última estimación

fx_parameters <- read.csv(file.path(rundir, "populationParameters.txt")) %>% 
  filter(!str_detect(parameter, "(omega|^b$)")) %>% 
  {setNames(round(.$value, 2), .$parameter)}


# Modificación de efectos aleatorios para mejorar la estimación SAEM
# Con esto se modifican los valores iniciales se dejan en 1.00 y esto permite una 
# mejor búsqueda en el espacio muestral. 

model_read_char_modified <- model_read_char %>%
  str_replace('(?<=ANTU)\\}', ', new_ID}') %>%
  str_replace('ID(?=\\s\\=\\s)', 'new_ID') %>%
  str_replace_all('(?<=b\\s\\=\\s\\{value\\=)\\d\\.\\d+(?=\\,)', '0.3') %>%
  
  str_replace_all('(?<=omega_(Cl|Q|V1|V2)\\s\\=\\s\\{value\\=)(\\-0|.)\\.\\d+(?=\\,)',
                  '1.50') %>%
  str_replace_all("(?<=file\\s\\=\\s)\\'(?=model\\/model\\.txt\\')",
                  "'../../../") %>%
  str_replace_all('data\\/', '') %>%
  # Selección de tarea a ejecutar en archivo de control
  str_replace(
    regex("(?<=\\[TASKS\\]).+(?=\\[SETTINGS\\])", dotall = TRUE),
    tasks
  ) %>% 
  str_replace(
    regex("(?<=\\[SETTINGS\\]).+(?=\\[COMMENTS\\])", dotall = TRUE),
    settings
  )

for (i in seq_along(fx_parameters)) {
  escaped_char <- gsub("(\\_)", "\\\\\\_", names(fx_parameters)[i])
  escaped_char <- paste0("(?<=", escaped_char, "\\s\\=\\s\\{value\\=)[\\-\\.\\d]+(?=\\,)")
  
  model_read_char_modified <- model_read_char_modified %>% 
    str_replace_all(escaped_char, as.character(fx_parameters[i]))
}

for (i in 1:1000) {
  id <- sprintf("%03d", i)
  
  model_read_final <- model_read_char_modified %>%
    str_replace("(?<=\\_)TAD", str_glue('{id}'))
  
  write_lines(model_read_final,
              file.path(bootdir, str_glue('B{id}'), 'run200.mlxtran'), sep = '\n')
}
