##------------------------------------------------------------------------------#
## Nombre del Script: Calculo indicador PK/PD: AUC/MIC > 400 -------------------
##  
## Propósito del Script: Cálculo indicador PK/PD - AUC/MIC >= 400
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  15-03-2021
##  
## Copyright (c) Daniel S. Parra, 2021 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# 1. Introducción -----------------------------------------------------
#-------------------------------------------------------------------------------#

# Cargar paquetes
require(data.table)
require(progress)
require(scales)

source(file.path('src', '069_paquetesSimulacion.R'), encoding = 'UTF-8')
source(file.path('src', '081_fun_CalculoIndicadores.R'), encoding = 'UTF-8')
source(file.path('src', '083_fun_GraficosPerfil.R'), encoding = 'UTF-8')

outexposure <- c(
  '011_exp_CLCR90_',
  '021_exp_CLCR100_',
  '031_exp_CLCR110_',
  '041_exp_CLCR120_',
  '051_exp_CLCR130_',
  '061_exp_CLCR150_'
)

#-------------------------------------------------------------------------------#
# 2. Cálculo indicador -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Vector con MIC etiquetas principales y secundarias
MIC_vec_0 = c(1 * (2 ^ (seq(-4, 6, 1))))
MIC_vec_1 = c(1 * (2 ^ (seq(-4, 6, length = 1e2))))

dataLS <- list()

# Seguimiento de tiempo
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent) eta: (:eta)",
                       total = length(outexposure))

x <- fread(file.path('results', paste0(outexposure[1], 1, '.csv')))







resPTA2_Tabla <- function(data, parameter, pkpd) {
  # pq <- rlang::ensym(parameter)
  
  data[, pt := map_dbl(parameter, ~pkpd)]
  return(sum(xdata$pt))
}

# ifelse(pkpd, 1, 0)

resPTA2_Tabla(x, 'cmin', function(x) {ifelse(x >= 15 & x < 20, 1, 0)})



for (k in seq_len(length(outexposure))) {
  pb$tick()
  
  dataDF <- data.table()
  
  for (i in 1:35) {
    data0 <- fread(file.path('results', paste0(outexposure[k], i, '.csv')))
    # Aclaramiento
    data1 <- resPTA_Tabla(data0, MIC_vec_1, resPTA1_AUC, crit = 400, g = i)
    
    dataDF <- rbind(dataDF, data1)
    rm(data0, data1)
  }
  
  dataLS[[k]] <- dataDF
  rm(dataDF)
}

# Vector con valores de aclaramiento
CLCR <- data.table(Tipo = 1:6,
                   CLCR = c(90, 100, 110, 120, 130, 150))

admDF <- fread(file.path('data', 'adm_list.csv'))

dataTotal <- map_df(dataLS, ~.x, .id = 'Tipo')

dataTotal <- dataTotal[, Tipo := as.integer(Tipo)] %>% 
  .[CLCR, on = .(Tipo)] %>% 
  .[admDF, on = .(g = ID)]

fwrite(dataTotal, file = file.path('results', 'datosFunRenal.csv'))

#-------------------------------------------------------------------------------#
# 3. Gráficos -----------------------------------------------------
#-------------------------------------------------------------------------------#

graficoPTA_AUC(dataTotal, MIC_vec_1, MIC_vec_0, color = CLCR, format = 'P') + 
  facet_grid(DD ~ ii)

