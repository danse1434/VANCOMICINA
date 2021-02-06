##------------------------------------------------------------------------------#
## Nombre del Script: Script de generación de VPC -------------------------------
##  
## Propósito del Script: crear gráficos de chequeo predictivo visual mediante  
##  simulación por medio del paquete MLXR de Monolix. 
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 18-02-2020 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# 1. Introducción -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Carga de paquetes
monolix2019R2.path <-  "C:/ProgramData/Lixoft/MonolixSuite2019R2"
require(lixoftConnectors, lib.loc = monolix2019R2.path )
require(mlxR)
initMlxR(path = monolix2019R2.path)   #(Adapte si es necesario).
require(tidyverse)
require(progress)
require(rlang)
require(data.table)

#-------------------------------------------------------------------------------#
auxdir <- file.path('.', 'BASE')
project.file <- file.path('BASE.mlxtran')
paramet.file <- file.path('BASE')
dir.create(file.path(auxdir, 'figures'))
dir.create(file.path(auxdir, 'figures', 'RDS'))

source(file.path('src', '052_fun_pcVPC.R'), encoding = 'UTF-8')

#-------------------------------------------------------------------------------#
# Apertura y modificación de archivo de datos con observaciones
#-------------------------------------------------------------------------------#
#' 1 Abrir archivo de observaciones TAD
#' 2 Filtrar sólo eventos de observación (EVID == 0)
#' 3 Renombrar columnas para armonizar con archivo de simulación
#' 4 Cambiar el tipo de columna para ID a factor
#' 5 Reasignar la variable data_TAD como una nueva variable tipo lista 
#' para el comando SimulX.
#................................................................................

# Lectura de archivo de observaciones
data_TAD <- read_delim(
  file.path('data', 'data_TAD.csv'),
  ",", escape_double = FALSE, locale = locale(), 
  trim_ws = TRUE, na = ".")

data_OBS <- data_TAD %>% 
  filter(EVID == 0) %>% 
  rename(time = TAD, id = ID, y = DV) %>% 
  mutate(id = factor(id))

data_TAD <-
  file.path('data', 'data_TAD.csv') %>% 
  mlxR::readDatamlx(datafile = ., 
                    header = c('id', 'y', 'time', 'ytype', 'evid', 'mdv',
                               'amt', 'tinf', 'addl', 'ii', 'ss', 
                               rep('ignore', 11)))

#-------------------------------------------------------------------------------#
# 2. Simulación de diseños del estudio ---------------------------------------
#-------------------------------------------------------------------------------#
# El objetivo es simular N set de datos con el diseño de dosis y 
# covariables original. Con esto se generan varios modelos teóricas que se 
# deben comparar con las observaciones empíricas.
#................................................................................
#  1 Seleccionar el archivo con el Modelo de Monolix obtenido, este fue 
#  modificado con un archivo de datos con TAD en lugar de TSFD. 
#  2 Abrir el archivo de parámetros poblacionales
#  3 Asignar los nombres a un vector
#  4 Ajustar las observaciones para tener 1000 puntos entre el intervalo 
#  de dosificación de 0 a 8 horas, almacenar esta configuración en una lista.
#  5 Configurar un vector con parámetros a simular. 
#  6 Pre-localizar un vector con 1000 posiciones.
#  7 Colocar en cada posición del vector un data.frame con simulaciones 
#  originadas a partir del diseño presente en el set de datos original.
#  8 Realizar la simulación teniendo en cuenta el archivo de proyecto con 
#  *simulx*. Elaborar una lista con 1000 tablas que contienen las simulaciones. 
# Se demora 6 minutos aproxim.
#................................................................................
# Parámetros poblacionales
param <-
  read_csv(file.path(paramet.file, "populationParameters.txt"), 
           col_types = cols()) %>%
  select(-se_sa, -rse_sa) %>%
  column_to_rownames(var = "parameter")

N = 5e2 # N.° diseños a simular
param <- setNames(pull(param), as.character(rownames(param)))
out1  <- list(name = 'y1', time = seq(0, 12, length.out = 1e3))
out2  <- list(name = 'y2', time = seq(0, 12, length.out = 1e3))

data_list_1 <- vector(mode = "list", length = N)
data_list_2 <- vector(mode = "list", length = N)


ptm <- proc.time()

pb <-
  progress_bar$new(total = N, format = "[:bar] :percent :percent in :elapsed eta: :eta")

for (i in 1:N) {
  pb$tick()
  
  resultados <- simulx(
    project = project.file,
    output = list(out1, out2), 
    settings = list(load.design=FALSE, digits=3)
  )
  
  data_list_1[i] <- resultados['y1']
  data_list_2[i] <- resultados['y2']
  
  # print(paste('Lista la interacción N.º: ', i))
}
print(proc.time() - ptm)
# Demora 6 minutos

#-------------------------------------------------------------------------------#
# 3. Cálculo predicciones PRED -----------------------------------------------------
#-------------------------------------------------------------------------------#
#' El objetivo es simular un set de datos con el diseño de dosis y 
#' covariables original. Este set de datos se comparte en todas las simulaciones 
#' ya realizadas por que son predicciones poblacionales. Los parámetros de 
#' variabilidad se ajustan a cero para reflejar el valor esperado de acuerdo 
#' al diseño y el tiempo seleccionado.
#................................................................................
#  1 Ajustar los parámetros de variabilidad a cero.
#  2 Realizar la simulación teniendo en cuenta el archivo de proyecto con 
#  *simulx*. Este consiste de una sola tabla que se compartirá en todas 
#  las tablas de data_list, y se trata de simulaciones poblacionales (PRED).
#................................................................................
sim.param <- c(
  omega_Cl = 0,
  omega_V1 = 0,
  omega_Q  = 0,
  omega_V2 = 0,
  a1       = 0, 
  a2       = 0
)

data_pred <- simulx(
  project = project.file,
  parameter = sim.param,
  treatment = data_TAD$treatment,
  output = list(out1, out2)
)

#-------------------------------------------------------------------------------#
#' ** Calcular mediana de PRED en el perfil **
#' 1. Cambiar a datatable
#' 2. Crear 100 bins a partir del tiempo *gr*
#' 3. Calcular el tiempo medio en cada *bin*
#' 4. Calcular la mediana en cada bin *ME_bin*
#' 5. Almacenar como **data_pred_1**

data_pred <- rbind(
  data_pred$y1 %>% rename(y = y1) %>% add_column(YTYPE = 1),
  data_pred$y2 %>% rename(y = y2) %>% add_column(YTYPE = 2)
) %>% as.data.table(.)

data_pred_1 <- data_pred %>%
  .[, gr := ntile(time, 100)] %>%
  .[, .(time_bin = mean(time),
        ME_bin = quantile(x = y, probs = 0.50)), by = .(gr)]

data_pred[data_pred_1, on = 'gr', 
          `:=`(time     = round(time, 3),
               time_bin = round(i.time_bin, 3),
               ME_bin   = i.ME_bin)]

#-------------------------------------------------------------------------------#
# 4. Transformación de Simulaciones -----------------------------------
#-------------------------------------------------------------------------------#
#' Calcular IC del 95% para los percentiles de predicción para el conjunto de 
#' simulaciones de diseños (dentro de cada posición de la lista). 
#................................................................................
#' 1- Convertir la lista *data_list* en un dataFrame único *dfr_percs*
#' 2- Convertir *dfr_percs* en DataTable
#' 3- Calcular grupos basados en 7 bins de la variable _time_
#' 4- Convertir las variables _time_ y _y2_ a 3 dígitos
#' 5- Agrupar las predicciones **data_pred** por izquierda basados en id, gr, y 
#' tiempo. Adicionar columnas por referencias y cambiar a nombres
#' 6- Calcular conc. corregidas por pred (*pc_y2*), teniendo en cuenta las 
#' concentraciones originales (*y2*), predicciones poblacionales (*PRED*), y 
#' mediana para la predicción en el bin correspondiente (*PRED_bin*).
#................................................................................

options(dplyr.summarise.inform = FALSE) # Elimina unos mensajes molestos

dfr_percs_1 <- data_list_1 %>% map_dfr(~.x, .id = 'design')
dfr_percs_2 <- data_list_2 %>% map_dfr(~.x, .id = 'design')

rm(data_list_1, data_list_2)

setDT(dfr_percs_1); setnames(dfr_percs_1, 'y1', 'y')
setDT(dfr_percs_2); setnames(dfr_percs_2, 'y2', 'y')

dfr_percs_1[, `:=`(time = round(time, 3), y = round(y, 3))]
dfr_percs_2[, `:=`(time = round(time, 3), y = round(y, 3))]

dfr_percs <- rbind(dfr_percs_1[, YTYPE := 1],
                   dfr_percs_2[, YTYPE := 2])

rm(dfr_percs_1, dfr_percs_2)

dfr_percs %>%
  .[, gr := ntile(time, 100), by = .(YTYPE, design)] %>%
  .[, `:=`(time = round(time, 3), y = round(y, 3))] %>%
  .[data_pred, on = c('YTYPE', 'id', 'gr', 'time'), `:=`(PRED = i.y, PRED_bin = i.ME_bin)] %>%
  .[, pc_y := y * PRED_bin / PRED]

#................................................................................
#' 1- Crear una tabla con resumen de **dfr_percs** con media de tiempo, medianas, 
#' percentiles 10 y 90 de simulaciones, para cada bin y cada simulación de diseño.
#................................................................................

dfr_percs_1 <- dfr_percs %>% 
  .[, .(
    TIME = mean(time),
    ME   = quantile(x = pc_y, probs = 0.50),
    LI   = quantile(x = pc_y, probs = 0.10),
    LS   = quantile(x = pc_y, probs = 0.90)
  ), by = .(YTYPE, design, gr)]

#................................................................................
#' 1- Crear una tabla de resumen de **dfr_percs_1** con intervalo de confianza 
#' (90%) y mediana para cada uno de los percentiles de simulación, para cada bin.
#................................................................................

dfr_percs_2 <- dfr_percs_1[, .(
  TIME = mean(TIME),
  ME_me = quantile(ME, probs = 0.50),
  ME_li = quantile(ME, probs = 0.05),
  ME_ls = quantile(ME, probs = 0.95),
  
  LI_me = quantile(LI, probs = 0.50),
  LI_li = quantile(LI, probs = 0.05),
  LI_ls = quantile(LI, probs = 0.95),
  
  LS_me = quantile(LS, probs = 0.50),
  LS_li = quantile(LS, probs = 0.05),
  LS_ls = quantile(LS, probs = 0.95)
), by = .(YTYPE, gr)]

#-------------------------------------------------------------------------------#
# 5. Transformación de observaciones --------------
#-------------------------------------------------------------------------------#
# Crear predicciones de acuerdo al diseño definido para cada ID

data_OBS_PRED <- simulx(
  project = project.file,
  parameter = sim.param,
  treatment = data_TAD$treatment
)

data_OBS_PRED <- rbind(
  data_OBS_PRED$y1 %>% rename(y = y1) %>% add_column(YTYPE = 1),
  data_OBS_PRED$y2 %>% rename(y = y2) %>% add_column(YTYPE = 2)
) %>% as.data.table(.)

#................................................................................
#' 1- Convertir *data_OBS_PRED* en data.table
#' 2- Renombrar a y2 como PRED ya que realmente representa esto
#' 3- Para las predicciones forma 7 bins basados en el tiempo
#' 4- Calcular la media de tiempo y la mediana de la predicción en cada bin.
#................................................................................
# Esta corresponde a las predicciones poblacionales
setDT(data_OBS_PRED)
setnames(data_OBS_PRED, old='y', new='PRED')

data_OBS_PRED %>%
  .[, `:=`(time = round(time, 2), gr = ntile(time, 7))] %>%
  .[, `:=`(time_bin = mean(time),
           PRED_bin = quantile(PRED, probs = 0.50)), by = .(gr)] 

#................................................................................
#' 1- Convertir el set de datos de observaciones en data.table
#' 2- Filtrar por YTYPE igual a 2
#' 3- Convertir el tiempo a dos dígitos
#' 4- Seleccionar id, y2, y tiempo
#................................................................................

# Observaciones originales
data_OBS_1 <- as.data.table(data_OBS) %>% 
  # .[ YTYPE == 2] %>% 
  .[, time := round(time, 2)] %>% 
  .[, .(id, y, time, YTYPE)]

# Adicionar observaciones originales
#................................................................................
#' 1- Seleccionar tabla de predicciones 
#' 2- Unir la tabla de observaciones originales y adicionar y2 por referencia
#' 3- Corregir observaciones a *pc_y2*
#' 4- Crear tabla de resumen con promedio de tiempo y percentiles de predicción 
#' del 90%
#................................................................................

data_OBS_PRED %>% 
  .[data_OBS_1, on = c('YTYPE', 'id', 'time'), y := i.y] %>% 
  .[, pc_y := y * PRED_bin/PRED]


data_OBS_PRED_sum <- data_OBS_PRED %>%
  .[, .(
    TIME = mean(time),
    ME   = quantile(x = pc_y, probs = 0.50),
    LI   = quantile(x = pc_y, probs = 0.10),
    LS   = quantile(x = pc_y, probs = 0.90)
  ), by = .(YTYPE, gr)]


#-------------------------------------------------------------------------------#
# 6. Creación de gráfico -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Selección de tema
theme_set(theme_bw() +
            theme(panel.border = element_rect(fill = NULL, colour = 'black')))

cambiarNombreTipo <- function(string) {
  ifelse(string == 1, 'Microbiológico', 'Quimioluminiscencia')
}

# Creación de gráfico
g_percs <-
  dfr_percs_2 %>%
  ggplot(aes(x = TIME)) +
  geom_ribbon(aes(ymin = ME_li, ymax = ME_ls), alpha = 0.5, fill = 'gray70') +
  geom_ribbon(aes(ymin = LI_li, ymax = LI_ls), alpha = 0.5, fill = 'gray30') +
  geom_ribbon(aes(ymin = LS_li, ymax = LS_ls), alpha = 0.5, fill = 'gray30') +
  geom_line(aes(y = ME_me), lty = 'dashed') +
  geom_line(aes(y = LI_me), lty = 'dashed') +
  geom_line(aes(y = LS_me), lty = 'dashed') +
  linedots(data_OBS_PRED_sum, TIME, ME) +
  linedots(data_OBS_PRED_sum, TIME, LI) +
  linedots(data_OBS_PRED_sum, TIME, LS) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  coord_cartesian(ylim = c(0, 60)) +
  facet_wrap(. ~ YTYPE, labeller = labeller(YTYPE = cambiarNombreTipo)) +
  xlab('TAD, tiempo tras dosis (h)') +
  ylab('Concentración plasmática VAN \n Corregida por predicción (mg/L)')

#-------------------------------------------------------------------------------#
# Almacenamiento en formato PDF
ggsave(file.path(auxdir, 'figures', '014_pcVPC_percentil.pdf'), g_percs, 'pdf',
       width = 6, height = 3.5)

# Almacenar RDS
saveRDS(g_percs, file.path(auxdir, 'figures', 'RDS', '012_pcVPC_percentil.rds'))
