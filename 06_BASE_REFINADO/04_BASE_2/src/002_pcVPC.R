##------------------------------------------------------------------------------#
## Nombre del Script: Script de Generación de VPC  ------------------
##  
## Propósito del Script: crear gráficos de chequeo predictivo visual (VPC) 
## mediante simulación por medio del paquete MlxR
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 10-01-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
require(data.table)
require(tidyverse)
require(rlang)
require(mlxR)
require(progress)

#-------------------------------------------------------------------------------#
# 1. Abrir / Modificar archivos de datos con observaciones ------------
#-------------------------------------------------------------------------------#
data_TAD <- 
  file.path('data', 'data_TAD.csv') %>% 
  mlxR::readDatamlx(datafile = ., 
                    header = c("id", "y", "time", "ytype", "evid", "mdv",	"amt", 
                               "tinf", "addl", "ii", "ss", 
                               rep('ignore', 10)), nbSSDoses = 5)

popParam <- read_csv(file.path('data', 'processed', 'populationParameters.csv'))

popParam <- popParam %>%
  mutate(mn = ifelse(str_detect(parameter, 'omega'), mn, mn))

param <- setNames(popParam$mn,
                  c('Cl_pop', 'Q_pop', 'V1_pop', 'V2_pop',
                    'omega_Cl', 'omega_Q', 'omega_V1', 'omega_V2', 'b'))

out1  <- list(name = 'y1', time = seq(0, 12, length.out = 1e3))
out2  <- list(name = 'Cc', time = seq(0, 12, length.out = 1e3))

data_list <- vector(mode = "list", length = 1000)

ptm <- proc.time()
pb <- progress_bar$new(total = 1000)

for (i in 1:1e3) {
  pb$tick()
  
  data_list[[i]] <- simulx(
    parameter = param,
    treatment = data_TAD$treatment, 
    model = file.path('models', 'modeloBase_1.txt'),
    output = list(out1),
    settings = list(load.design=FALSE)
    )[['y1']]
  
}
print(proc.time() - ptm)

#-------------------------------------------------------------------------------#
# -----------------------------------------------------
#-------------------------------------------------------------------------------#

dfr_percs <- 
  data_list %>%
  map_dfr(~as_tibble(.x), .id = 'design')

setDT(dfr_percs)

dfr_percs[, gr:= ntile(time, 100), by=.(design)]

dfr_percs_1 <- dfr_percs[, .(
  TIME = mean(time),
  ME = quantile(x = y1, probs = 0.50),
  LI = quantile(x = y1, probs = 0.10),
  LS = quantile(x = y1, probs = 0.90)
), by = .(design, gr)]

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
), by = .(gr)]

#-------------------------------------------------------------------------------#
# -----------------------------------------------------
#-------------------------------------------------------------------------------#
dataOBS1 <- data_TAD$y1 %>% 
  mutate(gr = ntile(time, 7)) %>% 
  group_by(gr) %>% 
  summarise(TIME = mean(time), 
            ME = quantile(x = y1, probs = 0.50), 
            LI = quantile(x = y1, probs = 0.10),
            LS = quantile(x = y1, probs = 0.90),
            n = n())

# Función que adiciona lineas y puntos, diseñada para observaciones
linedots <- function(data, x, y) {
  x = rlang::ensym(x)
  y = rlang::ensym(y)
  return(
    list(geom_line(data = data, aes(x = !!x, y = !!y), col='red'),
         geom_point(data = data, aes(x = !!x, y = !!y), col='red', shape=15))
  )
}

# VPC con percentiles predichos
theme_set(theme_bw() +
            theme(panel.border = element_rect(fill = NULL, colour = 'black')))

dfr_percs_2 %>%
  ggplot(aes(x = TIME)) +
  geom_ribbon(aes(ymin = ME_li, ymax = ME_ls), fill=alpha('gray70', 0.5)) +
  geom_ribbon(aes(ymin = LI_li, ymax = LI_ls), fill=alpha('gray30', 0.5)) +
  geom_ribbon(aes(ymin = LS_li, ymax = LS_ls), fill=alpha('gray30', 0.5)) +
  geom_point(data=data_TAD$y1, aes(x=time, y=y1)) +
  linedots(dataOBS1, TIME, ME) +
  linedots(dataOBS1, TIME, LI) +
  linedots(dataOBS1, TIME, LS) +
  geom_line(aes(y = ME_me), lty='dashed') +
  geom_line(aes(y = LI_me), lty='dashed') +
  geom_line(aes(y = LS_me), lty='dashed') +
  theme_bw()





