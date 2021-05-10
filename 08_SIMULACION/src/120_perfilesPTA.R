################################################################################-
#' --- 
#' title: 'Creación de perfiles plasmáticos simulados' 
#' author: 'Daniel S. Parra G.' 
#' date: '01-01-2021' 
#' --- 
## Propósito del Script: Crear perfiles plasmáticos con los regímenes de 
## dosificación creados.
## 
## 
## Copyright (c) Daniel S. Parra G., 2021 
## 
## Email: dsparra@minsalud.gov.co 
################################################################################-

require(data.table)
require(progress)
require(scales)

source(file.path('src', '069_paquetesSimulacion.R'), encoding = 'UTF-8')
source(file.path('src', '081_fun_CalculoIndicadores.R'), encoding = 'UTF-8')

#'-------------------------------------------------------------------------------
# 1. Simulación de perfiles plasmáticos ------------------
#'-------------------------------------------------------------------------------

outputfile <- '002_out_desconocido'

dataLS <- list()

for (i in 1:35) {
  data0 <- fread(file.path('results', paste0(outputfile, i, '.csv')))
  data0[, G := i]
  # data1 <- resPTA_Tabla(data0, MIC_vec_1, resPTA1_AUC, crit = 400, g = i)
  
  dataLS[[i]] <- data0
}

admDF <- fread(file.path('data', 'adm_list.csv'))

# Creación de perfiles plasmáticas

df <- map_df(dataLS, ~.x)[admDF, on = .(G = ID)] %>%
  setnames(., 'ii', 'II')

G1 <- df %>% 
  ggplot(aes(x = time, group = G)) + 
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = factor(tinf)), alpha = 0.3) +
  geom_line(aes(y = Q2, color = factor(tinf))) + 
  xlab('Tiempo (hr)') + ylab(expression(C[PRED]~(mg/L))) +
  facet_grid(DD ~ II, labeller = labeller(.rows = label_both, .cols = label_both)) + 
  scale_color_manual(values = c('red', 'green3', 'blue'), name = 'Tinf') + 
  scale_fill_manual(values = c('red', 'green3', 'blue'), name = 'Tinf') + 
  labs(title = 'Simulación Vancomicina SS', subtitle = 'CLCR: Desconocido')

G1

ggsave('050_perfilPlasmaticoCrCldesconocido.pdf', G1, 'pdf', 'figures', 1, 8, 6)
