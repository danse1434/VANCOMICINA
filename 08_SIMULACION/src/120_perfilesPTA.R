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

for (i in 1:49) {
  data0 <- fread(file.path('results', paste0(outputfile, i, '.csv')))
  data0[, G := i]
  # data1 <- resPTA_Tabla(data0, MIC_vec_1, resPTA1_AUC, crit = 400, g = i)
  
  dataLS[[i]] <- data0
}

admDF <- fread(file.path('data', 'adm_list.csv'))

# Creación de perfiles plasmáticas

df <- map_df(dataLS, ~.x)[admDF, on = .(G = ID)]

G1 <- df[df$DD %in% c(1:4*1000), ] %>% 
  ggplot(aes(x = time, group = G)) + 
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = factor(Tinf)), alpha = 0.3) +
  geom_line(aes(y = Q2, color = factor(Tinf))) + 
  xlab('Tiempo (hr)') + ylab(expression(C[PRED]~(mg/L))) +
  facet_grid(DD ~ II, labeller = labeller(.rows = label_both, .cols = label_both)) + 
  scale_color_manual(values = c('red', 'green3', 'blue'), name = 'Tinf') + 
  scale_fill_manual(values = c('red', 'green3', 'blue'), name = 'Tinf') + 
  labs(title = 'Simulación Vancomicina SS', subtitle = 'CLCR: Desconocido')

G1

ggsave('050_perfilPlasmaticoCrCldesconocido.pdf', G1, 'pdf', 'figures', 1, 8, 6)


#'-------------------------------------------------------------------------------
# 2. Perfil plasmático ------------------
#'-------------------------------------------------------------------------------
outexposure <- '001_exp_desconocido'

dataLS <- list()

for (i in 1:49) {
  data1 <- fread(file.path('results', paste0(outexposure, i, '.csv')))
  data1[, G := i]
  # data1 <- resPTA_Tabla(data0, MIC_vec_1, resPTA1_AUC, crit = 400, g = i)
  
  dataLS[[i]] <- data1
}

df1 <- map_df(dataLS, ~.x)[admDF, on = .(G = ID)]



G2 <- df1[df1$DD %in% c(1:4*1000), ] %>% 
  ggplot(aes(group = G)) +
  geom_density(aes(x = auc, color = factor(Tinf))) + 
  xlab('AUC (mg·h/L)') + 
  ylab('Densidad') +
  facet_grid(DD ~ II, labeller = labeller(.rows = label_both, .cols = label_both)) + 
  scale_color_manual(values = c('red', 'green3', 'blue'), name = 'Tinf') + 
  scale_fill_manual(values = c('red', 'green3', 'blue'), name = 'Tinf')

G2

G3 <- df1[df1$DD %in% c(1:4*1000), ] %>% 
  ggplot(aes(group = G)) +
  geom_density(aes(x = cmin, color = factor(Tinf))) + 
  xlab(expression(C[min]~(mg/L))) + 
  ylab('Densidad') +
  facet_grid(DD ~ II, labeller = labeller(.rows = label_both, .cols = label_both)) + 
  scale_color_manual(values = c('red', 'green3', 'blue'), name = 'Tinf') + 
  scale_fill_manual(values = c('red', 'green3', 'blue'), name = 'Tinf')
G3

G4 <- df1[df1$DD %in% c(1:4*1000), ] %>% 
  ggplot(aes(group = G)) +
  geom_density(aes(x = cmax, color = factor(Tinf))) + 
  xlab(expression(C[max]~(mg/L))) + 
  ylab('Densidad') +
  facet_grid(DD ~ II, labeller = labeller(.rows = label_both, .cols = label_both)) + 
  scale_color_manual(values = c('red', 'green3', 'blue'), name = 'Tinf') + 
  scale_fill_manual(values = c('red', 'green3', 'blue'), name = 'Tinf')
G4

for (var in c('G2', 'G3', 'G4')) {
  fileName = paste0('051_expDesc_', var)
  # print(paste0(fileName, '.pdf'))
  
  ggsave(paste0(fileName, '.pdf'), get(var), 'pdf', 'figures', 1, 8, 6)
}

columnList <- as.list(c('Dosis', 'Media', 'SD', 'Mín', 'Q1', 'Q2', 'Q3', 'Max'))
columnList <- set_names(columnList, c('Dosis', 'mn', 'sd', 'min', 'q1', 'q2', 'q3', 'max'))

columnList.values <- c(unlist(columnList, use.names=F))
columnList.keys <- names(columnList)


gt1 <- df1[df1$DD %in% c(1:4 * 1000),] %>%
  .[, .(
    mn  = mean(auc),
    sd  = sd(auc),
    min = min(auc),
    q1  = quantile(auc, probs = 0.25),
    q2  = quantile(auc, probs = 0.50),
    q3  = quantile(auc, probs = 0.75),
    max = max(auc)
  ), by = G] 

gt1 <- admDF[gt1, on = .(ID = G)]
gt1[, Dosis:= paste0(DD/1e3, 'g q', II, 'h, tinf ', Tinf)]


gt1 <- subset(gt1, select=columnList.keys) %>%
  gt() %>%
  fmt_number(columns = 2:8, decimals = 2) %>%
  cols_label(.list = columnList) %>%
  tab_header(title = 'Resumen de indicadores de Exposición', 
             subtitle = 'AUC') %>% 
  tab_options(table.font.size = 12)

#'-------------------------------------------------------------------------------
# Cmin
gt2 <- df1[df1$DD %in% c(1:4 * 1000),] %>%
  .[, .(
    mn  = mean(cmin),
    sd  = sd(cmin),
    min = min(cmin),
    q1  = quantile(cmin, probs = 0.25),
    q2  = quantile(cmin, probs = 0.50),
    q3  = quantile(cmin, probs = 0.75),
    max = max(cmin)
  ), by = G] 

gt2 <- admDF[gt2, on = .(ID = G)]
gt2[, Dosis:= paste0(DD/1e3, 'g q', II, 'h, tinf ', Tinf)]

gt2 <- subset(gt2, select=columnList.keys) %>%
  gt() %>%
  fmt_number(columns = 2:8, decimals = 2) %>%
  cols_label(.list = columnList) %>%
  tab_header(title = 'Resumen de indicadores de Exposición', 
             subtitle = 'Cmin (mg/L)') %>% 
  tab_options(table.font.size = 12)

#'-------------------------------------------------------------------------------
# Cmax
gt3 <- df1[df1$DD %in% c(1:4 * 1000),] %>%
  .[, .(
    mn  = mean(cmax),
    sd  = sd(cmax),
    min = min(cmax),
    q1  = quantile(cmax, probs = 0.25),
    q2  = quantile(cmax, probs = 0.50),
    q3  = quantile(cmax, probs = 0.75),
    max = max(cmax)
  ), by = G] 

gt3 <- admDF[gt3, on = .(ID = G)]
gt3[, Dosis:= paste0(DD/1e3, 'g q', II, 'h, tinf ', Tinf)]

gt3 <- subset(gt3, select=columnList.keys) %>%
  gt() %>%
  fmt_number(columns = 2:8, decimals = 2) %>%
  cols_label(.list = columnList) %>%
  tab_header(title = 'Resumen de indicadores de Exposición', 
             subtitle = 'Cmax (mg/L)') %>% 
  tab_options(table.font.size = 12)


gtsave(gt1, '050_indicadorAUC.html', 'reports')
gtsave(gt2, '051_indicadorCmin.html', 'reports')
gtsave(gt3, '052_indicadorCmax.html', 'reports')

