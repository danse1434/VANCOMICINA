require(data.table)
require(progress)
require(scales)

source(file.path('src', '069_paquetesSimulacion.R'), encoding = 'UTF-8')
source(file.path('src', '081_fun_CalculoIndicadores.R'), encoding = 'UTF-8')


outputfile <- '002_out_desconocido'

dataLS <- list()

for (i in 1:21) {
  data0 <- fread(file.path('results', paste0(outputfile, i, '.csv')))
  data0[, G := i]
  # data1 <- resPTA_Tabla(data0, MIC_vec_1, resPTA1_AUC, crit = 400, g = i)
  
  dataLS[[i]] <- data0
}

admDF <- fread(file.path('data', 'adm_list.csv'))

G1 <- map_df(dataLS, ~.x)[admDF, on = .(G = ID)] %>%
  ggplot(aes(x = time, color = factor(tinf), group = G)) + 
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = factor(tinf)), alpha = 0.1) +
  geom_line(aes(y = Q2)) + 
  xlab('Tiempo (hr)') + ylab(expression(C[PRED]~(mg/L))) +
  facet_grid(DD ~ ii, labeller = labeller(.rows = label_both, .cols = label_both)) + 
  scale_color_manual(values = c('red', 'green', 'blue'), name = 'Tinf') + 
  scale_fill_manual(values = c('red', 'green', 'blue'), name = 'Tinf') + 
  labs(title = 'Simulación Vancomicina SS', subtitle = 'CLCR: Desconocido')



ggsave(paste0('perfil_', outputfile, '.pdf'), G1, 'pdf', 'figures', 1, 8, 6)
