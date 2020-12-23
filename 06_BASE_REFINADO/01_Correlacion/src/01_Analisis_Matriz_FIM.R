##------------------------------------------------------------------------------#
## Nombre del Script: Análisis de Matriz de Información de Fisher con 
##     correlación entre parámetros y estabilidad frente a inversión -----
##  
## Propósito del Script: analizar matriz FIM
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  22-12-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
require(gt)
require(rlang)
require(tidyverse)
require(corrr)
require(patchwork)

#-------------------------------------------------------------------------------#
# 1. Lectura y procesamiento de Matriz FIM ---------------------
#-------------------------------------------------------------------------------#

# Especificación de modelo
model_1 <- file.path('models', 'M2CPTM_nobs_1_prop')
# Lectura de archivo de correlación
dfCorr_1 <- read_csv(file.path(model_1, 'M2CPTM_nobs_1_prop', 
                               'FisherInformation', 'correlationEstimatesSA.txt'), 
                     col_names = FALSE)
# Cambiar los nombres de columnas
colnames(dfCorr_1) <- c('I', dfCorr_1$X1)
# Convertir en objeto tipo Corr
dfCorr_2 <- dfCorr_1 %>%
  column_to_rownames(var='I') %>% 
  corrr::as_cordf()

dfCorr_2

#-------------------------------------------------------------------------------#
# 1.1. Crear gráficos 

label_vec <- c('Cl', bquote('V'[1]), 'Q', bquote('V'[2]), 
               bquote(omega['Cl']^2), bquote(omega['V'[1]]^2), 
               bquote(omega['Q']^2), bquote(omega['V'[2]]^2), 'b')


df_Corr_plot <-
  dfCorr_2 %>%
  rplot(
    colours = c('red', 'white', 'green'),
    shape = 16,
    print_cor = TRUE
  ) +
  scale_x_discrete(labels = label_vec) +
  scale_y_discrete(labels = rev(label_vec)) +
  theme(panel.border = element_rect(color = 'black', fill = NA))


ggsave(file.path('figures', '01_Correlacion_Base_1.pdf'), 
       df_Corr_plot, 'pdf', width = 6, height = 4)

#-------------------------------------------------------------------------------#
# 1.2. Matriz de correlación en formato tabular

df_Corr_gt <- shave(dfCorr_2) %>% 
  filter(term!='Cl_pop') %>% 
  gt(rowname_col='term') %>% 
  fmt_number(columns = everything(), decimals = 3) %>% 
  fmt_missing(columns=everything(), missing_text = '-') %>% 
  gt::tab_header(title = md('**Modelo Base - Matriz FIM**'))

gtsave(df_Corr_gt, '02_Correlacion_Base_1.html', normalizePath(file.path('figures')))

#-------------------------------------------------------------------------------#
# 1.3. Precisión de Estimación

dfPrec_1 <- read_csv(file.path(model_1, 'M2CPTM_nobs_1_prop', 
                               'populationParameters.txt'))

dfPrec_2 <- dfPrec_1 %>% 
  mutate(parameter = factor(parameter)) %>% 
  add_column(algorithm = 'SAEM')

# Carga de modelo bayesiano
load(file = file.path('models', paste0('104_modeltwoCptmDiagProp_errResNor', "Fit.Rsave")))

dfPrec_3 <- as.matrix(fit) %>% 
  as_tibble() %>% 
  select(1:9)

colnames(dfPrec_3) <- dfPrec_1$parameter

dfPrec_4 <- dfPrec_3 %>% 
  summarise(across(everything(), ~sd(.x)*100/mean(.x))) %>% 
  t(.) %>% 
  as.data.frame() %>% 
  rownames_to_column('parameter') %>% 
  rename(rse_sa=V1) %>% 
  add_column(algorithm = 'NUTS') 


# Creación de un gráfico con medidas unificadas para ambos algoritmos

df_Prec_plot <- dfPrec_2 %>% 
  bind_rows(dfPrec_4) %>% 
  mutate(parameter = factor(parameter)) %>% 
  ggplot(aes(x = rse_sa, 
             y = fct_reorder2(parameter, algorithm, rse_sa, .desc=FALSE), 
             fill = algorithm)) + 
  geom_bar(stat = 'identity', position = 'dodge', colour='black') +
  xlab('RSE (%)') + 
  theme_bw() +
  scale_y_discrete(labels = rev(label_vec[c(8,6,3,7,5,9,1,4,2)])) +
  scale_fill_manual(name='Algoritmo', values = c('red2', 'blue2')) +
  geom_text(
    aes(
      x = rse_sa * 1.0,
      label = ifelse(rse_sa > 1, round(rse_sa, 2), formatC(rse_sa, digits = 2))
    ),
    position = position_dodge(width = 0.9),
    hjust = -0.1
  ) +
  coord_cartesian(xlim = c(0, 145)) +
  theme(
    legend.position = c(0.85, 0.2), 
    axis.title.y = element_blank())


# Almacenamiento de gráfico
(df_Prec_plot + df_Corr_plot + plot_annotation(tag_levels = 'A')) %>% 
  ggsave(file.path('figures', '03_Correlacion_Base_2.pdf'), 
         ., 'pdf', width = 10, height = 5)
