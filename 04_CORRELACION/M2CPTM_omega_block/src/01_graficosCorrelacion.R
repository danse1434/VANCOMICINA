##------------------------------------------------------------------------------#
## Nombre del Script: Obtención de gráficos de correlación entre parámetros 
## simulados ---
##  
## Proposito del Script: crear y almacenar gráficos generados a partir de 
##  estudios de la distribución de efectos aleatorios de parámetros. Estos 
##  se estudian mediante simulación con MCMC desde la distribución condicional. 
##  Se muestran correlogramas y gráficos de distribución mediante diagrama de 
##  cajas.
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creacion: 03-mar-2020 
## Fecha de modificación: 08-10-2020
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
require(patchwork)

# Script de funciones
source(file.path('src', '11_funciones.R'), encoding = 'UTF-8')
# Abrir datos de correlación
proy_dir <- file.path('M2CPTM_omega_block_1')
corr_dir <-
  file.path(proy_dir, 'ChartsData', 'CorrelationBetweenRandomEffects')

# Selección de tema
theme_set(theme_bw())

#-------------------------------------------------------------------------------#
# Matriz de correlación de efectos aleatorios ---------------------------------
#-------------------------------------------------------------------------------#
# Valores eta simulados
data_corr_1 <- read_csv(file.path(corr_dir, 'simulatedEta.txt'))
# Valores de eta por individuo
data_corr_2 <- read_csv(file.path(corr_dir, 'eta.txt'))
# Ayudas visuales a gráficos de correlación
data_corr_3 <- read_csv(file.path(corr_dir, 'visualGuides.txt'))

# Creación de matriz de correlación entre efectos aleatorios
correl_mat_com <-
  (
    hist_plot(data_corr_1, eta_Cl_simulated, expression(eta ~ (Cl))) +
      corr_plot(data_corr_1, eta_Q_simulated, eta_Cl_simulated) +
      corr_plot(data_corr_1, eta_V2_simulated, eta_Cl_simulated) +    
      corr_plot(data_corr_1, eta_Cl_simulated, eta_Q_simulated) +
      hist_plot(data_corr_1, eta_Q_simulated, expression(eta ~ (Q))) +
      corr_plot(data_corr_1, eta_V2_simulated, eta_Q_simulated) +
      corr_plot(data_corr_1, eta_Cl_simulated, eta_V2_simulated) +
      corr_plot(data_corr_1, eta_Q_simulated, eta_V2_simulated) +
      hist_plot(data_corr_1, eta_V2_simulated, expression(eta ~ (V[2])))
  ) +
  plot_layout(ncol = 3)

# Almacenamiento de gráficos
ggsave('figures/01_graficoCorrelacion1.png', correl_mat_com, device = 'png', 
       width = 6 * 1.5, height = 4 * 1.7, dpi = 720)
ggsave('figures/01_graficoCorrelacion2.pdf', correl_mat_com, device = 'pdf', 
       width = 6 * 1.5, height = 4 * 1.7)

#-------------------------------------------------------------------------------#
# Distribución de Eta -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Especificación del directorio de extracción de datos
esteta_dir <-
  file.path(proy_dir,
            'ChartsData',
            'DistributionOfTheStandardizedRandomEffects')

# Lectura de archivos de datos contenidos en el directorio
data_esteta_1 <-
  read_csv(file.path(esteta_dir, 'simulatedStandardizedEta.txt'))
data_esteta_2 <-
  read_csv(file.path(esteta_dir, 'StandardizedEta.txt'))

#-------------------------------------------------------------------------------#
# Especificación de parámetros de gráfico
#................................................................................
#' 1 Colapsar columnas de valores eta
#' 2 Eliminar las instancias en donde aparece "stand" o "_simulated"
#' 3 Convertir la variable parameter en un factor ordenado
#' 4 Generar gráfico
#................................................................................
data_esteta_2 <- data_esteta_1 %>%
  pivot_longer(
    cols = matches('_simulated'),
    names_to = 'parameter',
    values_to = 'eta_value'
  ) %>%
  mutate(
    parameter = str_replace(parameter, 'stand', ''),
    parameter = str_replace(parameter, '_simulated', ''),
    parameter = factor(parameter, levels = c('Eta_Cl', 'Eta_Q', 'Eta_V2'))
  ) 

G_boxplot <- data_esteta_2 %>% 
  ggplot(aes(x = parameter, y = eta_value, 
             colour = parameter)) +
  geom_hline(yintercept = 0) +
  # IC95% para distribución normal estándar
  geom_hline(yintercept = c(-1.960, 1.960), lty = 'dashed', size = 0.5) +
  # IC99% para distribución normal estándar
  geom_hline(yintercept = c(-2.576, 2.576), lty = 'dashed', size = 0.25) +
  # Violin, Puntos, y 
  geom_violin(aes(fill = after_scale(alpha(colour, 0.5)))) +
  geom_jitter(shape = 16, width = 0.1, height = 0, size = 1, alpha=0.1) + 
  geom_boxplot( width = .3,  position = "dodge", outlier.shape = 4,
                outlier.colour = "red"  ) +
  coord_cartesian(ylim = c(-5, 5)) +
  ylab(expression(eta["std"])) + xlab('') + 
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "none") 
# 
# Almacenamientos de gráficos
ggsave(G_boxplot, filename = 'figures/02_boxplotEtaEst1.png', 
        device = 'png', width = 5, height = 4, dpi = 720)
ggsave(G_boxplot, filename = 'figures/02_boxplotEtaEst2.pdf', 
        device = 'pdf', width = 5, height = 4)

#-------------------------------------------------------------------------------#
# Matriz de Correlación -----------------------------------------------------
#-------------------------------------------------------------------------------#
data_FishMatrix <- read_csv(file.path(proy_dir, 'FisherInformation', 
                                      'correlationEstimatesSA.txt'), 
                            col_names = FALSE)
# Cambio de nombre de columnas
colnames(data_FishMatrix) <- c('Parametros', data_FishMatrix$X1)
# Creación de gt
gt_FishMatrix <- data_FishMatrix %>% 
  gt::gt() %>% 
  gt::fmt_number(columns = 2:dim(data_FishMatrix)[2],
                 decimals = 3) %>% 
  gt::tab_header(title = 'Matriz de correlación - Modelo Base VAN N.º1') %>% 
  formatoCondicionalCovarianza('Cl_pop') %>% 
  formatoCondicionalCovarianza('V1_pop') %>% 
  formatoCondicionalCovarianza('Q_pop') %>% 
  formatoCondicionalCovarianza('V2_pop') %>% 
  formatoCondicionalCovarianza('omega_Cl') %>% 
  formatoCondicionalCovarianza('omega_Q') %>% 
  formatoCondicionalCovarianza('omega_V2') %>%
  formatoCondicionalCovarianza('b')

gt::gtsave(gt_FishMatrix, filename = '03_matrizCorrelacion.html', 
           path = glue::glue('{getwd()}/figures'))
