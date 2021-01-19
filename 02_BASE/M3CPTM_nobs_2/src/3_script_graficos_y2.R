##------------------------------------------------------------------------------#
## Nombre del Script: Obtención de gráficos a partir de datos de figuras ---
## generados por Monolix GUI
##  
## Propósito del Script: crear y almacenar gráficos generados a partir de 
## los datos generados por la suite de Monolix, se debe colocar en la misma 
## carpeta en la que se encuentra el proyecto. Este script lee en la carpeta 
## ChartsData, que tiene como subdirectorios a cada gráfico generado por la 
## suite de Monolix
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 04-feb-2020  
## Fecha de modificación: 29-may-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# Introducción-----------------------------------------------------
#-------------------------------------------------------------------------------#
# Carga de paquetes
require(scales)
require(ggrepel)
require(rlang)
require(patchwork)
require(tidyverse)
require(gt)

# Apertura de fuente
source(file.path('src', '10_performance_fun.R'), encoding = 'UTF-8')
source("M3CPTM_nobs_2/src/4_funciones_y2.R", encoding = 'UTF-8')

#-------------------------------------------------------------------------------#
# Bondad de ajuste -------------------------------------------------
#-------------------------------------------------------------------------------#
# Ajuste de una variable para guardar la subcarpeta que contiene datos de 
# gráfico de bondad de ajuste
auxdir <- file.path('M3CPTM_nobs_2', 'M3CPTM_nobs_2')
dir.create(path = file.path(auxdir, 'figures'))

gof_dir <- file.path(auxdir, 'ChartsData','ObservationsVsPredictions')

# Selección de tema
theme_set(theme_bw())

# Apertura de archivo de datos
y2_obsVsPred <- # Observaciones vs predicciones
  read_csv(file.path(gof_dir, 'y2_obsVsPred.txt'))

y2_visualGuides <- # Ayudas visuales
  read_csv(file.path(gof_dir, 'y2_visualGuides.txt'))

y2_obsVsSimulatedPred <- # Observaciones vs predicciones simuladas
  read_csv(file.path(gof_dir, 'y2_obsVsSimulatedPred.txt'))

#-------------------------------------------------------------------------------#
# Bondad de ajuste OBS vs PRED

G_PRED_OBS_PRED <-  y2_obsVsPred %>% 
  ggplot(mapping = aes(x = popPred, y = y2, group = ID)) +
  geom_point(shape = 1) + 
  xlab('PRED') + ylab('OBS') +
  geom_abline(slope = 1, intercept = 0, lty = 'dotted') + 
  # Modelo LOESS
  geom_line(y2_visualGuides, 
            mapping =  aes(x = popPred_spline_abscissa, y = popPred_spline), 
            inherit.aes = F, colour = 'blue4') +
  geom_ribbon(y2_visualGuides, 
              mapping =  aes(x = popPred_ci_abscissa, 
                             ymin = popPred_piLower,
                             ymax = popPred_piUpper), 
              inherit.aes = F, fill = 'blue1', alpha = 0.1) +
  coord_cartesian(xlim = c(0,60), ylim = c(0,60))

#-------------------------------------------------------------------------------#
# Gráficos de bondad de ajuste

G_PRED_OBS_PRED <- 
  GOF_PRED(x = popPred, y = y2, 
           xspline = popPred_spline_abscissa, 
           yspline = popPred_spline, 
           xconfint = popPred_ci_abscissa, 
           yconfint_lo = popPred_piLower, 
           yconfint_up = popPred_piUpper, 
           colourp = 'blue1', xlab = 'PRED', ylab = 'OBS')

G_PRED_OBS_IPRED <- 
  GOF_PRED(x = indivPredMean, y = y2, 
           xspline = indivPred_spline_abscissa, 
           yspline = indivPred_spline, 
           xconfint = indivPred_ci_abscissa, 
           yconfint_lo = indivPred_piLower, 
           yconfint_up = indivPred_piUpper, 
           colourp = 'red1', xlab = 'IPRED', ylab = 'OBS')

#-------------------------------------------------------------------------------#
# Bondad de ajuste OBS vs PPRED
G_PRED_OBS_PPRED <- 
  y2_obsVsSimulatedPred    %>%  
  ggplot(mapping = aes(x = indivPredSimulated, y = y2)) +
  geom_point(shape = 1) + 
  xlab('PPRED') + ylab('OBS') +
  stat_smooth(method = 'loess') +
  geom_abline(slope = 1, intercept = 0, lty = 'dotted') +
  coord_cartesian(xlim = c(0, 90), ylim = c(0, 90))

#-------------------------------------------------------------------------------#
# Transformación en logaritmos
breaks     <- 10^(0:2)
min_breaks <- rep(1:9, 3)*(10^rep(0:2, each=9))

G_PRED_OBS_IPREDLOG <-
  G_PRED_OBS_IPRED + 
  scale_y_continuous(trans = 'pseudo_log', 
                     breaks = breaks, minor_breaks = min_breaks) +
  scale_x_continuous(trans = 'pseudo_log', 
                     breaks = breaks, minor_breaks = min_breaks) 

G_PRED_OBS_PREDLOG <-
  G_PRED_OBS_PRED + 
  scale_y_continuous(trans = 'pseudo_log', 
                     breaks = breaks, minor_breaks = min_breaks) +
  scale_x_continuous(trans = 'pseudo_log', 
                     breaks = breaks, minor_breaks = min_breaks) 

# Almacenamiento en pdf de los gráficos
G_PRED_OBS_PRED <- G_PRED_OBS_PRED + predictivePerformaceLabel(
  y2_obsVsPred, 'popPred', 'y2', x = 0.65, round = 3, size=2.3, y=0.1,
  boot = TRUE, R = 1e3, xlim = c(0,60), ylim = c(0,60)
)
G_PRED_OBS_IPRED <- G_PRED_OBS_IPRED + predictivePerformaceLabel(
  y2_obsVsPred, 'indivPredMean', 'y2', x = 0.65, round = 3, size=2.3, y=0.1,
  boot = TRUE, R = 1e3, xlim = c(0,60), ylim = c(0,60)
)

G1 <- ((G_PRED_OBS_PRED + G_PRED_OBS_IPRED) /
      (G_PRED_OBS_PREDLOG + G_PRED_OBS_IPREDLOG)) &
      theme(panel.grid.major = element_line(colour = "gray80"), 
            panel.grid.minor = element_line(colour = "gray95"))

G1 <- G1 + plot_annotation(tag_levels = 'A')

ggsave(file.path(auxdir, 'figures/y2_G_GOF.pdf'), G1, 
       device = 'pdf', width = 8, height = 6, units = 'in')  

#-------------------------------------------------------------------------------#
# Residuales -------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Ajuste de una variable para guardar la subcarpeta que contiene datos de 
# gráfico de residuales

res_dir <- file.path(auxdir, 'ChartsData','ScatterPlotOfTheResiduals')
# Todos los Residuales
y2_residuals <-
  read_csv(file.path(res_dir, 'y2_residuals.txt'))
# Residuales simulados
y2_simulatedResiduals <-
  read_csv(file.path(res_dir, 'y2_simulatedResiduals.txt'))
# Percentiles de residuales vs predicción
y2_prediction_percentiles_iwRes <-
  read_csv(file.path(res_dir, 'y2_prediction_percentiles_iwRes.txt'))
y2_prediction_percentiles_pwRes <-
  read_csv(file.path(res_dir, 'y2_prediction_percentiles_pwRes.txt'))
y2_prediction_percentiles_npde <-
  read_csv(file.path(res_dir, 'y2_prediction_percentiles_npde.txt'))
# Percentiles de residuales vs tiempo
y2_time_percentiles_iwRes <-
  read_csv(file.path(res_dir, 'y2_time_percentiles_iwRes.txt'))
y2_time_percentiles_pwRes <-
  read_csv(file.path(res_dir, 'y2_time_percentiles_pwRes.txt'))
y2_time_percentiles_npde <-
  read_csv(file.path(res_dir, 'y2_time_percentiles_npde.txt'))
# Especificación de contenedores (bins) en los datos
y2_individualBins <-
  read_csv(file.path(res_dir, 'y2_individualBins.txt'))
y2_populationBins <-
  read_csv(file.path(res_dir, 'y2_populationBins.txt'))
y2_timeBins <-
  read_csv(file.path(res_dir, 'y2_timeBins.txt'))
# Especificación de línea de tendencia
y2_spline <-
  read_csv(file.path(res_dir, 'y2_spline.txt'))

#-------------------------------------------------------------------------------#
# Gráficos de residuales vs Tiempo ---------------------------------------------
#-------------------------------------------------------------------------------#

G_RES_T_PWRES <- RES_TSFD(
  x = time,
  y = pwRes,
  xspline = time_pwRes,
  yspline = time_pwRes_spline,
  perc_data = y2_time_percentiles_pwRes,
  xlab = 'TAD',
  ylab = 'PWRES'
)

G_RES_T_IWRES <- RES_TSFD(
  x = time,
  y = iwRes_mean,
  xspline = time_iwRes,
  yspline = time_iwRes_spline,
  perc_data = y2_time_percentiles_iwRes,
  xlab = 'TAD',
  ylab = 'IWRES'
)

G_RES_T_NPDE <- RES_TSFD(
  x = time,
  y = npde,
  xspline = time_npde,
  yspline = time_npde_spline,
  perc_data = y2_time_percentiles_npde,
  xlab = 'TAD',
  ylab = 'NPDE'
)

#-------------------------------------------------------------------------------#
# Gráficos de residuales vs Concentraciones -------------------------------
#-------------------------------------------------------------------------------#
# Residuales vs concentraciones

G_RES_C_PWRES <- 
  RES_PRE(x = prediction_pwRes, y = pwRes, 
          xspline = prediction_pwRes, yspline = prediction_pwRes_spline,
          perc_data = y2_prediction_percentiles_pwRes,
          xlab = 'PRED', ylab = 'WRES')

G_RES_C_IWRES <- 
  RES_PRE(x = prediction_iwRes_mean, y = iwRes_mean, 
          xspline = prediction_iwRes, yspline = prediction_iwRes_spline,
          perc_data = y2_prediction_percentiles_iwRes,
          xlab = 'IPRED', ylab = 'IWRES')

G_RES_C_NPDE <- 
  RES_PRE(x = prediction_npde, y = npde, 
          xspline = prediction_npde, yspline = prediction_npde_spline,
          perc_data = y2_prediction_percentiles_npde,
          xlab = 'PRED', ylab = 'NPDE')

#-------------------------------------------------------------------------------#
G2 <- 
  (G_RES_T_PWRES + G_RES_C_PWRES) / 
  (G_RES_T_IWRES + G_RES_C_IWRES) /
  (G_RES_T_NPDE  + G_RES_C_NPDE)  +
  plot_annotation(tag_levels = 'A')

ggsave(file.path(auxdir, 'figures/y2_G_RES.pdf'), G2, 'pdf', 
       width = 8, height = 8, units = 'in')

#-------------------------------------------------------------------------------#
# Gráficos observaciones y predicciones individuales ----------------------------
#-------------------------------------------------------------------------------#
# Carga de ajustes individuales
y2_fits <-
  read_csv(file.path(auxdir, 'ChartsData', 'IndividualFits', 'y2_fits.txt'))
  
y2_observations <- 
  read_csv(file.path(auxdir, 'ChartsData', 'IndividualFits', 
                     'y2_observations.txt'))

G_CP_TAD <-  y2_fits %>%
  ggplot(mapping = aes(x = time)) +
  geom_ribbon(y2_observations, mapping = aes(x = time, ymin = piLower, ymax = piUpper),
              fill = alpha('gray50', 0.14)) + 
  geom_line(aes(y = pop), lty = 'dashed') +
  geom_line(aes(y = indivPredMean), lty = 'solid', col = 'blue2') +
  facet_wrap( ~ ID, ncol = 4, labeller = labeller(.cols = label_both)) +
  geom_point(data = y2_observations,
             mapping = aes(x = time, y = y2),
             col = 'blue4') +
  scale_x_continuous(breaks = seq(0, 12, 2), minor_breaks = seq(0,12,1)) +
  coord_cartesian(xlim = c(0, 12)) +
  xlab('TAD') + ylab(expression(C[P]~"(mg/L)"))

ggsave(file.path(auxdir, 'figures/y2_G_CP_TAD.pdf'), G_CP_TAD, 'pdf', 
       width = 5, height = 6)
  
#-------------------------------------------------------------------------------#
# Cálculo post-hoc de encogimiento eta y epsilon ----------------------------
#-------------------------------------------------------------------------------#
eta <- 
  read_csv(file.path(auxdir, 'ChartsData', 'CorrelationBetweenRandomEffects' , 
                     'eta.txt'))

popeta <- read_csv(file.path(auxdir, 'populationParameters.txt'))

popeta1 <- popeta %>% 
  filter(str_detect(parameter, "omega\\_")) %>% 
  select(parameter, value) %>% 
  pivot_wider(names_from = parameter, values_from = value)

vareta <- eta %>% 
    summarise(across(matches("\\_mean"), ~var(.x, na.rm = TRUE))) %>% 
    rename_with(~str_replace_all(.x, "eta\\_|\\_mean", '')) %>%  
    rename_with(~paste0(.x, '_pop'))

# Shrinkage Eta
eta_res <- 1 - (vareta / (popeta1^ 2))

# Shrinkage Epsilon
epsilon_shrink <- 1-sd(y2_residuals$iwRes_mean)


gt_shrinkage <- 
  eta_res %>% 
  bind_cols(list(eta_Shrinkage = epsilon_shrink)) %>% 
  gt() %>% 
  fmt_number(columns = 1:(dim(eta_res)[2]+1), decimals = 3) %>% 
  gt::tab_header(
    title = 'Encogimiento de parámetros')

gt_shrinkage %>% 
  gt::gtsave('y2_shrink.html', file.path(getwd(), auxdir, 'figures'))

#-------------------------------------------------------------------------------#
# Gráfico de densidad distribución parámetros -----------------------------
#-------------------------------------------------------------------------------#
# 
data1a <- read_csv(file.path(auxdir, 'ChartsData', 
                     'DistributionOfTheIndividualParameters', 'cdf.txt'))
data1b <- read_csv(file.path(auxdir, 'ChartsData', 
                            'DistributionOfTheIndividualParameters', 'pdf.txt'))




