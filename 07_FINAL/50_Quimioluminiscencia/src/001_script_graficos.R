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
source(file.path('src', '050_performance_fun.R'), encoding = 'UTF-8')
source(file.path('src', '051_fun_graficos_combinados.R'), encoding = 'UTF-8')

#-------------------------------------------------------------------------------#
# Bondad de ajuste -------------------------------------------------
#-------------------------------------------------------------------------------#
# Ajuste de una variable para guardar la subcarpeta que contiene datos de 
# gráfico de bondad de ajuste
auxdir <- file.path('run200')
dir.create(path = file.path(auxdir, 'figures'))

gof_dir <- file.path(auxdir, 'ChartsData','ObservationsVsPredictions')
res_dir <- file.path(auxdir, 'ChartsData','ScatterPlotOfTheResiduals')
red_dir <- file.path(auxdir, 'ChartsData','DistributionOfTheResiduals')

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

# Gráficos de bondad de ajuste

# Predicciones poblacionales 
G_PRED_OBS_PRED <- GOF_PRED(
  y2_obsVsPred,
  popPred,
  y2,
  xlab = "Population predicted \n concentrations (mg/L)",
  ylab = "Observed concentrations (mg/L)",
  colourp = "blue4"
) + coord_cartesian(xlim = c(0, 60), ylim = c(0, 60)) +
  predictivePerformaceLabel(y2_obsVsPred, 'popPred', 'y2', hjust =
                              1, xlim = c(40, 60), x = 1, y = 0.1, boot = F)
# Predicciones individuales 
G_PRED_OBS_IPRED <- GOF_PRED(
  y2_obsVsPred,
  indivPredMean,
  y2,
  xlab = "Individual predicted \n concentrations (mg/L)",
  ylab = "Observed concentrations (mg/L)",
  colourp = "red4"
) + coord_cartesian(xlim = c(0, 60), ylim = c(0, 60)) +
  predictivePerformaceLabel(y2_obsVsPred, 'indivPredMean', 'y2', hjust =
                              1, xlim = c(0, 60), y = 0.1, x = 1, boot = F)

#-------------------------------------------------------------------------------#
# Transformación en logaritmos
breaks     <- 10^(0:2)
min_breaks <- rep(1:9, 3)*(10^rep(0:2, each=9))

G_PRED_OBS_PREDLOG <- GOF_PRED(
  y2_obsVsPred,
  popPred,
  y2,
  xlab = "Population predicted \n concentrations (mg/L)",
  ylab = "Observed concentrations (mg/L)",
  colourp = "blue4"
) + coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  scale_y_continuous(trans = 'pseudo_log',
                     breaks = breaks,
                     minor_breaks = min_breaks) +
  scale_x_continuous(trans = 'pseudo_log',
                     breaks = breaks,
                     minor_breaks = min_breaks)

G_PRED_OBS_IPREDLOG <- GOF_PRED(
  y2_obsVsPred,
  indivPredMean,
  y2,
  xlab = "Individual predicted \n concentrations (mg/L)",
  ylab = "Observed concentrations (mg/L)",
  colourp = "red4"
) + coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  scale_y_continuous(trans = 'pseudo_log',
                     breaks = breaks,
                     minor_breaks = min_breaks) +
  scale_x_continuous(trans = 'pseudo_log',
                     breaks = breaks,
                     minor_breaks = min_breaks) 

# Almacenamiento en pdf de los gráficos
G1 <- ((G_PRED_OBS_PRED + G_PRED_OBS_IPRED) /
      (G_PRED_OBS_PREDLOG + G_PRED_OBS_IPREDLOG)) &
      theme(panel.grid.major = element_line(colour = "gray80"), 
            panel.grid.minor = element_line(colour = "gray95"))

G1 <- G1 + plot_annotation(tag_levels = 'A')

ggsave(file.path(auxdir, "figures", "y2_G_GOF.pdf"), G1, 
       device = 'pdf', width = 9, height = 9 * 0.9, units = 'in') 
saveRDS(G1, file.path(auxdir, 'figures/y2_G_GOF.rds'))

#-------------------------------------------------------------------------------#
# Residuales -------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Ajuste de una variable para guardar la subcarpeta que contiene datos de 
# gráfico de residuales

# Residuales
y2_residuals <- read_csv(file.path(res_dir, 'y2_residuals.txt'))

#-------------------------------------------------------------------------------#
# Gráficos de residuales vs Tiempo ---------------------------------------------
#-------------------------------------------------------------------------------#

# > 2.1. Gráfico de Residuales vs Tiempo

G_RES_T_PWRES <- RES_PRE(y2_residuals, time, pwRes, ID, 7,
                         'Time after last dose (h)',
                         'PWRES', ylim = c(-4, 4),
                         legend = c(0.5, 0.08))
G_RES_T_IWRES <- RES_PRE(y2_residuals, time, iwRes_mean, ID, 7,
                         'Time after last dose (h)',
                         'IWRES', ylim = c(-4, 4),
                         legend = c(0.5, 0.08))
G_RES_T_NPDE  <- RES_PRE(y2_residuals, time, npde, ID, 7,
                         'Time after last dose (h)',
                         'NPDE', ylim = c(-4, 4),
                         legend = c(0.5, 0.08))

# > 2.2. Gráfico de Residuales vs Concentración

G_RES_C_PWRES <- RES_PRE(y2_residuals, prediction_pwRes, 
                         pwRes, ID, 7,
                         'Population predicted \n concentration (mg/L)',
                         'PWRES', ylim = c(-4, 4),
                         legend = c(0.5, 0.08))
G_RES_C_IWRES <- RES_PRE(y2_residuals, prediction_iwRes_mean, 
                         iwRes_mean, ID, 7,
                         'Individual predicted \n concentration (mg/L)',
                         'IWRES', ylim = c(-4, 4),
                         legend = c(0.5, 0.08))
G_RES_C_NPDE  <- RES_PRE(y2_residuals, prediction_npde, 
                         npde, ID, 7,
                         'Population predicted \n concentration (mg/L)',
                         'NPDE', ylim = c(-4, 4),
                         legend = c(0.5, 0.08))

#-------------------------------------------------------------------------------#

G2 <- wrap_plots(G_RES_T_PWRES, G_RES_T_IWRES, G_RES_T_NPDE, G_RES_C_PWRES, 
                 G_RES_C_IWRES, G_RES_C_NPDE) +
  plot_annotation(tag_levels = 'A')

ggsave(file.path(auxdir, 'figures/y2_G_RES.pdf'), G2, 'pdf', 
       width = 8, height = 8*0.75, units = 'in')

saveRDS(G2, file.path(auxdir, 'figures/y2_G_RES.rds'))

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
  facet_wrap( ~ ID, ncol = 5, labeller = labeller(.cols = label_both)) +
  geom_point(data = y2_observations,
             mapping = aes(x = time, y = y2),
             col = 'blue4') +
  scale_x_continuous(breaks = seq(0, 12, 2), minor_breaks = seq(0,12,1)) +
  coord_cartesian(xlim = c(0, 12)) +
  xlab('Time after dose administration (h)') + 
  ylab(expression(C[P]~"(mg/L)"))

ggsave(file.path(auxdir, 'figures/y2_G_CP_TAD.pdf'), G_CP_TAD, 'pdf', 
       width = 7, height = 7*0.8)

saveRDS(G_CP_TAD, file.path(auxdir, 'figures/y2_G_CP_TAD.rds'))
  
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
    summarise(across(matches("\\mode"), ~var(.x, na.rm = TRUE))) %>% 
    rename_with(~str_replace_all(.x, "eta\\_|\\_mode", '')) %>%  
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


y2_residuals %>% pull(pwRes) %>% 
  range() %>% seq(from = .[1], oy)

G_DIST_RESIDUALES <- function(data, var) {
  data %>%
    ggplot(aes_string(x = var)) +
    geom_histogram(
      binwidth =  function(x)
        (max(x) - min(x)) / (ceiling(log2(length(x))) + 1), 
      color = "black", alpha = 0.1
    ) + 
    stat_function(fun = function(x) dnorm(x, 0, sd(x)) * length(x))
}

G_DIST_RESIDUALES(y2_residuals, "pwRes")
G_DIST_RESIDUALES(y2_residuals, "iwRes_mean")


