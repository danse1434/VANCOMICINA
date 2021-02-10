##------------------------------------------------------------------------------#
## Nombre del Script: Obtención de gráficos a partir de datos de figuras ---
## generados por Monolix GUI - Modelo Combinado
##  
## Propósito del Script: crear y almacenar gráficos generados a partir de 
## los datos generados por la suite de Monolix, se debe colocar en la misma 
## carpeta en la que se encuentra el proyecto. Este script lee en la carpeta 
## ChartsData, que tiene como subdirectorios a cada gráfico generado por la 
## suite de Monolix
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 22-ene-2021  
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# 1. Introducción -----------------------------------------------------
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

# Definición Carpetas
homedir <- file.path('BASE')
gofdir  <- file.path(homedir, 'ChartsData', 'ObservationsVsPredictions')
resdir  <- file.path(homedir, 'ChartsData', 'ScatterPlotOfTheResiduals')
prodir  <- file.path(homedir, 'ChartsData', 'IndividualFits')
etadir  <- file.path(homedir, 'ChartsData', 'CorrelationBetweenRandomEffects')

dir.create(path = file.path(homedir, 'figures'), showWarnings = FALSE)
theme_set(theme_bw())


#-------------------------------------------------------------------------------#
# 1. Bondad de Ajuste -----------------
#-------------------------------------------------------------------------------#
# Lectura de archivos
y_gof_1 <- read_csv(file.path(gofdir, 'y1_obsVsPred.txt')) 
y_gof_2 <- read_csv(file.path(gofdir, 'y2_obsVsPred.txt'))

# Unión de DataFrames
y_gof   <- bind_rows(
  y_gof_1 %>% add_column(YTYPE = 1L) %>% rename(y = y1),
  y_gof_2 %>% add_column(YTYPE = 2L) %>% rename(y = y2)
)

G_PRED_OBS_PRED <- 
  GOF_PRED(y_gof, popPred, y, YTYPE, 
           xlab = 'PRED', ylab = 'OBS', xlim = c(0, 60), ylim = c(0, 60), 
           legend = c(0.16, 0.82)) +
  predictivePerformaceLabel(y_gof, 'popPred', 'y', 1.1)


G_PRED_OBS_IPRED <- 
  GOF_PRED(y_gof, indivPredMean, y, YTYPE,
           xlab = 'IPRED', ylab = 'OBS', xlim = c(0, 60), ylim = c(0, 60), 
           colourp = c('red', 'purple'), legend = c(0.16,.82)) +
  predictivePerformaceLabel(y_gof, 'indivPredMean', 'y', 0.7)

#-------------------------------------------------------------------------------#
# Transformación en logaritmos
breaks     <- 10^(0:2)
min_breaks <- rep(1:9, 3)*(10^rep(0:2, each=9))

G_PRED_OBS_PREDLOG <- 
  GOF_PRED(y_gof, popPred, y, YTYPE, 
           xlab = 'PRED', ylab = 'OBS', xlim = c(2, 100), ylim = c(2, 100)) + 
  scale_y_continuous(trans = 'pseudo_log', 
                     breaks = breaks, minor_breaks = min_breaks) +
  scale_x_continuous(trans = 'pseudo_log', 
                     breaks = breaks, minor_breaks = min_breaks)


G_PRED_OBS_IPREDLOG <- 
  GOF_PRED(y_gof, indivPredMean, y, YTYPE,
           xlab = 'IPRED', ylab = 'OBS', xlim = c(2, 100), ylim = c(2, 100), 
           colourp = c('red', 'purple')) + 
  scale_y_continuous(trans = 'pseudo_log', 
                     breaks = breaks, minor_breaks = min_breaks) +
  scale_x_continuous(trans = 'pseudo_log', 
                     breaks = breaks, minor_breaks = min_breaks) 

G1 <- (G_PRED_OBS_PRED + G_PRED_OBS_IPRED + 
         G_PRED_OBS_PREDLOG + G_PRED_OBS_IPREDLOG +
         plot_layout(ncol = 2))  &
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray95"), 
        legend.key.size = unit(0.03, 'npc'),
        legend.text = element_text(size = rel(0.9))
  )  

# Creación de gráfico conjunto
G1 <- G1 + plot_annotation(tag_levels = 'A')

ggsave(file.path(homedir, 'figures', '001_y_G_GOF.pdf'), G1, 
       device = 'pdf', width = 8, height = 6, units = 'in')  


#-------------------------------------------------------------------------------#
# 2. Residuales -----------------
#-------------------------------------------------------------------------------#
# Lectura de archivos
y_residuals_1 <- read_csv(file.path(resdir, 'y1_residuals.txt'))
y_residuals_2 <- read_csv(file.path(resdir, 'y2_residuals.txt'))

# Unión de DataFrames
y_residuals   <- bind_rows(
  y_residuals_1 %>% add_column(YTYPE = 1),
  y_residuals_2 %>% add_column(YTYPE = 2)
)

# > 2.1. Gráfico de Residuales vs Tiempo

G_RES_T_PWRES <- RES_PRE(
  y_residuals, time, pwRes, YTYPE, ID, 7, 'TAD (h)', 'PWRES', 
  ylim = c(-5, 5), colourp = c('#1E4769', '#50B5A6'), legend = c(0.5, 0.08))
G_RES_T_IWRES <- RES_PRE(
  y_residuals, time, iwRes_mean, YTYPE, ID, 7, 'TAD (h)', 'IWRES', 
  ylim = c(-5, 5), colourp = c('#1E4769', '#50B5A6'))
G_RES_T_NPDE  <- RES_PRE(
  y_residuals, time, npde, YTYPE, ID, 7, 'TAD (h)', 'NPDE', 
  ylim = c(-5, 5), colourp = c('#1E4769', '#50B5A6'))

# > 2.2. Gráfico de Residuales vs Concentración

G_RES_C_PWRES <- RES_PRE(
  y_residuals, prediction_pwRes, pwRes, YTYPE, ID, 7, 'PRED (mg/L)', 'PWRES',
  ylim = c(-5, 5), colourp = c('#8721B8', '#B82D18'), legend = c(0.5, 0.08))
G_RES_C_IWRES <- RES_PRE(
  y_residuals, prediction_iwRes_mean, iwRes_mean, YTYPE, ID, 7, 'IPRED (mg/L)', 'IWRES',
  ylim = c(-5, 5), colourp = c('#8721B8', '#B82D18'))
G_RES_C_NPDE  <- RES_PRE(
  y_residuals, prediction_npde , npde, YTYPE, ID, 7, 'PRED (mg/L)', 'NPDE',
  ylim = c(-5, 5), colourp = c('#8721B8', '#B82D18'))

#-------------------------------------------------------------------------------#
G2 <- (G_RES_T_PWRES + G_RES_T_IWRES + G_RES_T_NPDE + 
         G_RES_C_PWRES + G_RES_C_IWRES + G_RES_C_NPDE) + 
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = 'A')

G2 <- G2 &
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray95"), 
        legend.key.size = unit(0.03, 'npc'),
        legend.text = element_text(size = rel(0.6)), 
        legend.spacing.x = unit(0.5,'pt'),
        legend.title = element_text(size = rel(0.6), face = 'bold'), 
        legend.margin = margin(0),
        legend.direction = 'horizontal'
  )


ggsave(file.path(homedir, 'figures', '002_y_G_RES.pdf'), G2, 'pdf', 
       width = 8, height = 5, units = 'in')

#-------------------------------------------------------------------------------#
# 3. Gráficos de concentración vs tiempo ----------
#-------------------------------------------------------------------------------#

yfits1 <- read_csv(file.path(prodir, 'y1_fits.txt'))
yfits2 <- read_csv(file.path(prodir, 'y2_fits.txt'))
yobs1  <- read_csv(file.path(prodir, 'y1_observations.txt'))
yobs2  <- read_csv(file.path(prodir, 'y2_observations.txt'))

yfits <- bind_rows(
  yfits1 %>% add_column(YTYPE = 1) , 
  yfits2 %>% add_column(YTYPE = 2) 
)

yobs <- bind_rows(
  yobs1 %>% add_column(YTYPE = 1) %>% rename(y = y1),
  yobs2 %>% add_column(YTYPE = 2) %>% rename(y = y2)
)


G_CP_col_vec  <- c('#FACD00', '#0000FA')
G_CP_fill_vec <- c('#FACD00', '#0000FA')
G_CP_lty_vec  <- c('solid', 'dashed')

names(G_CP_col_vec) <-
  names(G_CP_fill_vec) <- c('Micro.', 'Quimiol.')

names(G_CP_lty_vec) <- c('PRED', 'IPRED')

G_CP_TAD <-  yfits %>% 
  ggplot(aes(x = time)) +
  geom_ribbon(yobs, 
              mapping = aes(x = time, ymin = piLower, ymax = piUpper, 
                            color = ifelse(YTYPE==1, 'Micro.', 'Quimiol.'),
                            fill = ifelse(YTYPE==1, 'Micro.', 'Quimiol.')
              ), alpha=0.05) +
  geom_point(yobs, 
             mapping = aes(x = time, y = y, color=ifelse(YTYPE==1, 'Micro.', 'Quimiol.')), 
             alpha = 0.5) +
  geom_line(aes(y = popPred, lty = 'PRED'), color='gray10') +
  geom_line(aes(y = indivPredMean, lty = 'IPRED'), color='black') + 
  facet_wrap( ~ ID, ncol = 5, labeller = labeller(.cols = label_both)) +
  # 
  scale_x_continuous(breaks = seq(0, 12, 2), minor_breaks = seq(0, 12, 1)) +
  coord_cartesian(xlim = c(0, 12)) +
  scale_color_manual(values=G_CP_col_vec, name='Tipo') +
  scale_fill_manual(values=G_CP_fill_vec, name='Tipo') +
  scale_linetype_manual(values=G_CP_lty_vec, name='Pred') +
  guides(
    colour = guide_legend(ncol = 1),
    fill = guide_legend(ncol = 1),
    linetype = guide_legend(ncol = 1)
  ) +
  xlab('TAD (h)') + ylab(expression(C[P]~"(mg/L)")) +
  theme(
    legend.position = c(0.9, 0.1),
    legend.box = 'vertical',
    panel.grid = element_blank()
  )

# Original width = 5, height = 6; 4 Columnas
ggsave(file.path(homedir, 'figures', '003_y_G_CP_TAD.pdf'), G_CP_TAD, 'pdf', 
       width = 8, height = 4.5)

#-------------------------------------------------------------------------------#
# 4. Cálculo post-hoc de encogimiento eta y epsilon ----------------------------
#-------------------------------------------------------------------------------#
eta    <- read_csv(file.path(etadir, 'eta.txt'))
popeta <- read_csv(file.path(homedir, 'populationParameters.txt'))

popeta1 <- popeta %>% 
  # filter(str_detect(parameter, "omega\\_")) %>% 
  # select(parameter, value) %>% 
  .[grep('omega\\_', popeta$parameter), c('parameter', 'value')] %>% 
  pivot_wider(names_from = parameter, values_from = value)

vareta <- eta %>% 
  summarise(across(matches("\\_mean"), ~var(.x, na.rm = TRUE))) %>% 
  rename_with(~str_replace_all(.x, "eta\\_|\\_mean", '')) %>%  
  rename_with(~paste0(.x, '_pop'))

# Shrinkage Eta
eta_res <- 1 - (vareta / (popeta1^ 2))

# Shrinkage Epsilon
epsilon_shrink <- 1 - sd(y_residuals$iwRes_mean)


gt_shrinkage <- 
  eta_res %>% 
  bind_cols(list(eta_Shrinkage = epsilon_shrink)) %>% 
  gt() %>% 
  fmt_number(columns = 1:(dim(eta_res)[2]+1), decimals = 3) %>% 
  gt::tab_header(
    title = 'Encogimiento de parámetros')

gt_shrinkage %>% 
  gt::gtsave('004_y_shrink.html', file.path(homedir, 'figures') %>% normalizePath())

















