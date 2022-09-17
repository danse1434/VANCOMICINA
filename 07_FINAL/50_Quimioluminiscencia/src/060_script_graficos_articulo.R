##------------------------------------------------------------------------------#
## Nombre del Script: Obtención de gráficos para artículo
##  
## Propósito del Script: crear y almacenar gráficos para la generación del reporte
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
homedir <- file.path('run200')
gofdir  <- file.path(homedir, 'ChartsData', 'ObservationsVsPredictions')
resdir  <- file.path(homedir, 'ChartsData', 'ScatterPlotOfTheResiduals')
prodir  <- file.path(homedir, 'ChartsData', 'IndividualFits')
etadir  <- file.path(homedir, 'ChartsData', 'CorrelationBetweenRandomEffects')

dir.create(path = file.path(homedir, 'figures'), showWarnings = FALSE)
theme_set(theme_bw())

figdir  <- file.path(homedir, "figures")

#-------------------------------------------------------------------------------#
# 1. Bondad de Ajuste -----------------
#-------------------------------------------------------------------------------#
# Lectura de archivos

ggPlots <- list()

ggPRED <- readRDS(file.path(figdir, "y2_G_GOF.rds"))
ggRES  <- readRDS(file.path(figdir, "y2_G_RES.rds"))
ggVPC  <- readRDS(file.path(figdir, "pcVPC_IC.rds"))

ggPlots$PPRED <- ggPRED[[1]][[1]] + ylab("Observerd \nconcentration (mg/L)") + 
  xlab("Population predicted \n concentration (mg/L)")
ggPlots$IPRED <- ggPRED[[1]][[2]] + ylab("Observerd \nconcentration (mg/L)") + 
  xlab("Individual predicted \n concentration (mg/L)")
ggPlots$NPDE  <- ggRES[[6]] + ylab("Normalized prediction \n distribution errors (NPDE)")
ggPlots$IWRES <- ggRES[[5]] + ylab("Individual Weighted Residuals")

ggPlots$VPC   <- ggVPC + xlab("Time After Dose (h)") + 
  ylab("Prediction corrected\n concentration (mg/L)")


design <- "AABB\nCCDD\nEE##"
design <- "AABB\nCCDD\n#EE#"


ggAnnotate <- wrap_plots(ggPlots) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(design = design)

ggsave(file.path(figdir, "plot_articles.pdf"), ggAnnotate, "pdf", width = 10, height = 10*1.00)
ggsave(file.path(figdir, "plot_articles.png"), ggAnnotate, "png", width = 10, height = 10*1.00)












y_gof_1 <- read_csv(file.path(gofdir, 'y1_obsVsPred.txt')) 
y_gof_2 <- read_csv(file.path(gofdir, 'y2_obsVsPred.txt'))

# Unión de DataFrames
y_gof   <- bind_rows(
  y_gof_1 %>% add_column(YTYPE = 1L) %>% rename(y = y1),
  y_gof_2 %>% add_column(YTYPE = 2L) %>% rename(y = y2)
)

y_visGuides_1 <- read_csv(file.path(gofdir, 'y1_visualGuides.txt'))
y_visGuides_2 <- read_csv(file.path(gofdir, 'y2_visualGuides.txt'))

y_visGuides <- bind_rows(
  y_visGuides_1 %>% add_column(YTYPE = 1L),
  y_visGuides_2 %>% add_column(YTYPE = 2L)
)



G_PRED_OBS_PRED <-
  GOF_PRED(
    y_gof,
    popPred,
    y,
    YTYPE,
    xlab = 'Population prediction (mg/L)',
    ylab = 'Observation (mg/L)',
    legend = c(0.16, 0.82)
  ) +
  coord_flip(xlim = c(0, 60), ylim = c(0, 60)) +
  guides(
    colour = guide_legend(title = 'Method'),
    linetype = guide_legend(title = 'Method'),
    fill = guide_legend(title = 'Method'),
  ) +
  predictivePerformaceLabel(y_gof, 'popPred', 'y', x = .15, y = 0.75)

G_PRED_OBS_PRED


G_PRED_OBS_IPRED <- 
  GOF_PRED(y_gof, indivPredMean, y, YTYPE,
           xlab = 'Individual prediction (mg/L)', ylab = 'Observation (mg/L)',
           legend = c(0.16, 0.82)) +
  coord_flip(xlim = c(0, 60), ylim = c(0, 60)) +
  guides(
    colour = guide_legend(title = 'Method'),
    linetype = guide_legend(title = 'Method'),
    fill = guide_legend(title = 'Method'),
  ) +
  predictivePerformaceLabel(y_gof, 'indivPredMean', 'y', .15, .75)


G_PRED_OBS_IPRED


#'-------------------------------------------------------------------------------
# 2. Lectura de VPC ------------------
#'-------------------------------------------------------------------------------
source(file.path('src', '016_pcVPC_Monolix.R'), encoding = 'UTF-8')

dataTipo1 <- dataTipo %>% 
  mutate(YTYPE = case_when(
    YTYPE == 'Microbiológico' ~ 'Bioassay', 
    YTYPE == 'Quimioluminiscencia' ~ 'Quimioluminiscence',
    TRUE ~ NA_character_
  ))




vpcTipo1  <- crearVPC(dataTipo1) +
  xlab('TAD, Time After Dose') + 
  ylab('Prediction corrected \nconcentration (mg/L)')



layout <- "
AABB
CCCC
"

G_TOTAL <- G_PRED_OBS_PRED + G_PRED_OBS_IPRED + vpcTipo1 + 
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'A')


G_TOTAL


# Almacenamiento en formato PDF
ggsave(file.path(project_dir, 'figures', '100_mosaicArticle.pdf'), G_TOTAL, 'pdf',
       width = 9.5, height = 8.5)
ggsave(file.path(project_dir, 'figures', '100_mosaicArticle.png'), G_TOTAL, 'png',
       width = 9.5, height = 8.5)
saveRDS(G_TOTAL,
        file.path(project_dir, 'figures', '100_mosaicArticle.rds'))








