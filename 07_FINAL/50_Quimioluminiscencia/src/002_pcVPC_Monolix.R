##------------------------------------------------------------------------------#
## Nombre del Script: Lectura de pcVPC generado por Monolix para 
## Modelo Base de VAN ----------------------------------------------------------
##  
## Propósito del Script:  Generar una visualización alternativa de VPC para 
## modelo base con la herramienta incorporada en Monolix
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  03-02-2021
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(patchwork)

source('./src/052_fun_pcVPC.R', encoding = 'UTF-8')

#-------------------------------------------------------------------------------#
# 1. Lectura de datos -----------------------------------------------------
#-------------------------------------------------------------------------------#

project_dir <- file.path('.', 'run200')
vpc_dir <- file.path(project_dir, 'ChartsData', 'VisualPredictiveCheck')

# Selección de tema
theme_set(theme_bw() +
            theme(panel.border = element_rect(fill = NULL, colour = 'black')))


dataBins <- read_csv(file.path(vpc_dir, 'y2_bins.txt'))

dataTipo <- read_csv(file.path(vpc_dir, 'y2_percentiles.txt')) %>% 
  add_column(xmin = dataBins$bins_values[1:7],
             xmax = dataBins$bins_values[2:8])

dataObservaciones <- read_csv(file.path(vpc_dir, 'y2_observations.txt'))

#-------------------------------------------------------------------------------#
# 2. Creación de gráficos -----------------------------------------------------
#-------------------------------------------------------------------------------#

crearVPC <- function(dataTipo, dataObservaciones) {
  dataTipo %>%
    ggplot(aes(x = bins_middles)) +
    geom_rect(aes(ymin=theoretical_median_piLower_pc,
                  ymax=theoretical_median_piUpper_pc,
                  xmin=xmin, xmax=xmax), alpha = 0.5, fill = 'gray70') +
    geom_rect(aes(ymin=theoretical_lower_piLower_pc,
                  ymax=theoretical_lower_piUpper_pc,
                  xmin=xmin, xmax=xmax), alpha = 0.5, fill = 'gray30') +
    geom_rect(aes(ymin=theoretical_upper_piLower_pc ,
                  ymax=theoretical_upper_piUpper_pc ,
                  xmin=xmin, xmax=xmax), alpha = 0.5, fill = 'gray30') +
    geom_point(data = dataObservaciones, aes(x = time, y = y2_pc), col = "gray20") + 
    geom_line(aes(y = theoretical_lower_median_pc), lty='dashed') +
    geom_line(aes(y = theoretical_median_median_pc), lty='dashed') +
    geom_line(aes(y = theoretical_upper_median_pc), lty='dashed') +
    linedots(dataTipo, bins_middles, empirical_median) +
    linedots(dataTipo, bins_middles, empirical_lower) +
    linedots(dataTipo, bins_middles, empirical_upper) +
    # facet_wrap(. ~ YTYPE) +
    xlab('TAD, tiempo tras dosis (h)') +
    ylab('Concentración plasmática VAN \n Corregida por predicción (mg/L)') +
    scale_x_continuous(breaks = seq(0, 12, 2)) +
    coord_cartesian(ylim = c(0, 60), xlim = c(0, 12)) 
}

vpcTipo  <- crearVPC(dataTipo, dataObservaciones)
vpcTipo

#-------------------------------------------------------------------------------#
# Almacenamiento en formato PDF
ggsave(file.path(project_dir, 'figures', 'pcVPC_IC.pdf'), vpcTipo, 'pdf',
       width = 6, height = 0.65 * 6)

saveRDS(vpcTipo, file.path(project_dir, 'figures', 'pcVPC_IC.rds'))
