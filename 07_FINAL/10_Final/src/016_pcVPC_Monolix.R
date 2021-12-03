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

project_dir <- file.path('.', 'FINAL')
vpc_dir <- file.path(project_dir, 'ChartsData', 'VisualPredictiveCheck')

# Selección de tema
theme_set(theme_bw() +
            theme(panel.border = element_rect(fill = NULL, colour = 'black')))


dataBins1 <- read_csv(file.path(vpc_dir, 'y1_bins.txt'))
dataBins2 <- read_csv(file.path(vpc_dir, 'y2_bins.txt'))

dataTipo1 <- read_csv(file.path(vpc_dir, 'y1_percentiles.txt')) %>% 
  add_column(xmin = dataBins1$bins_values[1:7],
             xmax = dataBins1$bins_values[2:8])

dataTipo2 <- read_csv(file.path(vpc_dir, 'y2_percentiles.txt')) %>% 
  add_column(xmin = dataBins2$bins_values[1:7],
             xmax = dataBins2$bins_values[2:8])



dataTipo <- bind_rows(
  dataTipo1 %>% add_column(YTYPE = 'Microbiológico'),
  dataTipo2 %>% add_column(YTYPE = 'Quimioluminiscencia')
)

#-------------------------------------------------------------------------------#
# 2. Creación de gráficos -----------------------------------------------------
#-------------------------------------------------------------------------------#

crearVPC <- function(dataTipo) {
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
    geom_line(aes(y = theoretical_lower_median_pc), lty='dashed') +
    geom_line(aes(y = theoretical_median_median_pc), lty='dashed') +
    geom_line(aes(y = theoretical_upper_median_pc), lty='dashed') +
    linedots(dataTipo, bins_middles, empirical_median) +
    linedots(dataTipo, bins_middles, empirical_lower) +
    linedots(dataTipo, bins_middles, empirical_upper) +
    facet_wrap(. ~ YTYPE) +
    xlab('TAD, tiempo tras dosis (h)') +
    ylab('Concentración plasmática VAN \n Corregida por predicción (mg/L)') +
    scale_x_continuous(breaks = seq(0, 12, 2)) +
    coord_cartesian(ylim = c(0, 80), xlim = c(0, 12)) 
}

vpcTipo  <- crearVPC(dataTipo)


#-------------------------------------------------------------------------------#
# Almacenamiento en formato PDF
ggsave(file.path(project_dir, 'figures', '016_pcVPC_IC.pdf'), vpcTipo, 'pdf',
       width = 6, height = 3.5)
