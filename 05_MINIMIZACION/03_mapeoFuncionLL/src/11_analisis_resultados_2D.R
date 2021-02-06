##------------------------------------------------------------------------------#
## Nombre del Script: Análisis de mapeo de la función objetivo -----------------
##  
## Propósito del Script: El mapeo de OFV permite conocer si se ha alcanzado 
## un mínimo global del modelo, además permite conocer IC no asintóticos 
## alrededor de los parámetros del modelo. En este script se analizan los 
## resultados obtenidos mediante la evaluación de verosimilitud con la suite 
## de Monolix. En este script se leen datos y se produce un gráfico con 
## funciones de optimización.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  07-03-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

# Carga de paquetes
require(patchwork)
require(rlang)
require(tidyverse)
require(gt)
require(plotly)

#-------------------------------------------------------------------------------#
# Introducción ------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Definición de directorio principal
aux_dir =  file.path(getwd(), 'results')
# Lectura de archivo funciones
source('src/20_funciones_resultados.R', encoding = 'UTF-8')
# Directorio externo donde se ubica el modelo base 1
dirModeloBase <- file.path(
  "..", "..", "04_CORRELACION"
)
# Apertura de archivo de datos de parámetros de modelo base final
populationParameters <-
  read_csv(file.path(dirModeloBase, 'M2CPTM_nobs_2_aditv_corr2', "populationParameters.txt"))

popParam <- populationParameters %>% 
  select(parameter, value) %>% 
  pivot_wider(names_from = parameter, values_from = value)

#-------------------------------------------------------------------------------#
# 1. Lectura de archivos de verosimilitud ------------------------------------
#-------------------------------------------------------------------------------#
df_Clpop_V1pop       <- extractor('Cl_pop_V1_pop', n=900)
df_V1_pop_V2_pop     <- extractor('V1_pop_V2_pop', n=900)
df_Cl_pop_Q_pop      <- extractor('Cl_pop_Q_pop', n=900)
df_Q_pop_V2_pop      <- extractor('Q_pop_V2_pop', n=900)
df_omega_V1_omega_V2 <- extractor('omega_V1_omega_V2', n=900)

# Selección de tema
theme_set(theme_classic() +
            theme(panel.border = element_rect(fill = NA, colour = 'black')))

#-------------------------------------------------------------------------------#
# 2. Gráficos de contornos ggplot2 ------------------------------------
#-------------------------------------------------------------------------------#

grafico_2D_LS <- list()

grafico_2D_LS[[1]] <- 
  generarGrafico2D(df_Clpop_V1pop, popParam, 'Cl_pop', 'V1_pop', 'LL1', 
                 expression(CL~'(L/h)'), bquote(V[1]~'(L)'), n_bins = 6)

grafico_2D_LS[[2]] <- 
generarGrafico2D(df_V1_pop_V2_pop, popParam, 'V1_pop', 'V2_pop', 'LL1', 
                 bquote(V[1]~'(L)'), bquote(V[2]~'(L)'), n_bins = 6)

grafico_2D_LS[[3]] <- 
generarGrafico2D(df_Cl_pop_Q_pop, popParam, 'Cl_pop', 'Q_pop', 'LL1', 
                 bquote(Cl~'(L/h)'), bquote(Q~'(L/h)'), n_bins = 6)

grafico_2D_LS[[4]] <- 
generarGrafico2D(df_Q_pop_V2_pop, popParam, 'Q_pop', 'V2_pop', 'LL1', 
                 bquote(Q~'(L/h)'), bquote(V[2]~'(L)'), n_bins = 6)

grafico_2D_LS[[5]] <- 
generarGrafico2D(df_omega_V1_omega_V2, popParam, 'omega_V1', 'omega_V2', 'LL1', 
                 bquote(omega[V[1]]), bquote(omega[V[2]]), n_bins = 6)


r_G_ll1 <- purrr::reduce(grafico_2D_LS, `+`) +
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A')

# Almacenamiento de gráfico
ggsave(r_G_ll1, filename = 'figures/06_perfiles_LL_2D.pdf', device = 'pdf', 
       width = 9, height = 8)


#-------------------------------------------------------------------------------#
# 3. Gráficos interactivos -----------------------------
#-------------------------------------------------------------------------------#

plotly_2D_LS <- list()

plotly_2D_LS[[1]] <- 
  generarGrafico2D(df_Clpop_V1pop, popParam, 'Cl_pop', 'V1_pop', 'LL1', 
                   'CL (L/h)', 'V<sub>2</sub> (L)', 
                   n_bins = 10, tipo = 'plotly')

plotly_2D_LS[[2]] <- 
  generarGrafico2D(df_V1_pop_V2_pop, popParam, 'V1_pop', 'V2_pop', 'LL1', 
                   'V<sub>1</sub> (L)', 'V<sub>2</sub> (L)', 
                   n_bins = 10, tipo = 'plotly')

plotly_2D_LS[[3]] <- 
  generarGrafico2D(df_Cl_pop_Q_pop, popParam, 'Cl_pop', 'Q_pop', 'LL1', 
                   "Cl (L/h)", "Q (L/h)", 
                   n_bins = 10, tipo = 'plotly')

plotly_2D_LS[[4]] <- 
  generarGrafico2D(df_Q_pop_V2_pop, popParam, 'Q_pop', 'V2_pop', 'LL1', 
                   "Q (L/h)", 'V<sub>2</sub> (L)', 
                   n_bins = 10, tipo = 'plotly')

plotly_2D_LS[[5]] <- 
  generarGrafico2D(df_omega_V1_omega_V2, popParam, 'omega_V1', 'omega_V2', 'LL1', 
                   TeX('\\omega_{V_1}'), TeX("\\omega_{V_2}"), 
                   n_bins = 10, tipo = 'plotly')

# Composición de gráficos Plotly
r_G_ll2 <- subplot(plotly_2D_LS, nrows = 3, 
        widths = c(.45, .45), 
        heights = rep(0.33, 3), 
        margin = c(0.08,0.08,0.055,0.055),
        titleX = TRUE, titleY = TRUE) %>%
  config(.Last.value, mathjax = "cdn")

# Almacenar gráfico como HTML
htmlwidgets::saveWidget(
  as_widget(r_G_ll2),
  normalizePath(file.path('figures', '07_perfiles_LL_2D.html')))
