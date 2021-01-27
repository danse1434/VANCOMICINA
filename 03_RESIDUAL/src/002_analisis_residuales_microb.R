##------------------------------------------------------------------------#
## Nombre del Script: Análisis de distribución de errores residuales ---
##  
## Propósito del Script: Análisis de sesgo con métodos de censura de datos
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  06-feb-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(rlang)
require(ggrepel)
require(readxl)
require(patchwork)
require(gt)

# Selección de tema
theme_set(theme_bw())
# Ejecutar script de funciones
source(file.path('src', '009_funciones.R'), encoding = 'UTF-8')

#-------------------------------------------------------------------------------#
# 1 Introducción ------------------
#-------------------------------------------------------------------------------#
# Abrir archivos de residuales

resid_vector <- c('aditv', 'prop', 'comb1', 'comb2', 'combP')
res_nobs1 <- vector('list', 5L)
res_nobs2 <- vector('list', 5L)

for (i in seq_along(resid_vector)) {
  res_nobs1[[i]] <-
    read_csv(
      file.path(
        paste0('M2CPTM_nobs_2_', resid_vector[i]),
        paste0('M2CPTM_nobs_2_', resid_vector[i]),
        'ChartsData', 'ScatterPlotOfTheResiduals', 'y1_residuals.txt'
      ), col_types = cols()
    )
}

for (i in seq_along(resid_vector)) {
  res_nobs2[[i]] <-
    read_csv(
      file.path(
        paste0('M2CPTM_nobs_2_', resid_vector[i]),
        paste0('M2CPTM_nobs_2_', resid_vector[i]),
        'ChartsData', 'ScatterPlotOfTheResiduals', 'y2_residuals.txt'
      ), col_types = cols()
    )
}

res_nobs1 <- res_nobs1 %>% 
  map_dfr(~.x, .id = 'Resid') %>% 
  mutate(Resid = resid_vector[as.double(Resid)])

res_nobs2 <- res_nobs2 %>% 
  map_dfr(~.x, .id = 'Resid') %>% 
  mutate(Resid = resid_vector[as.double(Resid)])

#-------------------------------------------------------------------------------#
# 2 Residuales -----------------
#-------------------------------------------------------------------------------#

res_nobs <- bind_rows(
  res_nobs1 %>% add_column(YTYPE = 1),
  res_nobs2 %>% add_column(YTYPE = 2)
)

crearGraficoResiduales <- function(df, criteria, filt, ...) {
  c_quo = ensym(criteria)
  f_quo = ensym(filt)
  
  df1 <- df %>% 
    filter(!!c_quo == filt)
  
  RES_COMB(df1, ..., legend = 'bottom')
}

# RES_COMB(res_nobs, time, pwRes, YTYPE, ID, 7, 
#           xlab='TAD (h)', ylab='PWRES', ylim=c(-4, 4), legend = 'bottom')

PWRES_T_ls <- map(resid_vector,
                  ~ crearGraficoResiduales(
                  res_nobs, Resid, .x, time, pwRes, YTYPE, ID, 7, 
                  xlab='TAD (h)', ylab='PWRES', ylim=c(-4, 4)))

IWRES_T_ls <- map(resid_vector, 
                ~ crearGraficoResiduales(
                  res_nobs, Resid, .x, time, iwRes_mean, YTYPE, ID, 7, 
                  xlab='TAD (h)', ylab='IWRES', ylim=c(-4, 4)))

NPDE_T_ls <- map(resid_vector, 
                ~ crearGraficoResiduales(
                  res_nobs, Resid, .x, time, npde, YTYPE, ID, 7, 
                  xlab='TAD (h)', ylab='NPDE', ylim=c(-4, 4)))

RES_T_ls <- PWRES_T_ls %>% 
  append(IWRES_T_ls) %>% 
  append(NPDE_T_ls) %>% 
  reduce(., `+`) + plot_layout(ncol = 5) 

gtitle1 <- wrap_elements(grid::textGrob( expression(bold('Aditivo'))) ) + 
  wrap_elements(grid::textGrob( expression(bold('Proporcional')))) +
  wrap_elements(grid::textGrob( expression(bold('Combinado 1')))) +
  wrap_elements(grid::textGrob( expression(bold('Combinado 2')))) +
  wrap_elements(grid::textGrob( expression(bold('Combinado P')))) +
  plot_layout(ncol = 5)

RES_T_ls_total <- (gtitle1 / RES_T_ls) + 
  plot_layout(
    heights = unit(c(0.05, 1-0.1), c('npc', 'null')), 
    guides = 'collect') & 
  theme(legend.position = 'bottom')

# Almacenamiento
ggsave(file.path('figures', '011_res_TAD_microb.pdf'), RES_T_ls_total, 'pdf', 
       width = 10, height = 7)


#-------------------------------------------------------------------------------#
# Residuales de concentración

PWRES_C_ls <- map(resid_vector,
                  ~ crearGraficoResiduales(
                    res_nobs, Resid, .x, prediction_pwRes, pwRes, YTYPE, ID, 7, 
                    xlab='PRED (mg/L)', ylab='PWRES', ylim=c(-4, 4)))

IWRES_C_ls <- map(resid_vector, 
                  ~ crearGraficoResiduales(
                    res_nobs, Resid, .x, prediction_iwRes_mean, iwRes_mean, YTYPE, ID, 7, 
                    xlab='IPRED (mg/L)', ylab='IWRES', ylim=c(-4, 4)))

NPDE_C_ls <- map(resid_vector, 
                 ~ crearGraficoResiduales(
                   res_nobs, Resid, .x, prediction_npde, npde, YTYPE, ID, 7, 
                   xlab='PRED (mg/L)', ylab='NPDE', ylim=c(-4, 4)))

RES_C_ls <- PWRES_C_ls %>% 
  append(IWRES_C_ls) %>% 
  append(NPDE_C_ls) %>% 
  reduce(., `+`) + plot_layout(ncol = 5) 

RES_C_ls_total <- (gtitle1 / RES_C_ls) + 
  plot_layout(
    heights = unit(c(0.05, 1-0.1), c('npc', 'null')), 
    guides = 'collect') & 
  theme(legend.position = 'bottom')

ggsave(file.path('figures', '012_res_PRE_microb.pdf'), RES_C_ls_total, 'pdf', 
       width = 10, height = 7)


#-------------------------------------------------------------------------------#
# 3 Normalidad en los residuales -----------------------
#-------------------------------------------------------------------------------#
# Se realizan pruebas de normalidad univariadas para los residuales en cada 
# uno modelo.
#................................................................................
#' 1 Seleccionar columnas con datos de _residuales_: *PWRES*, *IWRES*, y *NPDE*
#' 2 Colapsar tabla con _residuales_ en filas
#' 3 Agrupar por **Resid** (modelo) y *Residuales* (tipo de residual)
#' 4 Anidar vector de residuales en grupos
#' 5 Calcular valores p de residuales
#' 6-11 Extraer valores p de cada test en *Norm*
#' 12 Desagrupar
#' 13 Localizar *Residuales* al principio
#' 14 Convertir a *Residuales* en un factor ordenado
#' 15 Recodificar a la variable *Resid*
#' 16 Convertir a *Resid* en un factor ordenado
#................................................................................
res_norm <- res_nobs %>% 
  select(Resid, pwRes, iwResMean = iwRes_mean, iwResMode = iwRes_mode, npde) %>% 
  pivot_longer(cols = -Resid, names_to = 'Residuales', values_to = 'Val') %>% 
  group_by(Resid, Residuales) %>%
  nest() %>%
  mutate(
    Norm = map(data, ~ normtest_battery(.x, 'Val')),
    SW = map_dbl(Norm, 'SW'),
    AD = map_dbl(Norm, 'AD'),
    CM = map_dbl(Norm, 'CM'),
    Lf = map_dbl(Norm, 'Lf'),
    Pe = map_dbl(Norm, 'Pe'),
    SF = map_dbl(Norm, 'SF')
  ) %>% 
  ungroup() %>% 
  relocate(Residuales, .before = Resid) %>% 
  mutate(
    Residuales = factor(Residuales, levels = c('pwRes', 'iwResMean', 'iwResMode', 'npde')),
    Resid = case_when(
      Resid == 'aditv' ~ 'Aditivo',
      Resid == 'prop' ~ 'Proporcional',
      Resid == 'comb1' ~ 'Combinado 1',
      Resid == 'comb2' ~ 'Combinado 2',
      Resid == 'combP' ~ 'Combinado P',
      TRUE ~ NA_character_
    ),
    Resid = factor(Resid,
                   levels = c('Aditivo', 'Proporcional', 'Combinado 1', 'Combinado 2', 'Combinado P'))
  )

# Convertir en gt()
gt_res_norm <- res_norm %>% 
  arrange(Residuales, Resid) %>% 
  select(-data, -Norm) %>% 
  gt() %>% 
  tab_header(title = md('**Batería de Pruebas de Normalidad - Modelo de Residuales**'),
             subtitle = md('**Modelos con determinaciones por quimioluminiscencia y microbiología**')) %>% 
  cols_label(
    Residuales = md('**Residuales**'),
    Resid = md('**Modelo**'),
    SW = md('**Shapiro-Wilk**'),
    AD = md('**Andersen-Darling**'),
    CM = md('**Cramer von Mises**'),
    Lf = md('**Liliefors**'),
    Pe = md('**Pearson**'),
    SF = md('**Shapiro-Francia**')
  ) %>% 
  fmt_scientific(3:8, decimals = 2) %>% 
  cols_align('left', 1:2) %>% 
  tab_spanner(md('**Prueba Normalidad**'), 3:8) %>% 
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  )
# Almacenar en gt()
gt_res_norm %>% 
  gtsave('013_res_normalidad_microb.html', file.path('figures') %>% normalizePath())

#-------------------------------------------------------------------------------#
# 4 Gráficos QQ -------------------------------------------
#-------------------------------------------------------------------------------#
# Función para obtener gráfico de tipo QQ con línea y bandas, dado el modelo 
# el parámetro deseado y color opcional.
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Convertir param en expresión
##  2 Crear gráfico
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

plot_resid_comp <- res_norm %>% 
  select(Residuales, Resid, data) %>% 
  unnest(data) %>% 
  ggplot(aes(sample = Val)) +
  facet_grid(Resid ~ Residuales, labeller = labeller(Residuales = toupper)) +
  geom_abline(slope = 1, intercept = 0, lty = 'dashed') +
  stat_qq(shape = 16) + 
  qqplotr::stat_qq_line(distribution = 'norm', mapping = aes(col = Residuales)) +
  qqplotr::stat_qq_band(distribution = 'norm', mapping = aes(fill = Residuales), alpha=0.1) +
  coord_cartesian(xlim = c(-3, 5), ylim = c(-3, 5)) + 
  scale_x_continuous(breaks = seq(-4, 4, 2), 
                     minor_breaks = seq(-5, 5, 1)) +
  scale_color_manual(values = c('blue', 'red', 'purple', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'purple', 'green')) +
  theme(
    legend.position = 'none',
    panel.grid.major = element_line(color = 'gray85'),
    panel.grid.minor = element_line(color = 'gray93')
  ) +
  xlab('Cuantiles teóricos') + ylab('Cuantiles de muestreo')

ggsave('014_res_qqplot_microb.pdf', plot_resid_comp, 'pdf', 
       'figures', 1, 8, 6, 'in')

#-------------------------------------------------------------------------------#
# 5 Distribución Residuales ---------------
#-------------------------------------------------------------------------------#
#................................................................................
#' 1 Seleccionar modelos *Resid*, tipos de residuales *Residuales*, datos *data*
#' 2 Calcular media (*mean*) y desviación estándar (*sd*)
#' 3 Calcular función de distribución de densidad y densidad acumulada
#' 4 Eliminar *data*
#' 5 Desanidar *dn* para obtener densidad en cada columna
#................................................................................

res_norm_hist <- res_norm %>%
  select(Residuales, Resid, data) %>% 
  mutate(mean = map_dbl(data, ~ mean(.x$Val, na.rm = TRUE)),
         sd   = map_dbl(data, ~ sd(.x$Val, na.rm = TRUE)) ) %>% 
  mutate(dn = pmap(list(mn = mean, sd = sd),
                   function(mn, sd) {
                     data.frame(x = seq(-5, 5, 0.1),
                                y = dnorm(seq(-5, 5, 0.1), mn, sd),
                                z = pnorm(seq(-5, 5, 0.1), mn, sd))
                   })) %>% 
  select(-data) %>% 
  unnest(dn)

plot_hist_comp <- res_norm %>% 
  select(Residuales, Resid, data, SW) %>% 
  unnest(data) %>% 
  ggplot(aes(x = Val, col = Residuales)) +
  facet_grid(Resid ~ Residuales, 
             labeller = labeller(Residuales = toupper)) +
  geom_histogram(aes(fill = after_scale(alpha(col, 0.1)),
                     y = ..density..), bins = 20) + 
  geom_density(aes(y = ..density..)) +
  geom_line(res_norm_hist, 
            mapping = aes(x = x, y = y),lty = "dashed", col = 'black') +
  scale_color_manual(values = c('blue', 'red', 'purple', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'purple', 'green')) +
  geom_label(
    data = res_norm,
    mapping = aes(label = paste0(
      'Shapiro-Wilk: \n p = ', formatC(SW, 2, format = 'e')
    )), y = 0.95, x = 3.0, hjust=.5, vjust=1, size=2, colour='black' ) +
  coord_cartesian(ylim=c(0, 1)) +
  theme(
    legend.position = 'none',
    panel.grid.major = element_line(color = 'gray85'),
    panel.grid.minor = element_line(color = 'gray93')
  ) + 
  xlab('Valor') + ylab('Densidad')

ggsave('015_res_histogramas_microb.pdf', plot_hist_comp, 'pdf', 
       'figures', 1, 8, 6, 'in')

plot_acum_comp <- res_norm %>% 
  select(Residuales, Resid, data, SW) %>% 
  unnest(data) %>% 
  ggplot(aes(x = Val, col = Residuales)) +
  facet_grid(Resid ~ Residuales, 
             labeller = labeller(Residuales = toupper)) +
  stat_ecdf(geom = "step") + 
  geom_line(res_norm_hist, 
            mapping = aes(x = x, y = z),lty = "solid", col = 'black')+
  theme(
    legend.position = 'none',
    panel.grid.major = element_line(color = 'gray85'),
    panel.grid.minor = element_line(color = 'gray93')
  ) + 
  xlab('Valor') + ylab('Densidad Acumulada') +
  scale_color_manual(values = c('blue', 'red', 'purple', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'purple', 'green'))

ggsave('016_res_ecdf_microb.pdf', plot_acum_comp, 'pdf', 
       'figures', 1, 8, 6, 'in')
