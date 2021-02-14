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

#-------------------------------------------------------------------------------#
# Introducción ------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Carga de paquetes
require(patchwork)
require(rlang)
require(tidyverse)
require(gt)

# Definición de directorio principal
aux_dir =  file.path(getwd(), 'results')
# Lectura de archivo funciones
source(file.path('src', '20_funciones_resultados.R'), encoding = 'UTF-8')
# Directorio externo donde se ubica el modelo base 1
dirModeloBase <- file.path(".")

# Apertura de archivo de datos de parámetros de modelo base final
populationParameters <-
  read_csv(file.path(dirModeloBase, 'modeloFinal', "populationParameters.txt"))

popParam <- populationParameters %>% 
  select(parameter, value) %>% 
  pivot_wider(names_from = parameter, values_from = value)

#-------------------------------------------------------------------------------#
# Lectura de archivos de verosimilitud ------------------------------------
#-------------------------------------------------------------------------------#
df_Clpop              <- extractor('Cl_pop')
df_beta_Cl_tCLCRMLMIN <- extractor('beta_Cl_tCLCRMLMIN')
df_beta_Cl_logtWTKG   <- extractor('beta_Cl_logtWTKG')
df_corr_V2_V1         <- extractor('corr_V2_V1')
# df_Qpop  <- extractor('Q_pop')
# df_V1pop <- extractor('V1_pop')
# df_V2pop <- extractor('V2_pop')

# Cálculo de valores mínimos y adición de 3.84
df_Clpop1              <- df_Clpop %>% xval.func(Cl_pop, LL1)
df_beta_Cl_tCLCRMLMIN1 <- df_beta_Cl_tCLCRMLMIN %>% xval.func(beta_Cl_tCLCRMLMIN, LL1)
df_beta_Cl_logtWTKG1   <- df_beta_Cl_logtWTKG %>% xval.func(beta_Cl_logtWTKG, LL1)
df_corr_V2_V11         <- df_corr_V2_V1 %>% xval.func(corr_V2_V1, LL1)

# df_Qpop1  <- df_Qpop %>% xval.func(Q_pop, LL1)
# df_V1pop1 <- df_V1pop %>% xval.func(V1_pop, LL1)
# df_V2pop1 <- df_V2pop %>% xval.func(V2_pop, LL1)

# Selección de tema
theme_set(theme_classic() +
            theme(panel.border = element_rect(fill = NA, colour = 'black')))

# Gráficos
aux_plot <- list(geom_line(),
                 ylab('LL - min(LL)'),
                 geom_hline(yintercept = 3.84, lty = 'dotted'))

G_ll_1 <- ggplot(df_Clpop, aes(x = Cl_pop, y = LL1)) +
  geom_point(data = df_Clpop1$Minimo, col = 'red3', shape = 8) +
  geom_point(data = df_Clpop1$Punto_Corte, col = 'green1') +
  geom_vline(data = popParam, aes(xintercept = Cl_pop), lty='dashed', col='blue4') + 
  xlab(expression('Cl (L/h)')) + aux_plot

G_ll_2 <- ggplot(df_beta_Cl_tCLCRMLMIN, aes(x = beta_Cl_tCLCRMLMIN, y = LL1)) +
  geom_point(data = df_beta_Cl_tCLCRMLMIN1$Minimo, col = 'red3', shape = 8) +
  geom_point(data = df_beta_Cl_tCLCRMLMIN1$Punto_Corte, col = 'green1') +
  geom_vline(data = popParam, aes(xintercept = beta_Cl_tCLCRMLMIN), lty='dashed', col='blue4') + 
  xlab(expression(beta~'Cl-tClCr')) + aux_plot

G_ll_3 <- ggplot(df_beta_Cl_logtWTKG, aes(x = beta_Cl_logtWTKG, y = LL1)) +
  geom_point(data = df_beta_Cl_logtWTKG1$Minimo, col = 'red3', shape = 8) +
  geom_point(data = df_beta_Cl_logtWTKG1$Punto_Corte, col = 'green1') +
  geom_vline(data = popParam, aes(xintercept = beta_Cl_logtWTKG), lty='dashed', col='blue4') + 
  xlab(expression(beta~'Cl-logtWT')) + aux_plot

G_ll_4 <- ggplot(df_corr_V2_V1, aes(x = corr_V2_V1, y = LL1)) +
  geom_point(data = df_corr_V2_V11$Minimo, col = 'red3', shape = 8) +
  geom_point(data = df_corr_V2_V11$Punto_Corte, col = 'green1') +
  geom_vline(data = popParam, aes(xintercept = corr_V2_V1), lty='dashed', col='blue4') + 
  xlab(expression('corr_V2_V1')) + aux_plot


# G_ll_2 <- ggplot(df_Qpop, aes(x = Q_pop, y = LL1)) +
#   geom_point(data = df_Qpop1$Minimo, col='red3', shape = 8) +
#   geom_point(data = df_Qpop1$Punto_Corte, col='green1') +
#   geom_vline(data = popParam, aes(xintercept = Q_pop), lty='dashed', col='blue4') + 
#   xlab(expression('Q (L/h)')) + aux_plot
# 
# G_ll_3 <- ggplot(df_V1pop, aes(x = V1_pop, y = LL1)) +
#   geom_point(data = df_V1pop1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = df_V1pop1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = V1_pop), lty='dashed', col='blue4') + 
#   xlab(expression(V[1]~'(L)')) + aux_plot
# 
# G_ll_4 <- ggplot(df_V2pop, aes(x = V2_pop, y = LL1)) +
#   geom_point(data = df_V2pop1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = df_V2pop1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = V2_pop), lty='dashed', col='blue4') + 
#   xlab(expression(V[2]~'(L)')) + aux_plot

G_ll <- ((G_ll_1 + G_ll_2) / (G_ll_3 + G_ll_4)) + 
  plot_annotation(tag_levels = 'A')

# Almacenamiento de gráfico
ggsave(G_ll, filename = 'figures/01_perfiles_LL_1D.pdf', device = 'pdf', 
       width = 7, height = 5)

#-------------------------------------------------------------------------------#
# Bandas de Verosimilitud -----------------------------------------------------
#-------------------------------------------------------------------------------#
x_Clpop              <- extractor('Cl_pop', mode='interval')
x_beta_Cl_tCLCRMLMIN <- extractor('beta_Cl_tCLCRMLMIN', mode='interval')
x_beta_Cl_logtWTKG   <- extractor('beta_Cl_logtWTKG', mode='interval')
x_corr_V2_V1         <- extractor('corr_V2_V1', mode='interval')

# x_Qpop  <- extractor('Q_pop', mode='interval')
# x_V1pop <- extractor('V1_pop', mode='interval')
# x_V2pop <- extractor('V2_pop', mode='interval')

# Cálculo de valores mínimos y adición de 3.84
x_Clpop1              <- x_Clpop %>% xval.func(Cl_pop, LL_mn)
x_beta_Cl_tCLCRMLMIN1 <- x_beta_Cl_tCLCRMLMIN %>% xval.func(beta_Cl_tCLCRMLMIN, LL_mn)
x_beta_Cl_logtWTKG1   <- x_beta_Cl_logtWTKG %>% xval.func(beta_Cl_logtWTKG, LL_mn)
x_corr_V2_V11         <- x_corr_V2_V1 %>% xval.func(corr_V2_V1, LL_mn)
# x_Qpop1  <- x_Qpop  %>% xval.func(Q_pop, LL_mn)
# x_V1pop1 <- x_V1pop %>% xval.func(V1_pop, LL_mn)
# x_V2pop1 <- x_V2pop %>% xval.func(V2_pop, LL_mn)

x_ll_1 <- ggplot(x_Clpop, aes(x = Cl_pop, y = LL_mn)) +
  geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
  geom_point(data = x_Clpop1$Minimo, col = 'red3', shape = 8) +
  geom_point(data = x_Clpop1$Punto_Corte, col = 'green1') +
  geom_vline(data = popParam, aes(xintercept = Cl_pop), lty='dashed', col='blue4') + 
  xlab(expression('Cl (L/h)')) + aux_plot

x_ll_2 <- ggplot(x_beta_Cl_tCLCRMLMIN, aes(x = beta_Cl_tCLCRMLMIN, y = LL_mn)) +
  geom_point(data = x_beta_Cl_tCLCRMLMIN1$Minimo, col = 'red3', shape = 8) +
  geom_point(data = x_beta_Cl_tCLCRMLMIN1$Punto_Corte, col = 'green1') +
  geom_vline(data = popParam, aes(xintercept = beta_Cl_tCLCRMLMIN), lty='dashed', col='blue4') + 
  xlab(expression(beta~'Cl-tClCr')) + aux_plot

x_ll_3 <- ggplot(x_beta_Cl_logtWTKG, aes(x = beta_Cl_logtWTKG, y = LL_mn)) +
  geom_point(data = x_beta_Cl_logtWTKG1$Minimo, col = 'red3', shape = 8) +
  geom_point(data = x_beta_Cl_logtWTKG1$Punto_Corte, col = 'green1') +
  geom_vline(data = popParam, aes(xintercept = beta_Cl_logtWTKG), lty='dashed', col='blue4') + 
  xlab(expression(beta~'Cl-logtWT')) + aux_plot

x_ll_4 <- ggplot(x_corr_V2_V1, aes(x = corr_V2_V1, y = LL_mn)) +
  geom_point(data = x_corr_V2_V11$Minimo, col = 'red3', shape = 8) +
  geom_point(data = x_corr_V2_V11$Punto_Corte, col = 'green1') +
  geom_vline(data = popParam, aes(xintercept = corr_V2_V1), lty='dashed', col='blue4') + 
  xlab(expression('corr_V2_V1')) + aux_plot

# x_ll_2 <- ggplot(x_Qpop, aes(x = Q_pop, y = LL_mn)) +
#   geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
#   geom_point(data = x_Qpop1$Minimo, col='red3', shape = 8) +
#   geom_point(data = x_Qpop1$Punto_Corte, col='green1') +
#   geom_vline(data = popParam, aes(xintercept = Q_pop), lty='dashed', col='blue4') + 
#   xlab(expression('Q (L/h)')) + aux_plot
# 
# x_ll_3 <- ggplot(x_V1pop, aes(x = V1_pop, y = LL_mn)) +
#   geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
#   geom_point(data = x_V1pop1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = x_V1pop1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = V1_pop), lty='dashed', col='blue4') + 
#   xlab(expression(V[1]~'(L)')) + aux_plot
# 
# x_ll_4 <- ggplot(x_V2pop, aes(x = V2_pop, y = LL_mn)) +
#   geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
#   geom_point(data = x_V2pop1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = x_V2pop1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = V2_pop), lty='dashed', col='blue4') + 
#   xlab(expression(V[2]~'(L)')) + aux_plot

x_ll <- ((x_ll_1 + x_ll_2) / (x_ll_3 + x_ll_4)) + 
  plot_annotation(tag_levels = 'A')

# Almacenamiento de gráfico
ggsave(x_ll, filename = 'figures/03_perfiles_LL_1D.pdf', device = 'pdf', 
       width = 7, height = 5)

#-------------------------------------------------------------------------------#
# Todos los parámetros -------------------------------
#-------------------------------------------------------------------------------#
# x_omegaCl <- extractor('omega_Cl', mode='interval')
# x_omegaQ  <- extractor('omega_Q', mode='interval')
# x_omegaV1 <- extractor('omega_V1', mode='interval')
# x_omegaV2 <- extractor('omega_V2', mode='interval')
# x_a1      <- extractor('a1', mode='interval')
# x_a2      <- extractor('a2', mode='interval')
# 
# # Cálculo de valores mínimos y adición de 3.84
# x_omegaCl_1 <- x_omegaCl %>% xval.func(omega_Cl, LL_mn)
# x_omegaQ_1  <- x_omegaQ  %>% xval.func(omega_Q, LL_mn)
# x_omegaV1_1 <- x_omegaV1 %>% xval.func(omega_V1, LL_mn)
# x_omegaV2_1 <- x_omegaV2 %>% xval.func(omega_V2, LL_mn)
# x_a1_1      <- x_a1 %>% xval.func(a1, LL_mn)
# x_a2_1      <- x_a2 %>% xval.func(a2, LL_mn)
# 
# x_ll_5 <- ggplot(x_omegaCl, aes(x = omega_Cl, y = LL_mn)) +
#   geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
#   geom_point(data = x_omegaCl_1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = x_omegaCl_1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = omega_Cl), lty='dashed', col='blue4') + 
#   xlab(bquote(omega[Cl]^2)) + aux_plot
# 
# x_ll_6 <- ggplot(x_omegaQ, aes(x = omega_Q, y = LL_mn)) +
#   geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
#   geom_point(data = x_omegaQ_1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = x_omegaQ_1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = omega_Q), lty='dashed', col='blue4') + 
#   xlab(bquote(omega[Q]^2)) + aux_plot
# 
# x_ll_7 <- ggplot(x_omegaV1, aes(x = omega_V1, y = LL_mn)) +
#   geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
#   geom_point(data = x_omegaV1_1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = x_omegaV1_1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = omega_V1), lty='dashed', col='blue4') + 
#   xlab(bquote(omega[V1]^2)) + 
#   scale_x_continuous(guide=guide_axis(n.dodge = 2))+
#   aux_plot
# 
# x_ll_8 <- ggplot(x_omegaV2, aes(x = omega_V2, y = LL_mn)) +
#   geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
#   geom_point(data = x_omegaV2_1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = x_omegaV2_1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = omega_V2), lty='dashed', col='blue4') + 
#   xlab(bquote(omega[V2]^2)) + 
#   scale_x_continuous(guide=guide_axis(n.dodge = 2)) +
#   aux_plot
# 
# x_ll_9 <- ggplot(x_a1, aes(x = a1, y = LL_mn)) +
#   geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
#   geom_point(data = x_a1_1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = x_a1_1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = a1), lty='dashed', col='blue4') + 
#   xlab(bquote(a[1])) + aux_plot
# 
# x_ll_10 <- ggplot(x_a2, aes(x = a2, y = LL_mn)) +
#   geom_ribbon(aes(ymin=LL_li, ymax=LL_ls), fill=alpha('blue', 0.1)) +
#   geom_point(data = x_a2_1$Minimo, col = 'red3', shape = 8) +
#   geom_point(data = x_a2_1$Punto_Corte, col = 'green1') +
#   geom_vline(data = popParam, aes(xintercept = a2), lty='dashed', col='blue4') + 
#   xlab(bquote(a[2])) + aux_plot
# 
# x_ll_total <- paste0('x_ll_', 1:10) %>% 
#   paste(., collapse = ' + ') %>% 
#   parse_expr() %>% 
#   eval_bare() + 
#   plot_layout(ncol = 4) +
#   plot_annotation(tag_levels = 'A')
# 
# # Almacenamiento de gráfico
# ggsave(x_ll_total, filename = 'figures/04_perfiles_LL_1D_todos.pdf', device = 'pdf', 
#        width = 7*1.5, height = 5*1.5)
# 

# Resultados Tabulados ----------------------------------------------------
# Agrupar resultados
modelLs <- list()
modelLs[['Cl']]                 = x_Clpop1
modelLs[['beta_Cl_tCLCRMLMIN']] = x_beta_Cl_tCLCRMLMIN1
modelLs[['beta_Cl_logtWTKG']]   = x_beta_Cl_logtWTKG1
modelLs[['corr_V2_V1']]         = x_corr_V2_V11
# modelLs[['Q']]        = x_Qpop1
# modelLs[['V1']]       = x_V1pop1
# modelLs[['V2']]       = x_V2pop1
# modelLs[['omega_CL']] = x_omegaCl_1
# modelLs[['omega_Q']]  = x_omegaQ_1
# modelLs[['omega_V1']] = x_omegaV1_1
# modelLs[['omega_V2']] = x_omegaV2_1
# modelLs[['a1']]       = x_a1_1
# modelLs[['a2']]       = x_a2_1

modelLS_df_1 <- map_dfr(modelLs, 'Minimo') %>%
  pivot_longer(
    cols = !contains('LL_mn'),
    names_to = 'Parametros',
    values_to = 'Minimo',
    values_drop_na = TRUE
  )

modelLS_df_2 <-
  map_dfr(modelLs, 'Punto_Corte') %>%
  pivot_longer(
    cols = !contains('LL_mn'),
    names_to = 'Parametros',
    values_drop_na = TRUE
  ) %>% 
  add_column(ID = rep(c('LI', 'LS'), dim(.)[1]/2)) %>% 
  pivot_wider(names_from = ID, values_from=c(value, LL_mn))

#  Crear tablas

modelLS_gt <- modelLS_df_1 %>% 
  left_join(modelLS_df_2, by = 'Parametros') %>% 
  relocate(c(Minimo, LL_mn), .after=Parametros) %>% 
  relocate(c(value_LI, value_LS), .after=Minimo) %>% 
  gt(rowname_col = "Parametros") %>% 
  tab_header(md('**Resultados Verosimilitud**')) %>% 
  fmt_number(matches('\\_|Minimo'), decimals = 4) %>%
  cols_label(
    'Minimo'   = 'Mínimo',
    'value_LI' = 'Límite Inferior',
    'value_LS' = 'Límite Superior',
    'LL_mn'    = 'LL (min)',
    'LL_mn_LI' = 'LL (LI)',
    'LL_mn_LS' = 'LL (LS)'
    ) %>%
  tab_options(
    column_labels.font.size = "normal",
    table.font.size = "normal",
    data_row.padding = px(3), 
    heading.title.font.weight = 'bold', 
    column_labels.font.weight = 'bold'
  )

gtsave(modelLS_gt, '05_resultados_verosimilitud.htm', file.path(getwd(), 'figures'))
