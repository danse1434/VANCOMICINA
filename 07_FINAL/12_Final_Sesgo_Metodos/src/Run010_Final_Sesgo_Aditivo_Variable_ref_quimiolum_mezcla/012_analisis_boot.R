##------------------------------------------------------------------------------#
## Nombre del Script: Análisis de resultados de Bootstrap para modelo -----------
## final TBS con error aditivo. ----------------------------------------------------------
##  
## Propósito del Script:  Realizar un resumen de los resultados de bootstrap 
## para el modelo base VAN. 
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  17-03-2020
## Fecha de modificación:  11-08-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# Introducción -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Carga de paquetes
require(magrittr)
require(glue)
require(rlang)
require(tidyverse)
require(Rcpp)
require(RcppArmadillo)

# Cambio en directorio de trabajo
#-------------------------------------------------------------------------------#
auxdir  <- file.path('FINAL')
bootdir <- file.path(auxdir, 'results')
# project.file <- file.path(auxdir, 'M2CPTM_nobs_1_prop.mlxtran')
paramet.file <- file.path(auxdir)
# Compilar archivo C++
sourceCpp(file.path('src', '013_funciones_analisis_boot.cpp'))

#-------------------------------------------------------------------------------#
# Abrir archivo de parámetros originales (t0) -----------------------------------
#-------------------------------------------------------------------------------#
# Ajustar t0
# Se abre el archivo con los parámetros calculados con los datos originales, 
# esto corresponde a t0 en cada parámetro.

vectorParametros <- c('Cl_pop', 'beta_Cl_logtCLCRMLMIN',
                      'V1_pop', 'V2_pop', 'Q_pop',
                      'omega_V1', 'omega_V2', 'omega_Cl', 'omega_Q',
                      'corr_V2_V1',
                      'a1', 'a2', 'k10', 'k12', 'k21', 'alpha', 'beta',
                      't_alpha', 't_beta', 'A', 'B')

popParameters <- read_csv(file.path(paramet.file, 'populationParameters.txt'))

# Se calculan los parámetros secundarios a partir de los datos de t0 con el 
# fin de incluir estos datos en el cálculo de intervalos de confianza. 
popParameters1 <- popParameters %>% 
  pivot_wider(id_cols=parameter, names_from = parameter, values_from = value) %>% 
  mutate(
    List = pmap(list(1000, Cl_pop, Q_pop, V1_pop, V2_pop), constants_fun)
  ) %>%
  unnest(cols = c(List)) %>%
  pivot_longer(-contains('ID'), names_to = 'parameter', values_to = 'value') %>% 
  mutate(parameter = factor(parameter, level = vectorParametros))


#-------------------------------------------------------------------------------#
# Apertura de archivos de parámetros poblacionales ------------------------
#-------------------------------------------------------------------------------#
# Esta sección tiene como objetivo leer todos los archivos de parámetros 
# poblacionales en todos los subdirectorios. 
#................................................................................
#  1 Crear un objeto de tipo lista 
#  2 Almacenar en cada uno de sus elementos con una tabla leída de una 
#  dirección específica en el árbol de directorios.
#  3 Convertir el archivo lista en un data.frame con una columna para 
#  identificar de que carpeta proviene cada set de par?metros.
#................................................................................

param_list = vector('list', 1000L)

for (i in 1:1000) {
  param_list[[i]] = read_csv(
    glue('{bootdir}/B{i}/FINAL/populationParameters.txt'),
                             col_types = cols())
}

param_df <- param_list %>%
  map_dfr( ~ .x, .id = 'ID') %>% 
  as_tibble()

#-------------------------------------------------------------------------------#
# Manipulación de archivo de datos leído ----------------------------------
#-------------------------------------------------------------------------------#
# Manipulación de tabla con resultados de bootstrap no paramétrico
#................................................................................
#  1 Tomar el archivo de datos
#  2 Transformar el layout de los datos a una forma expandida con columnas 
#  para cada parámetro.
#  3 Calcular k10, k12, k21, alpha, beta, t_alpha, t_beta, A, y B. Con 
#  funciones definidas en situ, y funciones complejas micro.fun, y macro.fun
#  4 Transformar el layout de los datos a una forma compacta similar a la 
#  original.
#  5 Ordenar la tabla por ID.
#  6 Transformar la variable parameter en forma de un factor con ordenación 
#  específica.
#................................................................................

param_df1 <- param_df %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  mutate(
    List = pmap(list(1000, Cl_pop, Q_pop, V1_pop, V2_pop), constants_fun)
  ) %>%
  unnest(cols = c(List)) %>%
  pivot_longer(-contains('ID'), names_to = 'parameter', values_to = 'value') %>%
  arrange(ID) %>% 
  mutate(parameter = factor(parameter, level = vectorParametros))

#-------------------------------------------------------------------------------#
# Creación de gráficos-----------------------------------------------------
#-------------------------------------------------------------------------------#
# Selección de tema
theme_set(theme_classic() +
            theme(panel.border = element_rect(fill = NA, colour = 'black')))

#-------------------------------------------------------------------------------#
# Crear una tabla con valores de media, mediana e intervalo de confianza del
# 95% para cada parámetro en param_df1.
lines <- param_df1 %>%
  group_by(parameter) %>%
  summarise(mn = mean(value),
            me = quantile(value, probs = 0.5),
            li = quantile(value, probs = (0.05 / 2)),
            ls = quantile(value, probs = 1 - (0.05 / 2)) )

#-------------------------------------------------------------------------------#
# Crear un gráfico con las distribuciones de los parámetros en cada muestreo 
# de bootstrap, se colocan los valores de densidad, y estadísticos de 
# resumen (incluyendo los que se encuentran en la tabla *lines*)

G1 <- param_df1 %>%
  ggplot(mapping = aes(value), colour = 'black') +
  geom_histogram(aes(y = ..density..,  fill = parameter), bins = 12, col='black') +
  geom_density() + 
  geom_vline(data = popParameters1, aes(xintercept = value), col = 'green2') + 
  geom_vline(lines, mapping = aes(xintercept = me), col = 'red3', lty = 'dashed') +
  geom_vline(lines, mapping = aes(xintercept = li), col = 'red3', lty = 'dashed') +
  geom_vline(lines, mapping = aes(xintercept = ls), col = 'red3', lty = 'dashed') +
  facet_wrap(. ~ parameter, ncol = 4, scales = 'free') +
  theme(legend.position = 'none',
        axis.title.x = element_blank()) + ylab('Densidad')

# Almacenamiento del gráfico
ggsave(glue('{auxdir}/figures/005_resultados_bootstrap.pdf'), G1, 'pdf',
       width = 9, height = 8)

#-------------------------------------------------------------------------------#
# Crear un gráfico con las distribuciones para el set de parámetros estimados 
# por modelamiento.
#................................................................................
#  1 Utilizar los mismos niveles de param_df1 en param_df
#  2 Eliminar niveles no usados 
#................................................................................

param_df %<>%
  mutate(parameter = factor(parameter, levels = levels(param_df1$parameter)),
         parameter = fct_drop(parameter)) 

G2 <- param_df %>%
  ggplot(mapping = aes(value), colour = 'black') +
  geom_histogram(aes(y = ..density..,  fill = parameter), bins = 34, col='black') +
  geom_vline(data = popParameters %>% 
               mutate(parameter = factor(parameter, levels(param_df$parameter))), 
             aes(xintercept = value), col = 'green2') + 
  geom_density() + 
  facet_wrap(. ~ parameter, ncol = 4, scales = 'free') +
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Densidad')

# Almacenamiento del gráfico
ggsave(glue('{auxdir}/figures/006_originales_bootstrap.pdf'), G2, 'pdf',
       width = 8, height = 6)

#-------------------------------------------------------------------------------#
# Crear un gráfico de cajas y bigotes con los valores estimados para cada 
# modelo. 

G3 <- param_df %>%
  ggplot(mapping = aes(x = parameter, y = value), colour = 'black') +
  geom_jitter(aes(colour = parameter), shape = '.') +
  geom_violin(aes(fill = parameter), alpha = 0.5) +
  ylab('Valor') +
  facet_wrap(. ~ parameter, ncol = 4, scales = 'free') +
  theme(legend.position = 'none', 
        axis.title.x = element_blank())

# Almacenamiento del gráfico
ggsave(glue('{auxdir}/figures/007_bootstrap_dots.pdf'), G3, 'pdf',
       width = 6, height = 4)

#-------------------------------------------------------------------------------#
# Almacenar la tabla como un archivo de CSV de tipo Excel -----------------
#-------------------------------------------------------------------------------#
# Crear una tabla de parámetros con valores de resumen del análisis de bootstrap
#................................................................................
#  1 Tomar la tabla param_df1
#  2 Agrupar de acuerdo al tipo de par?metro.
#  3 Resumir de acuerdo a:
#    a Media del valor de cada par?metro
#    b Desviación estándar del valor de cada parámetro
#    c Coeficiente de variación del valor de cada parámetro
#    d Límite inferior IC95% del valor de cada parámetro
#    e Límite superior IC95% del valor de cada parámetro
#................................................................................

param_table <- param_df1 %>%
  group_by(parameter) %>%
  summarise(
    mn  = mean(value, na.rm = TRUE),
    sd  = sd(value, na.rm = TRUE),
    rsd = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE),
    li  = quantile(value, probs = (0.05 / 2)),
    ls  = quantile(value, probs = 1 - (0.05 / 2))
  )  

param_table_gt <- param_table %>%
  mutate(
    parameter = fct_recode(parameter,
     'V1' = 'V1_pop', 'V2' = 'V2_pop', 'Cl' = 'Cl_pop', 'Q' = 'Q_pop',
     'a_1' = 'a1', 'a_2' = 'a2')) %>%  
  gt::gt() %>%
  gt::fmt_number(columns = vars('mn', 'sd', 'rsd', 'li', 'ls'),
                 decimals = 2) %>%
  gt::cols_merge_range(col_begin = vars(li), col_end = vars(ls)) %>%
  gt::cols_label(
    parameter = 'Parámetro',
    mn = 'Media',
    sd = 'SE',
    rsd = 'RSD (%)',
    li = 'IC 95%'
  ) %>%
  gt::tab_header(gt::md('**Resultados bootstrap no parámétrico**'),
                 gt::md('**Modelo final para VAN por dos métodos - error aditivo**')) %>% 
  gt::tab_options(table.font.size = "smaller",
                  data_row.padding = gt::px(3)) %>% 
  gt::tab_footnote('Obtenido mediante bootstrap no paramétrico', 
                   gt::cells_column_labels(columns = vars(li))) %>% 
  gt::tab_footnote('Obtenido mediante matriz de info. de Fisher (FIM)', 
                   gt::cells_column_labels(columns = vars(sd, rsd)))


# Almacenamiento de tabla de datos de HTML
gt::gtsave(param_table_gt, '008_resultados_bootstrap.html', 
           glue('{auxdir}/figures/') %>% normalizePath())

#-------------------------------------------------------------------------------#
# Apertura de registro de convergencia de cada modelo ---------------------
#-------------------------------------------------------------------------------#
# Esta sección tiene como objetivo leer todos los archivos de par?metros 
# poblacionales en todos los subdirectorios. 
#................................................................................
#  1 Crear un objeto de tipo lista 
#  2 Almacenar en cada uno de sus elementos con una tabla le?da de una 
#  dirección específica en el árbol de directorios.
#  3 Convertir el archivo lista en un data.frame con una columna para 
#  identificar de que carpeta proviene cada set de par?metros.
#................................................................................
convergence_list = vector('list', 1000L)

for (i in 1:1000) {
  convergence_list[[i]] =
    read_csv(
      glue(
        '{bootdir}/B{i}/FINAL/ChartsData/Saem/CvParam.txt'
      ),
      col_types = cols()
    )
}

#-------------------------------------------------------------------------------#
# Cálculo de convergencia de modelos
#-------------------------------------------------------------------------------#
# Aplicación de la función en los datos
#' 1 Convertir *convergence_list* en un DataFrame con marcado de cada muestreo
#' 2 Seleccionar el número de muestra _B_ y el indicador de convergencia
#' 3 Agrupar por _B_
#' 4 Anidar formando un vector numérico con _convergenceIndicator_
#' 5 Calcular tamaño de vector en _n_
#' 6 Calcular indicador de convergencia (estadístico T) en _ev_, sin tendencia en 
#' 100 iteraciones consecutivas
#' 7 Desanidar _ev_ formando dos columnas
#' 8 Calcular valor p en *pval* e indicador de *pval_log*
#................................................................................

Z <- convergence_list %>%
  map_dfr(~ .x, .id = 'B') %>%
  select(B, iteration, convergenceIndicator, phase) %>%
  group_by(B) %>% nest() %>%
  mutate(n  = map_int(data, ~ dim(.x)[1]),
         ev = map(data,  ~ evalConvergencia(as_vector(
           .x$convergenceIndicator
         ), l = 100))) %>%
  unnest(ev) %>%
  mutate(
    pval = 2 * pt(estadistico_T, 100 - 2, 0, 0),
    pval_log = if_else(pval <= 0.01, T, F)
  )

convergence_df1 <- Z %>% 
  filter(pval_log == FALSE) %>% 
  slice(1) %>% 
  unnest(data) %>% 
  select(-pval, -pval_log)

#-------------------------------------------------------------------------------#
# Crear gráfico y almacenar con convergencia de indicador
GITER <- ggplot(convergence_df1, 
                aes(x = iteration, y = convergenceIndicator, group = B, col = phase)) +
  geom_line(alpha = 0.1) +
  geom_point(data = convergence_df1[convergence_df1$iteracion == convergence_df1$iteration,],
             shape = 4,
             colour = 'red') +
  labs(x = 'Iteraciones', y = 'Indicador Convergencia') +
  theme_bw() +
  theme(legend.position = 'none')

# Almacenamiento del gráfico
ggsave(glue('{auxdir}/figures/009_trayectorias_Iter.pdf'), GITER, 'pdf',
       width = 8, height = 6)

#-------------------------------------------------------------------------------#
# Realizar un estudio del número adecuado de n valores para evaluación de 
# convergencia a larga distancia.

df_conv <- Z %>%
  group_by(B) %>% nest() %>%
  mutate(
    data1         = map(data, ~ filter(.x, iteracion < 1000)),
    sumaAcumCont  = map(data, ~ verificadorContinuidad(.x$pval_log)),
    maxsumaAcum   = map_dbl(sumaAcumCont, ~ max(.x, na.rm = TRUE)),
    sumaAcumCont1 = map(data1, ~ verificadorContinuidad(.x$pval_log)),
    maxsumaAcum1  = map_dbl(sumaAcumCont1, ~ max(.x, na.rm = TRUE))
  )


df_conv_2 <- tibble(Iter = seq(1,1000))


G_conv_filt <- df_conv_2 %>% 
  mutate(
    N  = map_dbl(Iter, ~(sum(df_conv$maxsumaAcum>=.x))),
    N1 = map_dbl(Iter, ~(sum(df_conv$maxsumaAcum1>=.x)))
    ) %>%
  pivot_longer(cols = c(N,N1), names_to = 'Tipo', values_to = 'N') %>% 
  ggplot(aes(x = Iter, y = N, col = Tipo)) +
  geom_line() +
  scale_color_manual(values = c('red','blue'), 
                     labels = c('Total', 'Exploratoria')) +
  theme(legend.position = c(0.8, 0.8)) +
  ylab('Conteo de casos') + 
  xlab('Iteraciones') + 
  labs(title = 'Resultados de Bootstrap',
       subtitle = 'Fracción de iteraciones remanentes por filtro de convergencia') 

ggsave(glue('{auxdir}/figures/010_filtro_convergencia.pdf'), G_conv_filt, 'pdf', 
         width = 7, height = 5)

#-------------------------------------------------------------------------------#
# Eliminación de corridas con problemas de convergencia
#' 1 Agrupar por B 
#' 2 Anidar
#' 3 Extraer el valor de iteración mínimo para cada muestra _B_
#' 4 Filtrar muestras B con valor menor a 1000 (es decir fase exploratoria)

clean_df <- convergence_df1 %>% 
  group_by(B) %>% nest() %>% 
  mutate(iter = map_dbl(data, ~mean(.x$iteracion, na.rm = TRUE))) %>% 
  filter(iter <= 1000) %>% select(-iter)

#-------------------------------------------------------------------------------#
# Gráficos con corrección de convergencia ---------------------------------
#-------------------------------------------------------------------------------#
# Manipulación de tabla con resultados de bootstrap no paramétrico
#................................................................................
#  1 Tomar el archivo de datos
#  2 Seleccionar sólo las corridas que hallan pasado el criterio de convergencia
#  3 Transformar el layout de los datos a una forma expandida con columnas 
#  para cada parámetro.
#  4 Calcular k10, k12, k21, alpha, beta, t_alpha, t_beta, A, y B. Con 
#  funciones definidas en situ, y funciones complejas micro.fun, y macro.fun
#  5 Transformar el layout de los datos a una forma compacta similar a la 
#  original.
#  6 Ordenar la tabla por ID.
#  7 Transformar la variable parameter en forma de un factor con ordenación 
#  específica.
#................................................................................

param_df2 <- param_df %>%
  filter(ID %in% as.character(clean_df$B)) %>% 
  pivot_wider(names_from = parameter, values_from=value) %>% 
  mutate(
    List = pmap(list(1000, Cl_pop, Q_pop, V1_pop, V2_pop), constants_fun)
  ) %>%
  unnest(cols = c(List)) %>%
  pivot_longer(-contains('ID'), names_to = 'parameter', values_to = 'value') %>% 
  arrange(ID) %>% 
  mutate(parameter = factor(parameter, level = vectorParametros))

#-------------------------------------------------------------------------------#
# Creación de gráficos-----------------------------------------------------
#-------------------------------------------------------------------------------#
# Crear una tabla con valores de media, mediana e intervalo de confianza del
# 95% para cada parámetro en param_df2.

lines_2 <- param_df2 %>%
  group_by(parameter) %>%
  summarise(mn = mean(value),
            me = quantile(value, probs = 0.5),
            li = quantile(value, probs = (0.05 / 2)),
            ls = quantile(value, probs = 1 - (0.05 / 2)) )

#-------------------------------------------------------------------------------#
# Crear un gráfico con las distribuciones de los par?metros en cada muestreo 
# de bootstrap, se colocan los valores de densidad, y estadísticos de 
# resumen (incluyendo los que se encuentran en la tabla *lines*)

G1b <- param_df2 %>%
  ggplot(mapping = aes(value), colour = 'black') +
  geom_histogram(aes(y = ..density..,  fill = parameter), bins = 34) +
  geom_density() + 
  geom_vline(lines_2, mapping = aes(xintercept = mn), colour = 'green2') +
  geom_vline(lines_2, mapping = aes(xintercept = me), colour = 'red3', lty = 'dashed') +
  geom_vline(lines_2, mapping = aes(xintercept = li), colour = 'red3', lty = 'dashed') +
  geom_vline(lines_2, mapping = aes(xintercept = ls), colour = 'red3', lty = 'dashed') +
  facet_wrap(. ~ parameter, ncol = 4,
             scales = 'free') +
  theme(legend.position = 'none', 
        axis.title.x = element_blank()) +
  ylab('Densidad')

# Almacenamiento del gráfico
ggsave(glue('{auxdir}/figures/011_resultados_bootstrap_limpio.pdf'), G1b, 'pdf',
       width = 8, height = 6)

#-------------------------------------------------------------------------------#
# Crear un gráfico de cajas y bigotes con los valores estimados para cada 
# modelo. 

G3b <- param_df2 %>%
  filter(!(parameter %in% c('A', 'B'))) %>% 
  ggplot(mapping = aes(x = parameter, y = value), colour = 'black') +
  geom_jitter(aes(colour = parameter), shape = '.') +
  geom_violin(aes(fill = parameter), alpha = 0.5) +
  xlab('') + ylab('Valor') +
  facet_wrap(. ~ parameter, ncol = 4, scales = 'free') +
  theme(legend.position = 'none', 
        axis.text.x = element_blank())

# Almacenamiento del gráfico
ggsave(glue('{auxdir}/figures/012_bootstrap_dots_limpia.pdf'), G3b, 'pdf',
       width = 6, height = 4)

#-------------------------------------------------------------------------------#
# Crear una tabla de parámetros con valores de resumen del análisis de bootstrap
#................................................................................
# 1 Seleccionar las columnas "parameter" y "value" de *param_df2*, este contiene 
# todos los replicados del Bootstrap.
# 2 Agrupar y anidar por "parameter", esto deja a los replicados como un vector 
# dentro de cada parámetro.
# 3 Calcular media, y mediana de los replicados mediante un mapeo.
# 4 Aplicar la función "confints" en C++ que calcula varios tipos de intervalos 
# de confianza, se aplica nivel de significancia de 0.05.
# 5 Calcular mediana de replicados.
# 6 Desanidar a B (que es el data.frame con los IC95%), al hacer esto se vuelven 
# columnas.
# 7 Unir a la tabla con parámetros estimados originales, se y rse (estimados con 
# Matriz de Información de Fisher).
# 8 Mover las columnas de valor estimado, se, y rse al frente.
#................................................................................

param_df3 <- param_df2 %>% 
  select(parameter, value) %>% 
  nest_by(parameter) %>%
  left_join(popParameters1, by = 'parameter') %>% 
  mutate(B = pmap(list(param=data, t0=value, alpha=0.05), confints)) %>% 
  mutate(median = map_dbl(data, ~ median(.x)),
         n      = map_dbl(data, ~length(.x))) %>% 
  unnest(B) %>% 
  left_join(popParameters, by = 'parameter') %>%
  relocate(c(value.x, se_sa, rse_sa), .before=data)

#-------------------------------------------------------------------------------#
# Reportar Resultados de Bootstrap en formato HTML ------------------------------
#------------------------------------------------------------------------------#

param_df3_gt <- param_df3 %>%
  select(-data,-value.y, -n) %>% 
  ungroup() %>% 
    mutate(parameter = fct_recode(parameter,'V1'='V1_pop','V2'='V2_pop',
                                  'Cl'='Cl_pop', 'Q'='Q_pop')) %>%
  rename(value = value.x) %>% 
  gt::gt() %>%
  gt::fmt_number(columns = vars(value,se_sa,rse_sa,
                  median,classic.LI,classic.LS,percent.LI,percent.LS,
                  normal.LI,normal.LS,pivote.LI,pivote.LS,BCa.LI,
                  BCa.LS),
                 decimals = 2) %>% 
  gt::cols_merge_range(vars(classic.LI), vars(classic.LS), sep = I(", ")) %>% 
  gt::cols_merge_range(vars(percent.LI), vars(percent.LS), sep = I(", ")) %>% 
  gt::cols_merge_range(vars(normal.LI), vars(normal.LS), sep = I(", ")) %>% 
  gt::cols_merge_range(vars(pivote.LI), vars(pivote.LS), sep = I(", ")) %>% 
  gt::cols_merge_range(vars(BCa.LI), vars(BCa.LS), sep = I(", ")) %>% 
  gt::tab_spanner(label = glue("Resultados Bootstrap (n = {unique(param_df3$n)})"), 
                  vars(median, classic.LI,classic.LS,percent.LI,percent.LS,
                       normal.LI,normal.LS,pivote.LI,pivote.LS,BCa.LI,
                       BCa.LS)) %>% 
  gt::cols_label(
    parameter = 'Parámetro',
    value = 'Estimado',
    se_sa = 'SE',
    rse_sa = 'RSD (%)',
    median = 'Mediana',
    classic.LI = 'Clásico',
    percent.LI = 'Percentil',
    normal.LI = 'Normal',
    pivote.LI = 'Pivote',
    BCa.LI = 'BCa'
  ) %>%
  gt::fmt_missing(1:15, 1:18, missing_text = '-') %>% 
  gt::tab_header(gt::md('**Resultados bootstrap no parámétrico**'),
                 gt::md('**Modelo final VAN por dos métodos - error aditivo**')) %>% 
  gt::tab_options(table.font.size = "smaller",
                  data_row.padding = gt::px(3)) %>% 
  gt::tab_footnote('Obtenido mediante bootstrap no paramétrico', 
                   gt::cells_column_spanners(
                     spanners = glue("Resultados Bootstrap (n = {unique(param_df3$n)})"))) %>% 
  gt::tab_footnote('Obtenido mediante matriz de info. de Fisher (FIM)', 
                   gt::cells_column_labels(columns = vars(se_sa, rse_sa)))


# Almacenamiento de tabla de datos de HTML
gt::gtsave(param_df3_gt, '013_IC_bootstrap.html', 
           glue('{getwd()}/{auxdir}/figures/'))
