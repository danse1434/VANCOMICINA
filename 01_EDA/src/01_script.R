##------------------------------------------------------------------------------#
## Nombre del Script: EDA de Vancomicina ----------
##  
## Propósito del Script:  Análisis Exploratorio de Datos (EDA) de vancomicina.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  12-07-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(lubridate)
require(rlang)
require(tidyverse)
require(PerformanceAnalytics)
require(patchwork)
require(gt)

source('src/20_funciones.R')

theme_set(theme_bw())

#-------------------------------------------------------------------------------#
# Introducción -----------------------
#-------------------------------------------------------------------------------#
data <- read_csv('data/data_TAD.csv', na = '.')

df1 <- data %>%
  filter(EVID == 1) %>% 
  mutate(SCM2 = 0.007184 * (WTKG ^ 0.425) * (HCM ^ 0.725)) %>%
  mutate(CLCRMLMIN = eGFR(SCRMGDL, AGEA, SEXF, 0))

# > Pruebas de normalidad ---------------------------------------------------
df1_norm <- df1 %>% # Se seleccionan 15 datos correspondientes a los pacientes
  normtest_batery(data = .,
                  vector = c(12, 14:21, 23), 
                  alpha = 0.05) 

# Presentación en formato de tabla
norm_gt1 <- gt(df1_norm) %>% 
  fmt_number(columns = 2:7, decimals = 3) %>% 
  tab_header(title = html('<b>&#x2605;Batería de Pruebas de Normalidad&#x2605;</b>'),
             subtitle = md('*Set de datos de Vancomicina*')) %>% 
  tab_spanner(label = md("**Prueba de normalidad**"), columns = 2:7) %>% 
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  ) %>% 
  fun_param(Shapiro) %>% 
  fun_param(Anderson_Darling) %>% 
  fun_param(Cramer_von_Mises) %>% 
  fun_param(Liliefors) %>% 
  fun_param(Pearson) %>% 
  fun_param(Shapiro_Francia) %>% 
  tab_footnote(footnote = glue::glue('Los colores en verde indican un valor p > 0.05, 
                              no se puede rechazar la H0 de normalidad.'),
               locations = cells_column_labels(columns = 1)) 

norm_gt1 %>% 
  gtsave(filename = 'gt_normalidad.html', path = file.path(getwd(), 'output/tabs/'))

#-------------------------------------------------------------------------------#
# Estadística descriptiva -----------------------------
#-------------------------------------------------------------------------------#
#' Se realiza un análisis desriptivo de las covariable de los pacientes y se 
#' obtiene de las variables continuas media, desviación estándar, límite inferior 
#' (IC95%), límite superior (IC95%), mínimo, máximo, Q1, mediana, Q3, y rango 
#' intercuartílico (IQR).
#................................................................................
#' 1 Seleccionar variables continuas
#' 2 Resumir en todas las variables seleccionadas, y obtener media, sd, n, resumen 
#' de cinco números, rango intercuartílico.
#' 3 Colapsar la tabla con valores _var_ (variable) y _est_ (estadístico); valores 
#' a _val_.
#' 4 Expandir tabla con nombres de _est_ y _val_
#' 5 Calcular el límite inferior _li_ y límite superior _ls_
#' 6 Mover las columnas _li_, _ls_ con el resto de los resultados paramétricos
#................................................................................

res_df1 <- select(df1, all_of(c(12, 14:21, 23))) %>%
  summarise(across(
    everything(),
    list(
      mn  = ~ mean(.x, na.rm = TRUE),
      sd  = ~ sd(.x, na.rm = TRUE),
      n   = ~ n(),
      fv1 = ~ fivenum(.x)[1],
      fv2 = ~ fivenum(.x)[2],
      fv3 = ~ fivenum(.x)[3],
      fv4 = ~ fivenum(.x)[4],
      fv5 = ~ fivenum(.x)[5],
      iqr = ~ IQR(.x)
    )
  )) %>% 
  pivot_longer(cols = everything(), names_to = c('var', 'est'), 
               names_sep = '_', values_to = 'val') %>% 
  pivot_wider(names_from = 'est', values_from  = 'val') %>% 
  mutate(li = mn - sd*qt(1-0.05/2, df=n-1)/sqrt(n),
         ls = mn + sd*qt(1-0.05/2, df=n-1)/sqrt(n)) %>% 
  relocate(c(li, ls), .before = fv1)

# Presentación en formato de tabla
desc_gt1 <- gt(res_df1) %>% 
  fmt_number(columns  = 2:12, decimals = 3) %>% 
  fmt_number(columns  = vars(n), decimals = 0) %>% 
  fmt_number(columns  = 2:12, 
             rows     = 8:9, decimals = 0) %>% 
  tab_header(title    = html('<b>&#x2605; Estadísticos Descriptivos &#x2605;</b>'),
             subtitle = md("**Variables continuas**")) %>% 
  cols_label(var      = 'Variable',
             mn       = 'Media',
             sd       = 'DesvEst',
             n        = 'N',
             li       = 'LI (IC95%)',
             ls       = 'LS (IC95%)',
             fv1      = 'Min',
             fv2      = 'Q1',
             fv3      = 'Mediana',
             fv4      = 'Q3',
             fv5      = 'Max',
             iqr      = 'IQR') %>% 
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  )

desc_gt1 %>% 
  gtsave(filename = 'gt_desc_continua.html', path = file.path(getwd(), 'output/tabs/'))

# > Correlaciones entre covariables continuas -----------
# Se realizó un análisis de correlación entre variables de los pacientes por un 
# método gráfico.

{pdf('output/figs/Correl_Contin_1.pdf', 11.0, 8.50)
  select(df1, all_of(c(12, 14:21, 23))) %>% 
  chart.Correlation(., histogram = TRUE, pch = 19)
dev.off()}

# Se discute en el texto las implicaciones del análisis de correlación.


# > Detección de outliers ------------

outl_df1 <- select(df1, all_of(c(12, 14:21, 23))) %>% 
    mutate(across(everything(), ~ out_det(vec = .x, val = .x)))

outl_gt1 <- outl_df1 %>% 
  rownames_to_column(var = 'ID') %>% 
  gt() %>% 
  tab_header(title    = html('<b>&#x2605; Resumen de valores por fuera IQR &#x2605;</b>'),
             subtitle = glue::glue("En rojo se muestran los casos en donde el valor se 
                                   sospecha como anómalo.")) %>% 
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  ) %>% 
  fun_param_1(AGEA) %>% 
  fun_param_1(WTKG) %>% 
  fun_param_1(HCM) %>% 
  fun_param_1(SCRMGDL) %>% 
  fun_param_1(ALBGDL) %>% 
  fun_param_1(PROGDL) %>% 
  fun_param_1(CLCRMLMIN) %>% 
  fun_param_1(RAL) %>% 
  fun_param_1(RAN) %>% 
  fun_param_1(SCM2)
  
outl_gt1 %>% 
  gtsave(filename = 'gt_outliers.html', path = file.path(getwd(), 'output/tabs/'))


# > Coeficientes de correlación de Pearson -----------------
#................................................................................
#' 1 Seleccionar variables continuas
#' 2 Obtener los nombres de columna como vector
#' 3 Obtener las posibles combinaciones de dos valores sin repetición como lista
#' 4 Convertir en data frame con ID como nombre
#' 5 Crear una variable dummy que consiste de la secuencia Var1, Var2 repetida
#' 6 Expandir a la variable dummy para crear especificación de combinación de 
#' variables
#................................................................................

df1_b <- df1 %>% 
  select(12, 14:21, 23) %>% 
  colnames() %>% 
  combn(m = 2, simplify = FALSE) %>% 
  map_dfr( ~ as_tibble(.x), .id = 'ID') %>% 
  mutate(rep = rep(c('Var1', 'Var2'), 45)) %>% 
  pivot_wider(names_from = rep)

#................................................................................
#' 7 Crear a la columna _dat1_ que contiene un tibble con el par de covariables 
#' especificada en cada fila.
#' 8 Utilizar la función de correlación para obtener la correlación de Pearson
#' 9 Utilizar bootstrap para obtener IC de tipo percentil del 95%
#................................................................................

df1_C <- df1_b %>% 
  mutate(dat1 = map2(Var1,Var2, ~tibble(select(df1, .x, .y))),
         corr = map_dbl(dat1, ~ corr(.x, 1:14, 1, 2))) %>% 
  mutate(boot = map(dat1, ~ Confint_Boot(.x, 1, 2))) 

#................................................................................
#' 10 Extraer el límite inferior y superior del intervalo de confianza estimado
#' 11 Eliminar a las columnas _dat1_, y _boot_
#................................................................................

df1_C <- df1_C %>% 
  mutate(li = map_dbl(boot, ~ .x$percent[,4]),
         ls = map_dbl(boot, ~ .x$percent[,5])) %>% 
  select(-dat1, -boot) %>% 
  arrange(desc(abs(corr)))

# Crear una tabla con correlaciones
corr_gt1 <- df1_C %>% 
  mutate(across(c(corr, li, ls), ~ round(.x, 3))) %>% 
  gt() %>% 
  cols_merge(columns = vars(li, ls), pattern = "{1}, {2}") %>% 
  cols_merge(columns = vars(corr, li), pattern = "{1} [{2}]") %>% 
  tab_header(
    title = html('<b>&#x2605; Correlación de Pearson &#x2605;</b>'),
    subtitle = glue::glue("Se muestra un resumen de coeficientes en pares de covariables")) %>%
  cols_label(Var1 = "Variable 1", 
             Var2 = "Variable 2", 
             corr = "Correlación [IC95%]") %>% 
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  ) %>%
  tab_style(style = list(cell_fill(color = alpha('#00ff00', 0.5))),
            locations = cells_body(columns = vars(corr),
                                   rows = (sign(li) == sign(ls))))

corr_gt1 %>% 
  gtsave(filename = 'gt_corr_pearson.html', path = file.path(getwd(), 'output/tabs/'))

# > Covariables discretas ----------------
# Se crea una función que realiza el Test de Correlación Biserial por Puntos
# Las variables seleccionadas en un tibble

df2 <- tibble::tribble(
    ~Var1,     ~Var2,
    "SEXF",    "AGEA",
    "SEXF",    "WTKG",
    "SEXF",     "HCM",
    "SEXF", "SCRMGDL",
    "SEXF",  "ALBGDL",
    "SEXF",    "SCM2",
    "ANTU",     "AGEA",
    "ANTU",    "WTKG",
    "ANTU",     "HCM",
    "ANTU", "SCRMGDL",
    "ANTU",    "ALBGDL",
    "ANTU",    "SCM2")

#................................................................................
#' 1 Extraer el tibble con las variables de interés en _Var1_ y _Var2_
#' 2 Calcular la correlación biserial por puntos
#' 3 Calcular IC95% por bootstrap
#' 4 Calcular resultados de test con correl. biserial
#' 5 Extraer _li_, _ls_, _t_, _tc_, y _pval_ de las columnas listas
#................................................................................

df2b <- df2 %>%
  mutate(
    dat1  = map2(Var1, Var2, ~ tibble(select(df1, .x, .y))),
    biser = map_dbl(dat1, ~ corr_biserial(.x, 1:14)),
    boot  = map(dat1, ~ Confint_Boot_biserial(.x)),
    test  = map(biser, ~ test_biserial(.x, n = 14))
  ) 

df2b1 <- df2b %>%
  mutate(
    li   = map_dbl(boot, ~ .x$percent[, 4]),
    ls   = map_dbl(boot, ~ .x$percent[, 5]),
    t    = map_dbl(test, pluck("t")),
    tc   = map_dbl(test, pluck("tc")),
    pval = map_dbl(test, pluck("pval"))
  )

# Presentación de tabla
gt_correl_biser <- 
  select(df2b1, -dat1, -boot, -test) %>% 
  arrange(pval) %>% 
  gt() %>% 
  tab_header(
  title = html('<b>&#x2605; Test de Correlación Biserial por Puntos &#x2605;</b>'),
  subtitle = glue::glue("Se muestra un resumen de valores obtenidos")) %>%
  cols_label(Var1  = "Variable 1",
             Var2  = "Variable 2",
             biser = "Corr Biserial [IC95%]",
             t     = "Valor t",
             tc    = "Valor t Crítico",
             pval  = 'Valor p') %>%
  fmt_number(columns = 3:8, decimals = 3) %>% 
  cols_merge(columns = vars(li, ls), pattern = "{1}, {2}") %>% 
  cols_merge(columns = vars(biser, li), pattern = "{1} [{2}]") %>% 
  tab_style(style = list(cell_fill(color = alpha('#00ff00', 0.5))),
            locations = cells_body(columns = vars(biser, pval),
                           rows = (sign(li) == sign(ls)))) %>% 
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  )
# Almacenamiento de tabla
gt_correl_biser %>% 
  gtsave(filename = 'gt_corr_biserial.html', path = file.path(getwd(), 'output/tabs/'))


# > Gráficos de relaciones de covariables -----------
var_cont <- colnames(select(df1, 12, 14:21, 23)) 

df2_ls <- vector('list', 20L)

for (i in 1:10) {
  df2_ls[[i]]      <- plot2(df1, SEXF, var_cont[[i]])
  df2_ls[[i + 10]] <- plot2(df1, ANTU, var_cont[[i]], h=c('green', 'purple'))
}

df2_comp <- 
df2_ls[[1]] + df2_ls[[2]] + df2_ls[[3]] + df2_ls[[4]] + df2_ls[[5]] +
df2_ls[[6]] + df2_ls[[7]] + df2_ls[[8]] + df2_ls[[9]] + df2_ls[[10]] +
df2_ls[[11]] + df2_ls[[12]] + df2_ls[[13]] + df2_ls[[14]] + df2_ls[[15]] +
df2_ls[[16]] + df2_ls[[17]] + df2_ls[[18]] + df2_ls[[19]] + df2_ls[[20]] +
  guide_area() + plot_layout(guides = 'collect') + 
plot_spacer()+ plot_spacer() + 
plot_layout(ncol = 5)

ggsave('Correl_Discret_1.pdf', df2_comp, 'pdf', 'output/figs/', 1, 12, 10)

var_cont

# HCM, SCRMGDL, RAL, SCM2
df2_comp_a <- df2_ls[[3]] + df2_ls[[4]] + df2_ls[[8]] + 
  df2_ls[[10]] + plot_layout(guides = 'collect')
# WTKG, ALBGDL, PROGDL
df2_comp_b <- df2_ls[[10+2]] + df2_ls[[10+5]] + df2_ls[[10+6]] +
  guide_area() + plot_layout(guides = 'collect')

df2_comp_c <- df2_ls[[3]] + df2_ls[[4]] + df2_ls[[8]] + df2_ls[[10]] +
  df2_ls[[12]] + df2_ls[[15]] + df2_ls[[16]] +
  guide_area() + plot_layout(guides = 'collect')


ggsave('Correl_Discret_a.pdf', df2_comp_a, 'pdf', 'output/figs/', 1, 8, 6)
ggsave('Correl_Discret_b.pdf', df2_comp_b, 'pdf', 'output/figs/', 1, 8, 6)
ggsave('Correl_Discret_c.pdf', df2_comp_c, 'pdf', 'output/figs/', 1, 8, 6)
  

#-------------------------------------------------------------------------------#
# Revisión de diferencias en grupos por variable dicotómica -------------
#-------------------------------------------------------------------------------#
comb_df <-
  bind_rows(
    comp_func_1(data = df1, varg = SEXF) %>% rename('Var' = 'SEXF') %>% add_column(ID = 'SEXF'),
    comp_func_1(data = df1, varg = ANTU) %>% rename('Var' = 'ANTU') %>% add_column(ID = 'ANTU')
  ) %>% 
  relocate(ID, Var, Variable, mn:q3)
  
comb_gt2 <- comb_df %>% 
  group_by(ID) %>% 
  gt() %>% 
  tab_header(
    title = html('<b>&#x2605; Separación de covariables &#x2605;</b>'),
    subtitle = glue::glue("Se muestra un resumen de estadísticos en separación 
                          de covariables")) %>%
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  ) %>% 
  fmt_number(columns = vars(mn, q1, q3), decimals = 2) %>% 
  cols_merge(columns = vars(q1, q3), pattern = "{1}, {2}") %>% 
  cols_merge(columns = vars(mn, q1), pattern = "{1} ({2})")

comb_gt2 %>% 
  gtsave(filename = 'gt_covar_grupo.html', path = file.path(getwd(), 'output/tabs/'))


#-------------------------------------------------------------------------------#
# Revisión Datos PK -----------------------------------------
#-------------------------------------------------------------------------------#
theme_set(theme_bw() + 
            theme(legend.position = c(0.85, 0.85), 
                  legend.background = element_rect(fill = NULL, colour = 'gray50'), 
                  legend.key = element_rect(fill = NULL), 
                  legend.title = element_text(face = 'bold'), 
                  legend.spacing.y = unit(0.01, 'cm'),
                  legend.key.height = unit(0.30, 'cm')))
            

# Gráfico de concentración plasmática en escala normal 
#................................................................................
#'  1. Data
#'  2. Convertir la variable *ID* en un factor discreto
#'  3. Abrir el ambien GGplot
#'  4. Adicionar puntos, y adicionar l?neas
#'  5. Configurar etiqueta eje X
#'  6. Configurar etiqueta eje Y
#'  7. Configurar etiquetas para mostrar 5 filas
#'  8. Crear separaciónen facets
#'  9. Configurar colores en escala discreta como colores Viridis
#................................................................................

ytype_status <- c(
  `1` = 'Microbiológico',
  `2` = 'Quimioluminiscencia'
)

ytype_LLOQ <- tribble( ~ YTYPE, ~ LLOQ,
                       1, NA_real_,
                       2, 3)

gperfil1 <- data %>% 
  mutate(ID = factor(ID)) %>% 
  filter(EVID == 0) %>% 
  ggplot(., aes(x = TAD, y = DV, group = ID, col = ID)) + 
  geom_point() + geom_line() + 
  xlab('TAD: tiempo tras dosis (hr)') +
  ylab('Concentración plasmática (mg/L)') + 
  guides(col = guide_legend(nrow=5)) + 
  coord_cartesian(xlim=c(0,12), ylim = c(1,100)) +
  geom_hline(data = ytype_LLOQ, aes(yintercept = LLOQ), col='red', lty='dashed') +
  facet_wrap(. ~ YTYPE, labeller = labeller(
    YTYPE = ytype_status
  )) +
  scale_color_viridis_d()

#-------------------------------------------------------------------------#
# Resultados de 
ggsave(filename = 'output/figs/TAD_DV.pdf', plot = gperfil1, 
       device = 'pdf', width = 7, height = 4, units = 'in')

#-------------------------------------------------------------------------------#
# Gráfico de concentración plasmática en escala logarítmica
#................................................................................
#'  1. Data
#'  2. Convertir la variable *ID* en un factor discreto
#'  3. Abrir el ambien GGplot
#'  4. Adicionar puntos, y adicionar l?neas
#'  5. Configurar etiqueta eje X
#'  6. Configurar etiqueta eje Y
#'  7. Configurar etiquetas para mostrar 5 filas
#'  8. Crear facet con método
#'  9. Convertir la escala de Y en logar?tmo
#'  10. Configurar colores en escala discreta como colores Viridis
#'  11. Adicionar puntos de quiebre en la escala   
#................................................................................
breaks <- 10^(-1:4)
minor_breaks <- rep(1:9, 4) * (10 ^ rep(-1:2, each = 9))

gperfil2 <- data %>% 
  mutate(ID = factor(ID)) %>% 
  filter(EVID == 0) %>% 
  ggplot(., aes(x = TAD, y = DV, group = ID, col = ID)) + 
  geom_point() + geom_line() + 
  xlab('TAD: tiempo tras dosis (hr)') +
  ylab(expression(log(C[p]))) + 
  guides(col = guide_legend(nrow=5)) + 
  facet_wrap(. ~ YTYPE, labeller = labeller(YTYPE = ytype_status)) +
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
  geom_hline(data = ytype_LLOQ, aes(yintercept = LLOQ), col='red', lty='dashed') +
  scale_color_viridis_d() +
  coord_cartesian(xlim=c(0,12), ylim = c(1,100)) +
  annotation_logticks(sides = 'lr')

#-------------------------------------------------------------------------------#
# Resultados de 
ggsave(filename = 'output/figs/log_TAD_DV.pdf', plot = gperfil2, 
       device = 'pdf', width = 7, height = 4, units = 'in')
