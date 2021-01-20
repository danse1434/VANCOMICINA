##------------------------------------------------------------------------------#
## Nombre del Script: Extracción Parámetros de Modelo Bayesiano ----------------
##  
## Propósito del Script: Extraer parámetros del modelo estimación bayesiana.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 10-01-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
require(MASS)
require(gt)
require(tidyverse)

#-------------------------------------------------------------------------------#
# 1. Lectura de archivo con datos bayesiano -------------------
#-------------------------------------------------------------------------------#
modelName <- '081_modeltwoCptmDiagProp_errResNor_NoInfo'
load(file = file.path('models', paste0(modelName, "Fit.Rsave")))

#-------------------------------------------------------------------------------#
# 2. Extracción parámetros poblacionales   -----------------------
#-------------------------------------------------------------------------------#
# Detección de nombres de parámetros
parameters <- as.matrix(fit) %>% 
  colnames() %>% 
  keep(~ str_detect(.x, 'CLHat|QHat|V1Hat|V2Hat|omega|rho|^b$')) %>% 
  discard(~ str_detect(.x, 'rho\\[1,1\\]|rho\\[2,2\\]|rho\\[3,3\\]|rho\\[4,4\\]'))%>% 
  discard(~ str_detect(.x, 'rho\\[2,1\\]|rho\\[3,1\\]|rho\\[4,1\\]|rho\\[3,2\\]|rho\\[4,2\\]|rho\\[4,3\\]'))

# Definición de estadísticos a calcular
fun_list <- list(
  mn = mean, 
  sd = sd, 
  med = median, 
  q2.5 = ~ quantile(.x, 0.025),
  q97.5 = ~ quantile(.x, 1-0.025))

# Generación de tabla
model_Specs <- as.data.frame(fit, pars = parameters) %>%
  summarise(across(everything(), fun_list)) %>%
  pivot_longer(cols = everything(), names_to = c('parameter', 'statistic'),
               names_pattern = "(.*)_(.*)", values_to = 'vals') %>% 
  pivot_wider(id_cols='parameter', names_from = 'statistic', values_from='vals')

# Almacenamiento de modelo 
write_csv(model_Specs, file.path('data', 'processed', 'populationParameters.csv'))

# Creación tabla de Resumen en HTML
gt_model_Specs <- model_Specs %>% 
  gt(rowname_col = 'parameter') %>% 
  gt::fmt_number(columns = 2:6, decimals = 2) %>%
  gt::tab_spanner(label = 'Percentiles', columns = vars(q2.5, q97.5)) %>%
  gt::tab_header(
    title = md('**Resumen de parámetros estimados**'),
    subtitle = md('**Modelo Base con estimación bayesiana**')
  ) %>% 
  gt::cols_label(
    mn = 'Media',
    sd = 'Desv. Est.',
    med = 'Mediana',
    q2.5 = '2.5%',
    q97.5 = '97.5%') %>% 
  gt::tab_options(
    table.font.size = '16px',
    heading.title.font.weight = 'bold', 
    column_labels.font.weight = 'bold'
    )

# Almacenamiento de gráfico
gtsave(gt_model_Specs, '001_Parametros_Modelo_Bayesiano.html', 
       file.path('reports') %>% normalizePath())

#-------------------------------------------------------------------------------#
# 3. Extracción de parámetros individuales ----------
#-------------------------------------------------------------------------------#
# Extracción de parámetros calculados para cada individuo
indivParams <- as.matrix(fit) %>% 
  colnames() %>% 
  keep(~ str_detect(.x, '(CL|Q|V1|V2)(Pred|)(\\[\\d+\\])'))

# Conversión a tabla
df_indivParams <- as.data.frame(fit, pars = indivParams) %>%
  summarise(across(everything(), fun_list)) %>%
  pivot_longer(cols = everything(), names_to = c('parameter', 'statistic'),
               names_pattern = "(.*)_(.*)", values_to = 'vals') %>%
  mutate(parameter = str_replace(parameter, '\\]', '')) %>% 
  separate(col = parameter, into = c('parameter', 'ID'), sep = '\\[') %>%
  pivot_wider(id_cols=c('ID'), names_from=c('statistic', 'parameter'), values_from='vals')

# Almacenamiento de modelo 
write_csv(df_indivParams, file.path('data', 'processed', 
                                    'estimatedIndividualParameters.csv'))

# Creación tabla de Resumen en HTML
# gt_model_Specs <-

listNames <- sapply(colnames(df_indivParams),
                    str_replace,
                    pattern = '^(mn|sd|med|q2.5|q97.5)\\_',
                    replacement = '')
listNames <- setNames(listNames, colnames(df_indivParams))

gt_indivParams <- df_indivParams %>% 
  gt(rowname_col = 'ID') %>% 
  fmt(columns = matches('^(mn|sd|med|q2.5|q97.5)'), 
      fns = function(x) ifelse(x > 100 | x < 1e-2, 
                               formatC(x, digits = 2), round(x, digits = 2))) %>%
  # tab_spanner(columns = matches('Pred$'),  label='Pred') %>%
  tab_spanner(columns = matches('^med'),   label='Mediana') %>% 
  tab_spanner(columns = matches('^mn'),    label='Media') %>% 
  tab_spanner(columns = matches('^sd'),    label='DesvEst') %>% 
  tab_spanner(columns = matches('^q2.5'),  label='q2.5') %>%
  tab_spanner(columns = matches('^q97.5'), label='q97.5') %>% 
  tab_header(
    title = md('**Resumen de parámetros individuales estimados**'),
    subtitle = md('**Modelo Base con estimación bayesiana**')
  ) %>% 
  cols_label(.list = listNames) %>% 
  gt::tab_options(
    table.font.size = '16px',
    heading.title.font.weight = 'bold', 
    column_labels.font.weight = 'bold'
  )

# Almacenamiento de gráfico
gtsave(gt_indivParams, '002_Parametros_Individuales.html', 
       file.path('reports') %>% normalizePath())


#-------------------------------------------------------------------------------#
# 4. Tabla de predicciones -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Extracción de parámetros calculados para cada individuo
predictNames <- as.matrix(fit) %>% 
  colnames() %>% 
  keep(~ str_detect(.x, '(cHat|cHatPred|cObsPred|cObsCond)\\[\\d+\\]'))

# Conversión a tabla
df_predictions <- as.data.frame(fit, pars = predictNames) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = everything(), 
               names_to = c('type', 'number'), names_pattern = "(.*)\\[(.*)\\]", 
               values_to = 'vals') %>%
  pivot_wider(id_cols=c('number'), names_from=c('type'), values_from='vals') %>% 
  mutate(number = as.integer(number))

# Almacenamiento de modelo 
write_csv(df_predictions, file.path('data', 'processed', 
                                    'predictions_1.csv'))

df_predictions_2 <- 
  read_csv(file.path('data', 'data_TAD.csv'), na = '.') %>% 
  filter(EVID == 0 & YTYPE==1) %>% 
  dplyr::select(ID, DV, TAD, YTYPE) %>% 
  bind_cols(df_predictions %>% dplyr::select(-number))

# Almacenamiento de modelo 
write_csv(df_predictions_2,
          file.path('data', 'processed', 'predictions_2.csv'))

# df_predictions_2 %>% 
#   ggplot(aes(x=TAD, y=cObsCond, col=factor(ID)))+
#   geom_line() +
#   geom_line(aes(y=cObsPred), lty = 'dashed', color='black') +
#   geom_point(aes(y=DV)) + 
#   theme_bw() +
#   facet_wrap(. ~ ID, ncol = 4)


#-------------------------------------------------------------------------------#
# 5. Distribución e incertidumbre Parámetros --------
#-------------------------------------------------------------------------------#
theme_set(theme_bw())

distDF <- as.matrix(fit, pars = parameters) %>% 
  as_tibble(.) %>% 
  add_column(rep = 1:8000, .before = 'CLHat') %>% 
  pivot_longer(!matches('rep'), 
               names_to = 'parameter', values_to = 'values') %>%
  mutate(parameter = factor(parameter, 
                            levels = c('CLHat', 'QHat', 'V1Hat', 'V2Hat', 
                                       'omega[1]', 'omega[2]', 'omega[3]', 'omega[4]', 'b'))) 
  
distDF %>% 
  ggplot(aes(x=values, colour = parameter, 
             fill=after_scale(alpha(colour, 0.5)))) + 
  # geom_histogram(stat = 'density') +
  geom_density() + 
  facet_wrap(. ~ parameter, ncol=4, scales = 'free') + 
  theme(legend.position = 'none', axis.title.x = element_blank()) + 
  ylab('Densidad')


P#-------------------------------------------------------------------------------#
#' Encontrar parámetros de dist. probabilidad 
#'
#' @param x vector numérico con muestras
#' @param dist distribución de probabilidad elegida
#'
#' @return
#' @export
#' Objeto de tipo fitdistr con resultados de evaluación ML
#'
#' @examples
#' map2(data, dist, ~findParameters(.x$values, dist = .y))
#' 
findParameters <- function(x, dist='log-normal') {
  if (dist == 'log-normal') {
    m <- fitdistr(x, densfun = dist)
  } else if (dist == 'gamma' | dist == 'dgamma') {
    loc1 = mean(x)
    loc2 = var(x)
    m <- fitdistr(x, densfun = dist, start = list('shape'=loc1, 'rate'=loc2))    
  } else {
    print('Distribution not recognized')
    stop()
  }
  
  return(m)
}

#-------------------------------------------------------------------------------#
#' Crear perfil de función de distribución de densidad
#'
#' @param x tabla original con valores de MCMC
#' @param y estimación de 
#' @param dist 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
createDistribution <- function(x, y, dist = 'log-normal') {
  if (dist == 'log-normal') {
    x %>% 
      mutate(pred = dlnorm(values, y$estimate[['meanlog']], y$estimate[['sdlog']]) )
    
  } else if (dist == 'gamma') {
    x %>% 
      mutate(pred = dgamma(values, y$estimate[['shape']], y$estimate[['rate']]))
  } else {
    print('not recognized')
  }
}


distDF1 <- distDF %>%
  group_by(parameter) %>% nest() %>% 
  add_column(dist = c(rep('log-normal', 4), rep('gamma', 4), 'log-normal')) %>% 
  mutate(
    param = map2(data, dist,  
                 ~findParameters(.x$values, dist = .y)),
    densy = pmap(list(x=data, y=param, dist=dist), createDistribution)
    ) 

g <- distDF1 %>% 
  unnest(densy) %>% 
  ggplot(aes(x=values, colour = parameter, 
             fill=after_scale(alpha(colour, 0.5)))) + 
  geom_line(aes(x=values, y=pred), lty='dashed') +
  geom_density() + 
  facet_wrap(. ~ parameter, ncol=4, scales = 'free') + 
  theme(legend.position = 'none', axis.title.x = element_blank()) + 
  ylab('Densidad')
  
# Almacenamiento de distribuciones empíricas y modelos de distribución
ggsave('001_distBayesParam.pdf', g, 'pdf', 'figures', 1, 8,6)

# Extraer parámetros estimados de distribuciones de probabilidad
distDF2 <- distDF1 %>% 
  mutate(param   = map_df(param, 'estimate')) %>% 
  dplyr::select(-data, -dist, -densy) 

distDF2['meanlog'] <- distDF2$param$meanlog 
distDF2['sdlog']   <- distDF2$param$sdlog 
distDF2['shape']   <- distDF2$param$shape 
distDF2['rate']    <- distDF2$param$rate 

# Crear tabla de resultados 
distGT <- distDF2 %>% 
  dplyr::select(-param) %>% 
  ungroup() %>% 
  gt(rowname_col = 'parameter') %>% 
  fmt_number(columns = 2:5, decimals = 3) %>% 
  fmt_missing(columns = 2:5, missing_text = '-') %>% 
  tab_spanner(columns=2:3, label='Log-Normal') %>%
  tab_spanner(columns=4:5, label='Gamma') %>%
  tab_header(
    title = md('**Parámetros de distribuciones estimadas Stan**'), 
    subtitle = 'Modelo Base estimado con Stan') %>% 
  tab_options(table.font.size = '12px')

gtsave(distGT, '002_distBayesParam.html', file.path('figures') %>% normalizePath())  






