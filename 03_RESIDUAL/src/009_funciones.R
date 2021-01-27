RES_C <- function(data, filt, x, y, xlab, ylab, ...) {
  ##########################################################################-
  # Volver las variables x y y en expresiones para evaluación tardía
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  
  df1 <- data %>% filter(Resid == filt)
  
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##  Cálculo de percentiles empiricos
  ##  1 Tomar la variable de datos a explorar.
  ##  2 Crear una variable que tome 6 grupos a partir de los datos ordenados 
  ##  con la función dplyr::ntile().
  ##  3 Agrupar por la variable recién creado.
  ##  4 Resumir por la media de TAD, y los percentiles P5%, P50%, y P95% de 
  ##  los datos de residuales.
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  A <- df1 %>%
    mutate(gr = ntile(time, 6)) %>% 
    group_by(gr) %>% 
    summarise(TIME = mean(time), 
              ME = quantile(x = !!y, probs = 0.50), 
              LI = quantile(x = !!y, probs = 0.05), 
              LS = quantile(x = !!y, probs = 0.95))
  ##########################################################################-
  # Crear el gráfico de residuales con las especificaciones mostradas
  g1 <- df1 %>%
    ggplot(mapping = aes(x = !!x, y = !!y)) +
    geom_hline(yintercept = 0) +
    geom_point(col = '#4682B4') +
    coord_cartesian(ylim = c(-2.5, 5.0)) +
    xlab(xlab) + ylab(ylab) +
    stat_smooth(method = 'loess', formula = y ~ x, se = FALSE,
                col = '#EBA213', lty = 'solid', size = 1) +
    # geom_text_repel(data = filter(df1, abs(!!y) > 2), 
                    # aes(label = paste0(ID,' (',time,')')),
                    # size=3.5, ylim=c(4,NA) ) +
    geom_line(data = A, aes(TIME, ME),
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_line(data = A, aes(TIME, LI),
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_line(data = A, aes(TIME, LS),
              col = '#EBA213', lty = 'dashed', size = 1.2)
  return(g1)
}


#-------------------------------------------------------------------------------#
RES_PRE <- function(data, filt, x, y, xlab, ylab) {
  ##########################################################################-
  # Volver las variables x y y en expresiones para evaluación tardóa
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  df1 <- data %>% filter(Resid == filt)
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##  Calculo de percentiles empiricos
  ##  1 Tomar la variable de datos a explorar.
  ##  2 Crear una variable que tome 6 grupos a partir de los datos ordenados 
  ##  con la función dplyr::ntile().
  ##  3 Agrupar por la variable reción creado.
  ##  4 Resumir por la media de TAD, y los percentiles P5%, P50%, y P95% de 
  ##  los datos de residuales.
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  A <- df1 %>%
    mutate(gr = ntile(!!x, 6)) %>% 
    group_by(gr) %>% 
    summarise(PRED = mean(!!x), 
              ME = quantile(x = !!y, probs = 0.50), 
              LI = quantile(x = !!y, probs = 0.05), 
              LS = quantile(x = !!y, probs = 0.95))
  ##########################################################################-
  # Crear el grófico de residuales con las especificaciones mostradas
  df1 %>% 
    ggplot(mapping = aes(x = !!x, y = !!y)) +
    geom_hline(yintercept = 0) +
    geom_point(col = '#4682B4') +
    coord_cartesian(ylim = c(-2.5, 2.5)) +
    xlab(xlab) + ylab(ylab) +
    stat_smooth(method = 'loess', formula = y ~ x, se = FALSE, 
                col = '#EBA213', lty = 'solid', size = 1) +
    geom_line(data = A, aes(PRED, ME), 
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_line(data = A, aes(PRED, LI), 
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_line(data = A, aes(PRED, LS), 
              col = '#EBA213', lty = 'dashed', size = 1.2) %>% 
    return(.)
}
#-------------------------------------------------------------------------------#
# Función que aplica una batería de pruebas de normalidad a los datos
## *data* ingrese el objeto de datos a analizar.
## *vector* ingrese la posición de las columnas donde se encuentran los 
## residuales de interós.
## *alpha* ingrese el valor de probabilidad para los tests. 
normtest_battery <- function(data, var) {
  X          <- pull(data, var)
  df         <- vector('list', 6L)
  df[['SW']] <- shapiro.test(X)$p.value # Shapiro-Wilks
  df[['AD']] <- nortest::ad.test(X)$p.value #Anderson-Darling
  df[['CM']] <- nortest::cvm.test(X)$p.value #Cramer von Mises
  df[['Lf']] <- nortest::lillie.test(X)$p.value #Liliefors
  df[['Pe']] <- nortest::pearson.test(X)$p.value #Pearson
  df[['SF']] <- nortest::sf.test(X)$p.value #Shapiro Francia
  
  return(df)
}


#-------------------------------------------------------------------------------#
#' Creación de gráfico de residuales combinados (dos métodos)
#'
#' @param data dataframe
#' @param x nombre columna con eje X
#' @param y nombre columna con eje X
#' @param tipo nombre columna con tipo de método
#' @param bins número de bins para cálculo de percentiles empíricos
#' @param xlab etiqueta de eje X
#' @param ylab etiqueta de eje Y
#' @param xlim límites de X
#' @param ylim límites de Y 
#' @param nombreTipo Nombre de los tipos Default: c('Micro.', 'Quimiol.')
#' @param colourp paleta de dos colores. Default: c('green4', 'blue')
#' @param legend posición de la leyenda. Default: 'none'
#' @param alpha nivel de significancia. Default: 0.05
#'
#' @return Gráfico de residuales vs predicciones
#' @export
#'
#' @examples
#' RES_COMB(x = prediction_pwRes, y = pwRes, xspline = prediction_pwRes, 
#' yspline = prediction_pwRes_spline, perc_data = y1_prediction_percentiles_pwRes,
#' xlab = 'PRED', ylab = 'WRES')
#' 
RES_COMB <- function(data, x, y, tipo, id, bins, 
                    xlab, ylab, xlim, ylim,
                    nombreTipo = c('Micro.', 'Quimiol.'),
                    colourp = c('green4', 'blue'), 
                    shapep = c(16, 1),
                    legend = 'none', alpha=0.05, threshold=6) {
  x_quo = ensym(x)
  y_quo = ensym(y)
  t_quo = ensym(tipo)
  id_quo = ensym(id)
  
  if (missing(xlim)) {
    pull(data, !!x_quo) %>% {c(min(.), max(.))} -> xlim
  }
  
  if (missing(ylim)) {
    pull(data, !!y_quo) %>% {c(min(.), max(.))} -> ylim
  }
  
  if (missing(tipo)) {
    t_quo = 1
  }
  
  # Cálculo de residuales por bins
  df1 <- data %>%
    mutate(gr = ntile(!!x_quo, bins)) %>%
    group_by(gr) %>% 
    summarise(
      t  = median(!!x_quo),
      me = median(!!y_quo),
      li = quantile(!!y_quo, probs = alpha/2),
      ls = quantile(!!y_quo, probs = 1 - alpha/2), 
      .groups = 'drop'
    )
  
  # Componentes estructurales
  g <- data %>% 
    mutate(Tipo = ifelse(!!t_quo == 1, nombreTipo[1], nombreTipo[2])) %>% 
    ggplot(mapping = aes(x = !!x_quo, y = !!y_quo)) +
    geom_hline(yintercept = 0) +
    geom_point(aes(color = Tipo, shape=Tipo)) +
    stat_smooth(
      method = 'loess', formula='y~x', col='gray', fill=alpha('gray', 0.0)
    ) +
    geom_line(data=df1, aes(x=t, y=me), col = '#EBA213', lty = 'dashed', size = 1) +
    geom_line(data=df1, aes(x=t, y=li), col = '#EBA213', lty = 'dashed', size = 1) +
    geom_line(data=df1, aes(x=t, y=ls), col = '#EBA213', lty = 'dashed', size = 1)
  
  # Componentes estéticos
  g <- g +
    geom_text_repel(data = dplyr::filter(data, abs(!!y_quo) > threshold),
                    aes(label = !!id_quo)) +
    scale_color_manual(values = colourp, breaks = nombreTipo, name = 'Método') +
    scale_shape_manual(values  = shapep, breaks = nombreTipo, name = 'Método') +
    xlab(xlab) + ylab(ylab) +
    coord_cartesian(xlim, ylim=ylim) +
    theme(legend.position = legend)
  
  return(g)
}


