# Cargar paquetes necesitados
require(rlang)


#-------------------------------------------------------------------------------#
#' Función de gráficos diagnósticos de bondad de ajuste
#' Esta función asume que existe una variable de tipo 
#' 
#' @param data dataframe
#' @param x nombre columna con eje X
#' @param y nombre columna con eje X
#' @param tipo nombre columna con tipo de método
#' @param xlab etiqueta de eje X
#' @param ylab etiqueta de eje Y
#' @param xlim límites de X
#' @param ylim límites de Y 
#' @param nombreTipo Nombre de los tipos Default: c('Micro.', 'Quimiol.')
#' @param colourp paleta de dos colores. Default: c('green4', 'blue')
#' @param legend posición de la leyenda. Default: 'none'
#'
#' @return
#' @export este se basa en un dataframe, se eligen los nombres de las columnas donde está el eje X y eje Y
#'
#' @examples
#' GOF_PRED(y_gof, popPred, y, YTYPE, 
#' xlab = 'PRED', ylab = 'OBS', xlim = c(0, 60), ylim = c(0, 60))
#' 
GOF_PRED <- function(data, x, y, tipo, 
                     xlab, ylab, xlim, ylim,
                     nombreTipo = c('Micro.', 'Quimiol.'),
                     colourp = c('green4', 'blue'), 
                     legend = 'none') {
  
  x_quo = ensym(x)
  y_quo = ensym(y)
  t_quo = ensym(tipo)
  
  if (missing(xlim)) {
    pull(data, !!x_quo) %>% {c(min(.), max(.))} -> xlim
  }
  
  if (missing(ylim)) {
    pull(data, !!y_quo) %>% {c(min(.), max(.))} -> ylim
  }
  
  if (missing(tipo)) {
    t_quo = 1
  }
  
  # Componentes estructurales
  g <- data %>% 
    mutate(Tipo = ifelse(!!t_quo == 1, nombreTipo[1], nombreTipo[2])) %>% 
    ggplot(aes(x=!!x_quo, y=!!y_quo, color=Tipo)) +
    geom_point(shape=1) + 
    geom_abline(slope = 1, intercept = 0, lty = 'dotted') + 
    stat_smooth(aes(fill = Tipo), alpha=0.1, formula='y ~ x', method = 'lm')
  
  # Componentes estéticos
  g <- g + 
    scale_color_manual(values = colourp, breaks = nombreTipo, name = 'Método') +
    scale_fill_manual(values = colourp, breaks = nombreTipo, name = 'Método') +
    xlab(xlab) + ylab(ylab) + 
    coord_cartesian(xlim, ylim=ylim) +
    theme(legend.position = legend)
  
  return(g)
}

#-------------------------------------------------------------------------------#
#' Creación de gráfico de residuales
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
#' RES_PRE(x = prediction_pwRes, y = pwRes, xspline = prediction_pwRes, 
#' yspline = prediction_pwRes_spline, perc_data = y1_prediction_percentiles_pwRes,
#' xlab = 'PRED', ylab = 'WRES')
#' 
RES_PRE <- function(data, x, y, tipo, id, bins, 
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
