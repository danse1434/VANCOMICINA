#-------------------------------------------------------------------------------#
#' Función de gráficos diagnósticos de bondad de ajuste
#' Esta función asume que existe una variable de tipo 
#' 
#' @param df dataFrame inicial
#' @param x variable en eje X
#' @param y variable en eje Y
#' @param colourp color de gráfico diagnóstico
#' @param xlab etiqueta de eje X
#' @param ylab etiqueta de eje Y
#' @param xlim límite de eje X
#' @param ylim límite de eje Y
#'
#' @return
#' @export ggplot2 con una figura de bondad de ajuste del modelo
#'
#' @examples
#' GOF_PRED(residuals, 'mean_cObsPred', 'cObs', 'blue', 
#' 'PRED', 'OBS', 
#' xlim = c(0,45), ylim = c(0,45))
#' 
GOF_PRED <- function(df, x, y, colourp, xlab, ylab, xlim, ylim) {
  x_q = rlang::ensym(x)
  y_q = rlang::ensym(y)
  
  if (missing(xlim)) {
    pull(df, x) %>% {c(min(.), max(.))} -> xlim
  }
  
  if (missing(ylim)) {
    pull(df, y) %>% {c(min(.), max(.))} -> ylim
  }
  
  df %>% 
    ggplot(aes(x= !!x_q, y = !!y_q)) +
    geom_point(shape = 1) +
    xlab(xlab) + ylab(ylab) +
    geom_abline(slope = 1, intercept = 0, lty = 'dotted') + 
    stat_smooth(formula = 'y~x', method = 'loess', se = T, 
                fill = alpha(colourp, 0.05), colour = colourp) + 
    coord_cartesian(xlim = xlim, ylim = ylim) %>% 
    return(.)
}

#-------------------------------------------------------------------------------#
#' Función de gráficos de residuales vs concentración
#'
#' @param x variable en eje X
#' @param y variable en eje Y
#' @param xspline variable de eje X en tabla de spline
#' @param yspline variable de eje Y en tabla de spline
#' @param perc_data tabla de percentiles
#' @param xlab etiqueta de eje X
#' @param ylab etiqueta de eje Y
#'
#' @return se obtiene un gráfico de residuales con datos de *y2_residuals* y 
#' *y2_spline*
#' @export
#'
#' @examples 
#' RES_TSFD(x = time, y = pwRes, xspline = time_pwRes, yspline = time_pwRes_spline,
#' perc_data = y2_time_percentiles_pwRes, xlab = 'TSFD', ylab = 'PWRES')
#' 
RES_TSFD <- function(x, y, xspline, yspline, perc_data, xlab, ylab) {
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  xspline <- rlang::ensym(xspline)
  yspline <- rlang::ensym(yspline)
  stopifnot(is.data.frame(perc_data) == TRUE)
  
  y2_residuals %>% 
    ggplot(mapping = aes(x = !!x, y = !!y)) +
    geom_hline(yintercept = 0) +
    geom_point(col = '#4682B4') +
    geom_line(y2_spline, 
              mapping = aes(x = !!xspline, y = !!yspline), 
              col = '#EBA213', lty = 'solid', size = 1) +
    geom_line(perc_data, 
              mapping = aes(x = !!x, y = empirical_median), 
              col = '#EBA213', lty = 'dashed', size = 1.2) + 
    geom_line(perc_data, 
              mapping = aes(x = !!x, y = empirical_lower), 
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_line(perc_data, 
              mapping = aes(x = !!x, y = empirical_upper), 
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_text_repel(data = filter(y2_residuals, abs(!!y) > 2), 
                    aes(label = ID))  +
    coord_cartesian(ylim = c(-5.0,+5.0)) +
    xlab(xlab) + ylab(ylab) %>% return(.)
}