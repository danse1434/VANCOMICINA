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
#' @param data DataFrame original
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
RES_TSFD <- function(data, x, y, xlab, ylab, id, bins=5, alpha=0.05) {
  x_q  <- rlang::ensym(x)
  y_q  <- rlang::ensym(y)
  id_q <- rlang::ensym(id)
  # xspline <- rlang::ensym(xspline)
  # yspline <- rlang::ensym(yspline)
  # stopifnot(is.data.frame(perc_data) == TRUE)
  
  df1 <- data %>%
    mutate(gr = ntile(!!x_q, bins)) %>%
    group_by(gr) %>% 
    summarise(
      t  = median(!!x_q),
      me = median(!!y_q),
      li = quantile(!!y_q, probs = alpha/2),
      ls = quantile(!!y_q, probs = 1 - alpha/2)
    )
  # print(df1)
  
  data %>% 
    ggplot(mapping = aes(x = !!x_q, y = !!y_q)) +
    geom_hline(yintercept = 0) +
    geom_point(col = 'black') +
    stat_smooth(method = 'loess', formula='y~x', col='#1C949E', fill=alpha('#1C949E', 0.01)) +
    geom_line(data=df1, aes(x=t, y=me), col = '#EBA213', lty = 'dashed', size = 1) +
    geom_line(data=df1, aes(x=t, y=li), col = '#EBA213', lty = 'dashed', size = 1) + 
    geom_line(data=df1, aes(x=t, y=ls), col = '#EBA213', lty = 'dashed', size = 1) + 
    geom_text_repel(data = dplyr::filter(data, abs(!!y_q) > 5),
                    aes(label = !!id_q))  +
    coord_cartesian(ylim = c(-5.0,+5.0)) +
    xlab(xlab) + ylab(ylab) %>% return(.)
}