require(rlang)

#' Función que adiciona lineas y puntos, diseñada para observaciones
#'
#' @param data datos originales
#' @param x columna con el eje x
#' @param y columna con el eje Y
#' @param color color de los puntos
#' @param shape forma de los puntos
#'
#' @return
#' @export
#'
#' @examples
#' ggplot() + linedots(data_OBS_PRED_sum, TIME, ME)
#' 
linedots <- function(data, x, y, color='red', shape = 15) {
  x = rlang::ensym(x)
  y = rlang::ensym(y)
  
  return(
    list(geom_line(data = data, aes(x = !!x, y = !!y),  col=color),
         geom_point(data = data, aes(x = !!x, y = !!y), col=color, shape=shape))
  )
}