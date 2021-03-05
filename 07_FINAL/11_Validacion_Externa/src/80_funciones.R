#-------------------------------------------------------------------------------#
#' Title
#'
#' @param media 
#' @param limite_inferior 
#' @param limite_superior 
#' @param n 
#' @param alpha 
#' @param tipo 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
normalSD <- function(media, limite_inferior, limite_superior, n, alpha = 0.05, tipo='max') {
  z <- qnorm(1-alpha/2)
  e1 <- (limite_superior - media)*sqrt(n)/z
  e2 <- (media - limite_inferior)*sqrt(n)/z
  
  if (tipo == 'max') {
    return( max(e1, e2) )
  } else if (tipo == 'comb') {
    return(c(li = e1, ls = e2))
  } else {
    stop('Tipo no reconocido')
  }
}

#-------------------------------------------------------------------------------#
#' Title
#'
#' @param media 
#' @param limite_inferior 
#' @param limite_superior 
#' @param n 
#' @param alpha 
#' @param tipo 
#'
#' @return
#' @export
#'
#' @examples
lognormalSD <- function(media, limite_inferior, limite_superior, n, alpha = 0.05, tipo='max') {
  z <- qnorm(1-alpha/2)
  e1 <- (log(limite_superior) - log(media))*sqrt(n)/z
  e2 <- (log(media) - log(limite_inferior))*sqrt(n)/z
  
  if (tipo == 'max') {
    return( max(e1, e2) )
  } else if (tipo == 'comb') {
    return(c(li = e1, ls = e2))
  } else {
    stop('Tipo no reconocido')
  }
  
}