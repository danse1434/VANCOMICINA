# DataFrame con parámetros

source(file.path('src', '069_paquetesSimulacion.R'), encoding = 'UTF-8')


#-------------------------------------------------------------------------------#
#' Creación DF
#'
#' @param p 
#' @param N 
#'
#' @return
#' @export
#'
#' @examples
#' 
creacionDF <- function(p, N) {
  name_list <- names(p)
  
  DF <- data.frame(ID = 1:N)
  
  for (i in name_list) {
    DF[i] <- rep(p[i], N) 
  }
  
  return(tibble(DF))
}
