require(purrr)

#-------------------------------------------------------------------------------#
#' Extraer muestras para dos variables en las cadenas de un análisis por Stan
#'
#' @param stan_mcmc formato CODA obtenido a partir de Stan   
#' @param param1 parámetro 1
#' @param param2 parámetro 2
#'
#' @return tabla con muestras de parámetros en análisis bayesiano 
#' @export
#'
#' @examples
#' getEstimParamBayes(fit_mcmc, 'CLHat', 'V2Hat')
#' 
getEstimParamBayes <- function(stan_mcmc, param1, param2) {
  pos <- function(pos) {
    stan_mcmc %>% 
      map(., ~dimnames(.x)[[2]]) %>% 
      map_dbl(., ~which(.x == pos))
  }
  stopifnot(is.numeric(pos(param1)),
            is.numeric(pos(param2)) )
  
  pmap(list(df = stan_mcmc, p1 = pos(param1), p2 = pos(param2)), 
       function(df, p1, p2) df[, c(p1, p2)]) %>% 
    map_dfr(~as_tibble(.x), .id = 'Chain')
  
}

#-------------------------------------------------------------------------------#
#' Ejecutar Script entre las líneas definidas
#'
#' @param file archivo a analizar
#' @param start línea inicial
#' @param end línea final
#' @param ... 
#'
#' @return 
#' Ejecución de script
#' @export
#'
#' @examples
#' source2('./src/051_fun_funcion2Cptm.R', 1, 20)
#' 
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}