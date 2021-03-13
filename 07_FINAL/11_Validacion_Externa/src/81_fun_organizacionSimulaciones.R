require(rlang)

#-------------------------------------------------------------------------------#
#' Convierte los resultados de simulx en tibble, adiciona columnas que indican 
#' las poblaciones e individuos para c/u de las poblaciones
#'
#' @param res dataframe con resultados Simulx
#' @param n número de individuos por población
#' @param id_column nombre de columna con identificador único simulación
#' @param pob_column nombre de columna para la población
#' @param ind_column nombre de columna para el individuo en cada población
#'
#' @return
#' @export
#'
#' @examples
#'
separarGruposRes <- function(res,
                             n,
                             id_column = 'id',
                             pob_column = 'poblacion',
                             ind_column = 'individuo') {
  # 
  # Conversión de strings a expresiones
  id_q  <- ensym(id_column)
  pob_q <- ensym(pob_column)
  ind_q <- ensym(ind_column)
  
  tibble(res) %>%
    mutate(
      !!id_q  := as.numeric(as.character(!!id_q)),
      !!pob_q := (!!id_q - 1) %/% n,
      !!ind_q := !!id_q - (!!pob_q * n),
      !!pob_q := !!pob_q + 1
    )
}