require(data.table)
require(rlang)

#-------------------------------------------------------------------------------#
#' Calcula indicador PK-PD AUC/MIC con un sólo criterio
#'
#' @param indicador (str) indicador farmacodinámico
#' @param MIC_vec (vector numérico) vector de MIC a probar
#' @param crit (decimal) criterio a evaluar p.ej. AUC/MIC > 400
#'
#' @return
#' @export
#'
#' @examples
#' resPTA1_AUC(600, c(1, 2, 4), 400)
#' resPTA1_AUC(100, 2^(-3:1), 400)
#' resPTA1_AUC(100, 2^(seq(-3, 1, by = 0.1)), 400)
#' 
resPTA1_AUC <- function(indicador, MIC_vec, crit) {
  
  M <- data.table(MIC = MIC_vec,
                  IND = map_dbl(MIC_vec, ~ indicador >= crit * .x))
  
  return(M)
}

#-------------------------------------------------------------------------------#
#' Manipulación de DataTable
#'
#' @param data tabla de datos
#' @param MIC_vec vector de MIC
#' @param pkpd función de indicador PK-PD
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
resPTA_Tabla <- function(data, MIC_vec, pkpd, crit, ..., g) {
  
  df <- data %>%
    .[, DATA := map(auc, ~ pkpd(.x, MIC_vec, crit))] %>%
    unnest(DATA) %>% 
    group_by(MIC) %>% 
    summarise(
      mn = mean(IND), 
      g = g)
  
  return(df)
}

