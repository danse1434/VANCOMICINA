#' Función para adicionar formatos condicionales en GT basados en contenedores
#' 
#' Se interpretan los contenedores como rangos numéricos en donde se debe 
#' considerar un color (en este caso diferentes niveles de significancia).
#'
#' @param table tabla original sin colores
#' @param param nombre de columnas donde adicionar los formatos
#' @param crit criterios para definir contenedores (k-1)
#' @param col_vec vectores de colores para contenedores (k)
#' @param alpha transparencia de colores
#'
#' @return
#' @export 
#' Tabla con adición de formatos condicionales
#'
#' @examples
#' v <- c("SW", "AD", "CM", "Lf", "Pe", "SF") 
#' gt_res_norm %>% 
#' fun_param(v)
#' 
formatoCondicional <- function(table,
                      param,
                      crit = c(0.01, 0.01, 0.05),
                      col_vec = c("#76EE00", "#FFD700", "#1C86EE", "#FF4040"),
                      alpha = 0.05) {
  gt_data <- table
  n_crit <- length(crit)
  n_cols <- length(col_vec)
  
  stopifnot(n_crit == n_cols - 1)
  
  fun_estilo <- function(gt, color, alpha, param_quo, expr1){
    
    tab_style(data = gt, 
      style = list(cell_fill(color = alpha(color, alpha))),
      locations = cells_body(columns = vars(!!param_quo),
                             rows = eval(expr1)
      ))
  }
  
  for (i in seq_along(param)) {
    p <- eval(param[i])
    param_quo <- rlang::ensym(p)
    
    for (i in seq_along(crit)) {
      # Contenedor más bajo
      if (i == 1) {
        gt_data <- gt_data %>%
          fun_estilo(col_vec[i], alpha, param_quo, expr(!!param_quo > crit[i]))
      } 
      # Contenedor más alto
      else if (i == n_crit) {
        gt_data <- gt_data %>%
          fun_estilo(col_vec[n_cols], alpha, param_quo, expr(!!param_quo < crit[i]))
      }
      # Contenedores intermedios
      else {
        gt_data <- gt_data %>%
          fun_estilo(col_vec[i], alpha, param_quo, expr(!!param_quo >= crit[i-1] & !!param_quo < crit[i]))
      }
    }
  }
  return(gt_data)
}          