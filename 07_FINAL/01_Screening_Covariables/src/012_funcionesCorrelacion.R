##########################################################################-
# Correlación de Pearson --------------------------------------------------
##########################################################################-
#' Función de correlación adaptada
#'
#' @param data Tabla de datos 
#' @param x variable independiente (covariable)
#' @param y variable dependiente (desviación eta)
#'
#' @return coeficiente de correlación lineal de Pearson entre \code{x} y 
#' \code{y}
#' @examples
#' corr_test(data_ori, "eta_Cl_SAEM", "AGEA")
#' corr_test(mtcars, 'cyl', 'mpg')
#' 
corr_test <- function(data, x, y) {
  a <- pull(data, x) %>% as.numeric(.)
  b <- pull(data, y) %>% as.numeric(.)
  cor.test(a, b, method = 'spearman', conf.level = 0.95, exact = FALSE)
}

##########################################################################-
#' Lista de correlaciones entre eta y covariables
#'
#' @param eta (carácter) correlación 
#' @param cov_vec (carácter) vector de correlación
#' @param df (data.frame) tabla con datos
#'
#' @return lista con correlación entre covariables y eta
#' @examples
#' corr_ls("eta_Q_SAEM", covariates, data_ori)
#' 
#' mtcars %>% {corr_ls('cyl', colnames((.)[2:11]), (.))}
#' iris %>% {corr_ls('Sepal.Length', colnames((.)[2:4]), (.))}
#' 
corr_ls <- function(eta, cov_vec, df) {
  c_ls <- vector(mode = "list", length = length(cov_vec))
  
  for (i in 1:length(cov_vec)) {
    c_ls[[i]] <- corr_test(df, eta, cov_vec[i])
  }
  return(c_ls %>% set_names(cov_vec))
}

##########################################################################-
#' Procesamiento de lista con correlaciones
#' Esta función llama a corr_ls() y realiza unas modificaciones para generar 
#' una tabla desde una lista.
#'
#' @param df dataFrame
#' @param eta (carácter) correlación seleccionada (variable dependiente)
#' @param covariates vector de covariables a probar
#'
#' @return tabla de datos con correlaciones entre eta seleccionado vs 
#' covariables.
#' @examples
#' corr_table("eta_Cl_SAEM")
#' 
corr_table <- function(eta, covariates, df) {
  corr_ls(eta, covariates, df) %>%
    map_dfr(~ broom::tidy(.x), .id = 'tipo') %>%
    mutate(interp = ifelse(p.value < 0.05, 'Signif.', 'NS'))
}