require(rlang)

#-------------------------------------------------------------------------------#
#' Función para la creación de fórmulas en lm() con evaluación tidy
#'
#' @param x variable dependiente (respuesta eta)
#' @param y variable independiente (covariable)
#' @param flatten condicional si se debe aplanar el quosure
#'
#' @return objeto de tipo fórmula que utiliza las variables con environment
#' @export
#' @examples 
#' f(disp, am + (!! sym(var)))
#' lm(f(mpg, cyl), data = mtcars)
#' 
f <- function(x, y, flatten = TRUE) {
  # Creación de quosures
  xquo <- enquo(x) 
  yquo <- enquo(y)
  
  # Environments de quosures debe ser el mismo en \code{x} y \code{y}
  # Podrían ser diferentes si son forwarded mediante puntos
  env <- get_env(xquo) # Environment de \code{x} 
  stopifnot(identical(env, get_env(yquo)))
  
  # Aplanar los quosures. Esto avisa al usuario, si los quosures son anidados. 
  # Estos no son soporados por funciones como lm().
  if (flatten) {
    xquo <- quo_squash(xquo, warn = TRUE)
    yquo <- quo_squash(yquo, warn = TRUE)
  }
  
  new_formula(xquo, yquo, env = env)
}

#-------------------------------------------------------------------------------#
#' Creación de lista con regresiones GLM
#'
#' @param eta desviación \code{eta} de interés
#' @param cov_vec vector de covariables de interés
#' @param intercepto condicional indica si la regresión tiene intercepto
#' @param df tabla de datos 
#' @return
#' @export 
#' Permite obtener una lista con regresión glm para cada covariable y 
#' desviación eta de interés
#' @examples 
#' 
#' GLM_ls_1("eta_Cl_SAEM", covariates, data1)
#' 
#' pars <- c("cyl", "disp", "hp", "drat", "wt", 
#' "qsec", "vs", "am", "gear", "carb")
#' 
#' GLM_ls_1('mpg', pars, mtcars, tipo = 'lm')
#'
GLM_ls_1 <- function(eta,
                     cov_vec,
                     df,
                     intercepto = TRUE,
                     tipo = 'glm') {
  
  glm_list <- vector(mode = "list", length = length(cov_vec))
  
  if (tipo == 'lm') {
    for (i in 1:length(cov_vec)) {
      glm_list[[i]] <-
        lm(f(!!sym(eta),!!sym(cov_vec[i])), data = df)
    }
  } else if (tipo == 'glm') {
    if (intercepto == TRUE) {
      for (i in 1:length(cov_vec)) {
        glm_list[[i]] <-
          glm(f(!!sym(eta),!!sym(cov_vec[i])), data = df)
      }
    } else {
      for (i in 1:length(cov_vec)) {
        glm_list[[i]] <-
          glm(f(!!sym(eta), 0+!!sym(cov_vec[i])), data = df)
      }
    }
  }
  return(glm_list %>% set_names(cov_vec))
}

#-------------------------------------------------------------------------------#
#' Procesamiento de lista con regresiones GLM para obtener una tabla con 
#' valores de coeficientes de cada modelo.
#'
#' @param eta tipo de desviación necesitada
#' @param inter condicional si los modelos deben tener intercepto o no
#'
#' @return
#' Esta función llama a GLM_ls_1(), y la procesa para la obtención de un 
#' data.frame. El data.frame contiene sólo los resultados de los coeficie-
#' ntes del modelo, y tiene opciones para obtener regresiones GLM() con y 
#' sin intercepto; así como regresiones lm(). 
#' @export
#' @examples
#' glm_df("eta_Cl_SAEM", TRUE, 'lm')
#' 
glm_df <- function(df, eta, inter, tipo = 'glm') {
  stopifnot(is_character(eta))
  stopifnot(is_bool(inter))
  
  GLM_ls_1(eta, covariates, df, intercepto = inter, tipo = tipo) %>%
    map_dfr(~ broom::tidy(.x), .id = 'tipo')
}