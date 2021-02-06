require(rlang)
require(broom)
require(mgcv)

##########################################################################-
# Modelo Aditivos Generalizados -------------------------------------------
##########################################################################-
#' Función de lista con modelos aditivos generalizados (GAM)
#'
#' @param eta (carácter) variable eta
#' @param cov_vec (carácter) vector de covariables
#' @param df (data.frame) tabla de datos
#'
#' @return lista con resultados de regresión GAM entre etas y covariables
#' continuas
#' @examples
#' GAM_ls('eta_Cl_SAEM', cov_vec = covariates[2], df = data_ori)
#' GAM_ls('eta_Cl_SAEM', cov_vec = covariates[c(2:12)], df = data_ori)[[1]]
#' 
#' iris %>% {GAM_ls('Petal.Length', colnames((.)[1]), (.))}
#' 
GAM_ls <- function(eta, cov_vec, df) {
  gam_ls <- vector(mode = "list", length = length(cov_vec))
  
  for (i in 1:length(cov_vec)) {
    gam_ls[[i]] <- gam(f(!!sym(eta),!!expr(s(!!sym(cov_vec[i])))),
      data = df)
  }
  
  return(gam_ls %>% set_names(cov_vec))
}

#-------------------------------------------------------------------------------#
#' Extracción en tabla de significancia de parámetros de alisamiento GAM
#'
#' @param cov_vec 
#' @param df 
#' @param eta (carácter) desviación eta de interés
#'
#' @return tabla con significancia del parámetro de alisamiento en
#' regresión GAM vs covariable eta
#' @examples
#' gam_df("eta_Q_SAEM")
#'
gam_df <- function(eta, cov_vec, df) {
  stopifnot(is_character(eta))
  
  GAM_ls(eta, cov_vec, df)  %>%
    map_dfr( ~ tidy(.x) , .id = 'tipo')
}

##########################################################################-
# Función LRT para modelos GAM --------------------------------------------
##########################################################################-
#' Función conversión lista de GAM para cada modelo de covariable vs modelo 
#' base
#' @param eta (carácter) desviación eta de interés
#' @param m0 (objeto GAM) modelo sin inclusión de las covariables sólo 
#' intercepto
#'
#' @return tabla de datos con valores de AIC vs grados de libertad de modelo 
#' base (reducida) y modelo full.
#' @examples
#' m0 <- mgcv::gam(formula = eta_Cl_SAEM ~ 1, data = data_ori)
#' lrt_GAM_df('eta_Cl_SAEM', m0)
#' 
lrt_GAM_df <- function(eta, m0) {
  a <- GAM_ls(eta, covariates[c(2:12)], df = data_ori) %>%
    map(~ AIC(.x, m0)) %>%
    map(~ as.data.frame(.x)) %>%
    map(~ rownames_to_column(.x, var = 'Parametro')) %>%
    map_dfr( ~ as.data.frame(.x), .id = 'Covariable') %>%
    mutate(Parametro = case_when(Parametro == '.x' ~ "full",
                                 Parametro == 'm0' ~ "reduced"))
  
  a1 <- a %>%
    select(-df) %>%
    spread(key = 'Parametro', value = 'AIC')
  
  a2 <- a %>%
    select(-AIC) %>%
    spread(key = 'Parametro', value = 'df')
  
  left_join(a1,
            a2,
            by = c('Covariable'),
            suffix = c('_AIC', '_df')) %>%
    mutate(LRT = reduced_AIC - full_AIC + 2 * (full_df - reduced_df)) %>%
    mutate(p = pchisq(LRT, 1, lower.tail = FALSE),
           interp = if_else(p <= 0.05, '< 0.05', 'NS')) %>%
    add_column(ETA = eta, .before = "Covariable")
}

##########################################################################-
# Función LRT para modelos GLM --------------------------------------------
##########################################################################-
#' Tabla de comparación de modelos glm ('lm') para cada coviarable vs modelo 
#' base sin inclusión de covariable
#'
#' @param eta (carácter) desviación eta de interés
#' @param m0 (objeto GLM) modelo base (reducido) con sólo intercepto
#' @return tabla de datos con LRT entre modelo completo (covariable + 
#' intercepto) vs modelo reducido (sólo intercepto). Sólo se acepta la reg-
#' resión 'lm' con pendiente e intercepto
#'
#' @examples
#' m0 <- glm(eta_Cl_SAEM ~ 1, data = data_ori)
#' lrt_GLM_df('eta_Cl_SAEM', m0)
#' 
lrt_GLM_df <- function(eta, m0) {
  a <- GLM_ls_1(eta, covariates, data_ori) %>%
    map( ~ AIC(.x, m0)) %>%
    map( ~ as.data.frame(.x)) %>%
    map( ~ rownames_to_column(.x, var = 'Parametro')) %>%
    map_dfr(~ as.data.frame(.x), .id = 'Covariable') %>%
    mutate(Parametro = case_when(Parametro == '.x' ~ "full",
                                 Parametro == 'm0' ~ "reduced"))
  
  a1 <- a %>%
    select(-df) %>%
    spread(key = 'Parametro', value = 'AIC')
  
  a2 <- a %>%
    select(-AIC) %>%
    spread(key = 'Parametro', value = 'df')
  
  left_join(a1, a2, by = c('Covariable'), suffix = c('_AIC', '_df')) %>%
    mutate(LRT = reduced_AIC - full_AIC + 2 * (full_df - reduced_df)) %>%
    mutate(p = pchisq(LRT, 1, lower.tail = FALSE),
           interp = if_else(p <= 0.05, '< 0.05', 'NS')) %>%
    add_column(ETA = eta, .before = "Covariable")
}
