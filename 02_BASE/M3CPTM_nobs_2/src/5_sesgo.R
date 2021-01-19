# Ejecutar scripts para obtener datos de residuales
source('M3CPTM_nobs_2/src/1_script_graficos.R', encoding = 'UTF-8')
source('M3CPTM_nobs_2/src/3_script_graficos_y2.R', encoding = 'UTF-8')



#-------------------------------------------------------------------------------#
# Modificación residuales ------------------------------------------
#................................................................................
#' 1 Unir data.frames
#' 2 Adicionar columna con indicador de método
#................................................................................

yt_residuals <- y1_residuals %>%
  bind_rows(y2_residuals) %>%
  add_column(Metodo = rep(c('M1', 'M2'), each = dim(y1_residuals)[1])) 

#' Obtener sesgo e imprecisión
#'
#' @param x residuales debe ser un vector
#' @param N tamaño de vector *x*
#'
#' @return lista con MPE y MRPE
#' @export
#'
#' @examples
#' ~funSesgo(.x)
#' 
funSesgo <- function(x, N) {
  mn <- mean(x, na.rm = TRUE)
  cv <- sum(x^2, na.rm = TRUE) / (N - mn^2)
  
  return(list(mn = mn, cv = cv))
}

#-------------------------------------------------------------------------------#
# Aplicación de funSesgo a los residuales unificados

yt_residuals %>%
  rename('iwRes.mn' = 'iwRes_mean',
         'iwRes.mo' = 'iwRes_mode') %>% 
  summarise(across(
    c('pwRes', 'iwRes.mn', 'iwRes.mo','npde'),
    list(
      Sesgo = ~ funSesgo(.x, 192)[[1]],
      Imprecision = ~ funSesgo(.x, 192)[[2]]
    )
  )) %>% 
  pivot_longer(
    cols = everything(),
    names_to = c('variable', 'estadistico'),
    values_to = 'Variable', names_sep = '_'
  ) %>% 
  pivot_wider(
    names_from = 'estadistico', 
    values_from = 'Variable'
  )

# A tibble: 4 x 3
# variable   Sesgo Imprecision
# <chr>      <dbl>       <dbl>
#   1 pwRes    -0.0806       0.755
# 2 iwRes.mn -0.117        0.846
# 3 iwRes.mo -0.0473       0.838
# 4 npde     -0.118        0.816
