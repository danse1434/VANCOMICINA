#-------------------------------------------------------------------------------#
#' Crear diagramas de dispersión entre dos covariables
#'
#' @param x - covariable 1
#' @param y - covariable 2 
#'
#' @return gráfico de tipo scatterplot
#' @export
#'
#' @examples
#' corr_plot(data_corr_1, eta_V1_simulated, eta_Cl_simulated)
#' 
corr_plot <- function(data, x, y) {
  
  xquo <- rlang::enquo(x)
  yquo <- rlang::enquo(y)
  
  # Obtener valores máximos de distribución
  xmaxLim <- max(abs(dplyr::pull(data, !!xquo)))
  ymaxLim <- max(abs(dplyr::pull(data, !!yquo)))
  
  # Gráfico
  G <- mutate(data, ID = factor(ID)) %>%
    ggplot(aes(x = !!xquo, y = !!yquo)) +
    geom_point(aes(col = ID)) +
    stat_smooth(method = 'lm', formula = 'y~x') +
    scale_color_viridis_d(alpha = 0.5) +
    scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
    xlab('') + ylab('') +
    coord_cartesian(xlim = c(-xmaxLim, xmaxLim),
                    ylim = c(-ymaxLim, +ymaxLim)) +
    theme(legend.position = "none")
  return(G)
}

#-------------------------------------------------------------------------------#
#' Crear histogramas para una variable
#'
#' Se especifican variables a modelar; se crea una serie de distirbuciones para cada
#' simulaicón de parámetro eta por MCMC
#' Se calculan las medias y desviaciones estándar por grupo y se adicionan al 
#' gráfico en forma de un loop.
#' @param data tabla de datos de distribución eta
#' @param x nombre de variable de interés
#' @param parameter etiqueta para con la variable para mostrar
#'
#' @return gráficos de histograma (diagonales en matriz de correlación)
#' @export
#'
#' @examples
#' hist_plot(data_corr_1, eta_V1_simulated, expression(eta ~ (V[1])))
#' 
hist_plot <- function(data, x, parameter) {
  x_quo <- rlang::ensym(x)
  
  grafico <- mutate(data, ID = factor(ID)) %>% 
    ggplot(aes(x = !!x_quo)) + 
    geom_histogram(aes(y = ..density.., fill = ID), bins = 30) +  
    xlab('') + ylab('') +
    theme(legend.position = "none") +
    scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
    scale_fill_viridis_d(alpha = 0.5)
  
  distribucionesNormales <- vector('list', 14L)
  
  for (i in 1:14) {
    # Cálculo de distribución normal
    X <-  data_corr_1[data_corr_1$ID == i, ] %>%
      summarise(mean = mean(!!x_quo),
                sd = sd(!!x_quo))
    # Cálculo de distribuciones normales para cada individuo
    distribucionesNormales[[i]] <- stat_function(
      fun = dnorm,
      args = list(mean = X[['mean']],
                  sd = X[['sd']]),
      size = 0.7,
      col = alpha('black', 0.8)
    )
  }
  
  graficoTotal = grafico + distribucionesNormales + 
    annotate(geom = "label", label = parameter,
      # x = 0.95 * max(pull(data_corr_1, !!x_quo)),
      x = Inf, y = Inf,
      hjust = 1, vjust = 1
    )
  
  return(graficoTotal)
}

#-------------------------------------------------------------------------------#
#' Aplicar formato de condicional
#' Esta función aplica formato condicional con la especificación de colores para 
#' tres niveles de valor de covarianza
#'
#' @param gt : objeto gt
#' @param param : nombre de la columna a la que se aplicará el formato condicional
#'
#' @return lista con especificación de formato condicional
#' @export
#'
#' @examples
#' gt::gt() %>% 
#' fun_param('Cl_pop')
#'  
formatoCondicionalCovarianza <- function(gt, param) {
  param_quo <- rlang::ensym(param)
  
  listaCondicional <- gt %>% 
    gt::tab_style(
      locations = gt::cells_body(columns = vars(!!param_quo), rows = !!param_quo >= 0.9),
      style = list(gt::cell_fill("red"),
                   gt::cell_text(weight = 'bold', color = 'white'))
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(
        columns = vars(!!param_quo),
        rows = !!param_quo >= 0.8 & !!param_quo < 0.9
      ),
      style = list(gt::cell_fill("orange"),
                   gt::cell_text(weight = 'bold'))
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(
        columns = vars(!!param_quo),
        rows = !!param_quo >= 0.5 & !!param_quo < 0.8
      ),
      style = list(gt::cell_fill("yellow"))
    )
  
  return(listaCondicional)
}