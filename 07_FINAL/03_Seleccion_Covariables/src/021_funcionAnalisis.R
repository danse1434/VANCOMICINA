#-------------------------------------------------------------------------------#
#' Modificación del set de datos de covariables
#' 
#' Con este procedimiento se toma el set de datos de seguimiento de trayectoria 
#' de algoritmos de búsqueda automática. Se seleccionan las covariables a incluir 
#' y excluir del análisis. Se lleva a formato expandido y se crea una nueva 
#' variable *par* que relaciona el parámetro y la covariable. Este se utiliza como 
#' un factor ordenado respecto a la lista de covariables del modelo.
#'
#' @param data (data.frame)
#' @param inclusion (string) tiene los parámetros a incluir, así como las variables 
#' _iteración_ y _parámetro_ en formato regex.
#' @param exclusion (string) tiene los parámetros a excluir que no se evaluaron 
#' con el algoritmo.
#' @param covariable (string) tiene sólo las covariables a incluir en formato regex
#'
#' @return
#' @export
#' data.frame con modificaciones
#'
#' @examples
#' evalParesCov(dataFrame)
#' 
evalParesCov <-
  function(data,
           inclusion = '^(logt|iter|par|SEXF|OFV|BIC|opt)',
           exclusion = '(RAN|RAL|ABGDL)$',
           covariable = '^logt|^AN|^SE') {
    data %>%
      select(matches(inclusion)) %>%
      select(!matches(exclusion)) %>%
      pivot_longer(
        cols = matches(covariable),
        names_to = 'covariable',
        values_to = 'valor'
      ) %>%
      mutate(
        par = paste0(parametro, '_', covariable),
        par = fct_reorder(par, desc(parametro)),
        iteracion = as.integer(iteracion)
      )
  }

#-------------------------------------------------------------------------------#
#' Creación de gráfico de Tilas de seguimiento de trayectoria
#'
#' @param data (data.frame) tiene el seguimiento de las trayectorias de 
#' evaluación de algoritmos automáticos
#'
#' @return (ggplot) gráfico de Tilas
#' @export
#'
#' @examples
#' graficoTilas(dataFrame)
#' 
graficoTilas <- function(data) {
  data %>% 
    ggplot(aes(x = iteracion, y = par, fill = ifelse(valor, 'r', 'b'))) +
    geom_tile(col = 'gray50', size = 0.1) +
    xlab("Iteración") + ylab('Parámetro - Covariable') +
    scale_x_continuous(position = 'top') +
    scale_fill_manual(values = c('r' = 'red', 'b' = 'gray99')) +
    theme(axis.text.x = element_text(angle = 90),
          axis.text.y = element_text(size = 6, hjust = 0), 
          axis.title.y = element_blank())
}

#-------------------------------------------------------------------------------#
#' Anidación y extracción de parámetros para cada iteración
#'
#' @param data 
#' @param corrida 
#' @param string 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
trazaOFV <- function(data, corrida, string = '^(it|OF|BIC|opt)') {
  evalParesCov(data) %>% 
    filter(valor == 1) %>% 
    group_by(across(matches(string))) %>% 
    nest() %>% 
    mutate(vals = map_chr(data, ~pull(.x, par) %>% paste0(collapse = '-'))) %>% 
    select(-data) %>% 
    add_column(corrida = corrida)
}


#-------------------------------------------------------------------------------#
#' Gráfico de convergencia tipo ggplot
#'
#' @param data 
#' @param xval 
#'
#' @return
#' @export
#'
#' @examples
#' 
graficoTrayecIndicador <- function(data, xval, xlim, ylim, crit = 965) {
  xval_quo <- rlang::ensym(xval)
  
  data %>%
    ggplot(aes(x = iteracion, y = !!xval_quo, col = indic)) +
    geom_point(aes(shape = optimo)) + geom_line(size = 0.5) + 
    geom_text_repel(aes(label = ifelse(data[xval] <= crit, iteracion, '')),
                    box.padding = 0.5, force = 1.2,
                    segment.color = 'black', segment.size = 0.1,
                    xlim = xlim, ylim = ylim, show.legend = FALSE) +
    facet_wrap(. ~ model, ncol = 3, scales = 'free_x') + 
    xlab('Iteración') + ylab(xval) +
    scale_shape_manual(values = c(8, 16), breaks = c(T, F), name = 'Óptimo', labels = c('Sí', 'No')) +
    scale_color_manual(values = c('LRT' = 'red', 'BIC' = 'blue'), name = 'Indicador') +
    theme(legend.position = 'bottom', legend.direction = 'horizontal')
}


#-------------------------------------------------------------------------------#
#' Gráfico de convergencia tipo Plotly
#'
#' @param data 
#' @param xval 
#'
#' @return
#' @export
#'
#' @examples
#' 
plotlyTrayectoriaIndicador <- function(data, xval) {
  xval_q <- parse(text = xval)
  
  modelSpecs <- function(df, term) {
    plot_ly(data = df, x = ~iteracion, y = term, 
            color = ~indic, colors = "Set1",
            text = ~paste('Modelo: ', vals)) %>% 
      add_lines(type = 'scatter', mode = 'lines') %>% 
      add_trace(type = 'scatter', mode = 'markers', 
                marker = list(size = 6), symbol = ~optimo, symbols = c('o', 'x'))    
  }
  
  
  fig1 <- data %>% filter(model == 'covSAMBA') %>% 
    modelSpecs(term = ~eval(xval_q))
  
  
  fig2 <- data %>% filter(model == 'COSSAC') %>% 
    modelSpecs(term = ~eval(xval_q))
  
  fig3 <- data %>% filter(model == 'SCM') %>% 
    modelSpecs(term = ~eval(xval_q))
  
  fig <- plotly::subplot(fig1, fig2, fig3, nrows = 1, shareY = TRUE, 
                         titleX = TRUE, titleY = TRUE)
  
  return(fig)
}
