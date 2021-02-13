require(ggplot2)
require(ggrepel)
require(ggraph)
require(tidygraph)
require(dplyr)

#-------------------------------------------------------------------------------#
#' Calcular distancia respecto a valores medios para vectores bidimensionales
#'
#' @param x posiciones en x
#' @param y posiciones en y
#' @param p norma de distanca (p=2 distancia euclídea)
#'
#' @return
#' @export
#' vector con distancias como números
#'
#' @examples
#' calc_distancia(PCdf[,'PC1'], PCdf[,'PC2'], 2)
#' 
calc_distancia <- function(x, y, p=2) {
  if (p == 2) {
    d = sqrt((x - mean(x)) ^ 2 + (y - mean(y)) ^ 2)
  } else {
    d = ((x - mean(x)) ^ p + (y - mean(y)) ^ p) ^ (1 / p)
  }
  
  return(d)
}

#-------------------------------------------------------------------------------#
#' Creación de gráfico PCA de tipo biplot
#'
#' @param data Tabla con datos transformados en dimensiones de PC
#' @param x Componente a graficar en el eje X
#' @param y Componente a graficar en el eje Y
#' @param col1 Color de puntos típicos
#' @param col2 Color de puntos atípicos
#' @param threshold Umbral para considerar a un dato atípico
#' @param xlab etiqueta de eje X
#' @param ylab etiqueta de eje Y
#'
#' @return
#' @export
#' gráfico de tipo biplot
#'
#' @examples
#' PCAbiplot(PCdf, x='PC1', y='PC2', xlab='PC1 (56.8%)', ylab='PC2 (32.1%)')
#' 
PCAbiplot <- function(data, x = 'PC1', y = 'PC2', col1='blue', col2='red', 
                      threshold = 3, xlab='PC1', ylab='PC2') {
  xvar <- rlang::ensym(x)
  yvar <- rlang::ensym(y)
  
  
  d <- mapply(function(x,y) calc_distancia(x,y), data[,x], data[,y])
  u <- d - mean(d) > threshold * sd(d)
  
  g <- data %>%
    add_column(u = as.logical(u)) %>% 
    ggplot(aes(!!xvar, !!yvar, col = u)) +
    geom_point() +
    geom_text_repel(
      data = filter(data, u), aes(x=!!xvar, y=!!yvar, label = label),
            xlim = c(-6, NA), inherit.aes = FALSE) +
    geom_hline(yintercept=0, lty='dashed') + geom_vline(xintercept=0, lty='dashed') +
    theme(legend.position = 'none') +
    xlab(xlab) + ylab(ylab) +
    scale_color_manual(values = c(col1, col2))
  
  return(g)
}


#-------------------------------------------------------------------------------#
#' Crear gráfico de comparaciones de Tukey
#'
#' @param aov Objeto de tipo ANOVA
#' @param type Tipo de gráfico comparativo: graph o box
#' @param offset
#'
#' @return
#' @export
#' lista con datos de Comparaciones de Tukey en formato y ggplot
#'
#' @examples
#' plotTukeyComparisons(aov, 'box')
#' 
plotTukeyComparisons <- function(aov, type='graph', offset=1.2, edge_label_size=2.5){
  aov_1_THSD <- aov %>% 
    TukeyHSD() %>% 
    broom::tidy() 
  
  if (type == 'graph') {
    g <- aov_1_THSD %>% 
      separate(contrast, c('from', 'to'), '\\-') %>% 
      as_tbl_graph() %>% 
      ggraph(layout='kk') +
      geom_edge_link2(
        aes(
          label = paste0(round(estimate, 2), ' [', round(conf.low,2), ',', round(conf.high,2), ']'), 
          edge_colour = adj.p.value<0.05
        ),
        arrow = arrow(length = unit(3, 'mm'), type='closed'),
        end_cap = circle(3, 'mm'),
        angle_calc = 'along', 
        label_colour = 'black', label_dodge = unit(-2,'mm'), label_size=edge_label_size
      ) + 
      geom_node_label(
        aes(label = name), size=4
      ) +
      facet_edges(~term) + 
      scale_edge_color_manual(values = c('black', 'red')) +
      theme(legend.position = 'none'
      )
    
    # Añadir un poco de offset a los ejes
    axisCoordinates <- c(
      'xAxis' = c(min(g$data$x), max(g$data$x)),
      'yAxis' = c(min(g$data$y), max(g$data$y))
      )

    g <- g + coord_cartesian(
      xlim = c(axisCoordinates[['xAxis1']]*offset, axisCoordinates[['xAxis2']]*offset),
      ylim = c(axisCoordinates[['yAxis1']]*offset, axisCoordinates[['yAxis2']]*offset)
      )
    
  } else if (type == 'box') {
    g <- aov_1_THSD %>% 
      ggplot(aes(y=contrast)) +
      geom_point(aes(x=estimate)) +
      geom_errorbar(aes(xmin=conf.low, xmax=conf.high)) +
      geom_vline(xintercept = 0.00, lty = 'dashed', color='blue') + 
      xlab(expression(Delta)) + ylab('Contraste') + 
      facet_wrap(. ~ term, ncol = 2)
  }
  
  
  return(list('Tukey' = aov_1_THSD, 'Plot'=g))
}
