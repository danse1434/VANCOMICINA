#' Gráfico PTA
#'
#' @param data 
#' @param MIC_vec 
#' @param MIC_eje 
#' @param x 
#' @param y 
#' @param group 
#' @param color 
#' @param format 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
graficoPTA_AUC <- function(data, MIC_vec, MIC_eje, 
                           x = 'MIC', y = 'mn', group = 'g', color = 'g', format = 'N') {
  q_x <- ensym(x)
  q_y <- ensym(y)
  q_group <- ensym(group)
  q_color <- ensym(color)
  
  log2r <- function(x) {
    round(log2(x), 1)
  }
  
  if (format == 'N') {
    formatFun <- function(x) 
    {format(x, drop0trailing = T, digits = 4, nsmall = 0, trim = T, 
            scientific = F)}
  } else if (format == 'P') {
    formatFun <- function(x){
      log2(x)
    }
  } else if (format == 'PN') {
    formatFun <- function(x){
      parse(text = bquote(2^.(log2(x))))
    }
  }
  
  data %>% 
    filter(log2r(MIC) %in% log2r(MIC_eje)) %>% 
    ggplot(aes(x = !!q_x, y = !!q_y, group = !!q_group, color = !!q_color)) +
    geom_line(data = data) + 
    geom_point() + 
    theme_bw() + 
    scale_x_continuous(trans = log2_trans(), 
                       breaks = MIC_eje,
                       guide = guide_axis(n.dodge = 2),
                       labels = map_dbl(MIC_eje, formatFun)) +
    xlab('MIC (mg/L)') + ylab('AUC > MIC')
}
