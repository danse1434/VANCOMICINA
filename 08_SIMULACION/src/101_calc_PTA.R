
require(data.table)
require(progress)
require(scales)

source(file.path('src', '069_paquetesSimulacion.R'), encoding = 'UTF-8')
source(file.path('src', '081_fun_CalculoIndicadores.R'), encoding = 'UTF-8')


outexposure <- '001_exp_desconocido'

MIC_vec_0 = c(1 * (2 ^ (seq(-8, 5, 1))))
MIC_vec_1 = c(1 * (2 ^ (seq(-8, 5, length = 1e2))))

dataLS <- list()

for (i in 1:21) {
  data0 <- fread(file.path('results', paste0(outexposure, i, '.csv')))
  data1 <- resPTA_Tabla(data0, MIC_vec_1, resPTA1_AUC, crit = 400, g = i)
  
  dataLS[[i]] <- data1
}


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


#-------------------------------------------------------------------------------#
# Graficos -----------------------------------------------------
#-------------------------------------------------------------------------------#
admDF <- read_csv(file.path('data', 'adm_list.csv'))


dfLS <- map_df(dataLS, ~ .x) %>%
  mutate(
    DD = map_dbl(g, ~ admDF[admDF$ID == .x, ]$DD),
    II = map_dbl(g, ~ admDF[admDF$ID == .x, ]$ii),
    Tinf = map_dbl(g, ~ admDF[admDF$ID == .x, ]$tinf)
  )

graficoPTA_AUC(dfLS, MIC_vec_1, MIC_vec_0, color = DD, format = 'P') + 
  facet_grid(DD ~ II)

