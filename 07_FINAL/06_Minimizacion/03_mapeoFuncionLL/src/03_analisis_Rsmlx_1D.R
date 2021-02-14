require(Rsmlx)
library(lixoftConnectors)
initializeLixoftConnectors(
  software = "monolix", 
  path = file.path('C:', 'ProgramData', 'Lixoft', 'MonolixSuite2020R1')
)

require(openxlsx)
require(readxl)
require(tidyverse)

# Directorio externo donde se ubica el modelo base 1
dirModeloBase <- file.path(".")

if (!file.exists('./results/01_listaResultados_1D.xlsx')) {
  # Tiempo de corrida ~ 30 minutos
  r <- confintmlx(
    file.path(dirModeloBase, 'modeloFinal.mlxtran'),
    parameters = "all",
    method = "proflike",
    level = 0.90,
    linearization = FALSE,
    settings = list('max.iter'=50)
  ) 
  
  # Almacenar resultados de Rsmlx
  
  rList <- list()
  
  rList <- append(rList, list(
    'confint' = r$confint %>% rownames_to_column('name_f'),
    'proflike'= r$proflike %>% map_dfr(~ as_tibble(.x), .id = 'Run')
  ))
  
  hs <- createStyle(
    textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
    fontName = "Arial Narrow", fgFill = "#4F80BD"
  )
  
  write.xlsx(rList, './results/01_listaResultados_1D.xlsx', headerStyle=hs)
}

r <- list()

r <- append(r, list(
  'confint'  = read_excel("results/01_listaResultados_1D.xlsx", sheet = 'confint'),
  'proflike' = read_excel("results/01_listaResultados_1D.xlsx", sheet = 'proflike')
))

par_order <- c('Cl_pop', 'beta_Cl_logtWTKG', 'beta_Cl_tCLCRMLMIN', 
               'V1_pop', 'Q_pop', 'V2_pop', 
               'omega_Cl', 'corr_V2_V1', 'omega_V1', 'omega_V2', 'omega_Q', 
               'a1', 'a2')

#' Generación de perfil de verosimilitud
#'
#' @param df_ls lista con componentes de verosimilitud
#' @param par_order órden de los parámetros en el gráfico compuesto
#' @param ll_Line Nombre de componente con perfil de verosimilitud -2LL
#' @param ll_Card Nombre de componente con puntos cardinales del perfil
#'
#' @return
#' @export
#' lista con tablas y gráficos
#'
#' @examples
#' likeLiHoodPlot(r, par_order)
#' 
likeLiHoodPlot <- function(df_ls, par_order, ll_Line='proflike', ll_Card='confint') {
  ll_DF <- df_ls %>% 
    pluck(ll_Line) %>% 
    mutate(name_f = factor(name, par_order))
  
  card_DF <- df_ls %>% 
    pluck(ll_Card) %>%
    mutate(name_f = factor(name_f, par_order))
  
  card_DF1 <- group_by(ll_DF, name, name_f) %>%
    nest() %>% 
    mutate(
      fun = map(data, ~ approxfun(.x$param, .x$OFV)),
      v_lwr = map_dbl(name, ~ card_DF[card_DF$name_f==.x, ]$lower),
      v_upr = map_dbl(name, ~ card_DF[card_DF$name_f==.x, ]$upper),
      l_lwr = map_dbl(fun, ~.x(v_lwr)),
      l_upr = map_dbl(fun, ~.x(v_upr))
    )
  
  g1 <- ll_DF %>% 
    ggplot(aes(x = param, y=OFV)) +
    geom_point(col='blue4') +
    geom_line(col='blue4') +
    facet_wrap(name_f ~ ., ncol = 3, scales = 'free', shrink = T)+ 
    geom_vline(
      data = card_DF ,
      aes(xintercept = estimate),
      lty = 'dashed'
    ) + 
    geom_segment(data=card_DF1, aes(x=v_lwr, xend=v_lwr, y=-Inf, yend=l_lwr), lty='dashed') +
    geom_segment(data=card_DF1, aes(x=v_upr, xend=v_upr, y=-Inf, yend=l_upr), lty='dashed') +
    ylab('-2LL') + theme_bw() +
    theme(axis.title.x = element_blank())
  
  
  return(list('g'=g1, 'll_DF'=ll_DF, 'CD'=card_DF1, 'card_DF'=card_DF))
}

# Se eliminan algunas iteraciones de corr_V2_V1 ya que este parámetro 
# tiene límite superior en 1.

r1 <- r
r1$proflike <- r$proflike %>%
  filter(!(name == 'corr_V2_V1') | !(param > 1))

g <- likeLiHoodPlot(r1, par_order)

ggsave('02_perfiles_proflike.pdf', g$g, 'pdf', 'figures', 1, 8, 6)
