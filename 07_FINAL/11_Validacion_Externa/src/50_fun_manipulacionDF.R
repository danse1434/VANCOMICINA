dataOBS <- res$y %>%
  mutate(
    id = as.numeric(as.character(id)),
    poblacion = (id - 1) %/% nIndiv,
    individuo = id - (poblacion * nIndiv),
    poblacion = poblacion + 1
  )

# Perfil plasmático de gOBS

gOBS <- ggplot(dataOBS) +
  geom_point(aes(x = time, y = y, col = individuo), 
             alpha = 0.5) +
  theme(legend.position = 'none') + 
  xlab('Tiempo tras dosis (h)') + 
  ylab('Concentración plasmática (mg/L)')

tablaParam <- res$parameter %>%
  summarise(across(!contains('id'), list(
    mn = mean,
    sd = sd,
    mi = min,
    q1 = function(x) quantile(x, probs = 0.25),
    q2 = function(x) quantile(x, probs = 0.50),
    q3 = function(x) quantile(x, probs = 0.75),
    ma = max
  ))) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(
    col = name,
    into = c('parameter', 'statistic'),
    sep = '\\_(?=\\w(\\d|\\w)$)'
  ) %>% 
  pivot_wider(names_from = parameter,
              values_from = value)

# Tabla de resumen de parámetros
gt(tablaParam) %>%
  tab_header(title = 'Parámetros muestreados') %>%
  fmt_number(columns = c(4), decimals = 0) %>% 
  fmt_number(columns = c(2,3,6,7, 9:11, 13, 14:19), decimals = 2) %>%
  fmt_number(columns = c(12), decimals = 3) %>% 
  tab_options(table.font.size = '12px') %>% 
  gtsave(., 'plotOBS.html', file.path(wdir) %>% normalizePath())


ggsave(file.path(wdir, 'plot_OBS.pdf'), gOBS, 'pdf', width = 8, height = 6)