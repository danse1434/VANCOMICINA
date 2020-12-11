require(tidyverse)
require(gt)

df1 <- read_csv(file.path('reports', '100_modeltwoCptmBase_Results.txt'))
df2 <- read_csv(file.path('reports', '101_modeltwoCptmDiag_Results.txt'))
df3 <- read_csv(file.path('reports', '102_modeltwoCptmDiagProp_Results.txt'))
df4 <- read_csv(file.path('reports', '103_modeltwoCptmDiagPropCov_Results.txt'))

parameters <- unique(c(df1$P, df2$P, df3$P, df4$P)) %>% 
  keep(~ str_detect(.x, 'CLHat|QHat|V1Hat|V2Hat|omega|rho|^b\\d{0,}$|sigma')) %>% 
  discard(~ str_detect(.x, 'rho\\[1,1\\]|rho\\[2,2\\]|rho\\[3,3\\]|rho\\[4,4\\]'))%>% 
  discard(~ str_detect(.x, 'rho\\[2,1\\]|rho\\[3,1\\]|rho\\[4,1\\]|rho\\[3,2\\]|rho\\[4,2\\]|rho\\[4,3\\]'))


df1 %>% 
  filter(P %in% parameters) %>% 
  mutate(P = str_replace_all(P, 'Hat', '<sub>Hat</sub>'))

crearTabla <- function(x, subtitle) {
  filter(x, P %in% parameters) %>% 
    mutate(
      P = str_replace_all(P, 'Hat', '<sub>Hat</sub>'),
      P = str_replace_all(P, '\\[', '<sub>['),
      P = str_replace_all(P, '\\]', ']</sub>'),
      ) %>% 
    gt() %>% 
    tab_spanner(label = 'Percentiles', 
                columns = c("2.5%", "25%", "50%", "75%", "97.5%")) %>% 
    tab_header(
      title = md('**Resumen Modelo Bayesiano Stan**'),
      subtitle = md(subtitle)
    ) %>% 
    fmt_number(columns = 2:11, decimals = 3) %>%
    fmt_markdown(columns = vars(P)) %>% 
    cols_label(
      P = 'Parámetros',
      mean = 'Media',
      se_mean = 'SE',
      sd = 'RSD',
      n_eff = html('N<sub>eff</sub>'),
      Rhat = html('R<sub>hat</sub>')
    ) %>% 
    tab_options(
      column_labels.font.size = "medium",column_labels.font.weight = 'bold',
      table.font.size = "medium",
      data_row.padding = px(3)
    )
}

crearTabla(df1, 'Matriz BSV Completa - Error Residual Aditivo') %>% 
  gtsave('100_modeltwoCptmBase_Results.html', file.path(getwd(), 'reports'))

crearTabla(df2, 'Matriz BSV Diagonal - Error Residual Aditivo') %>% 
  gtsave('101_modeltwoCptmDiag_Results.html', file.path(getwd(), 'reports'))

crearTabla(df3, 'Matriz BSV Diagonal - Error Residual Proporcional') %>% 
  gtsave('102_modeltwoCptmDiagProp_Results.html', file.path(getwd(), 'reports'))

crearTabla(df4, 'Matriz BSV Diagonal - Error Residual Proporcional - Métodos Analíticos') %>% 
  gtsave('103_modeltwoCptmDiagPropCov_Results.html', file.path(getwd(), 'reports'))

