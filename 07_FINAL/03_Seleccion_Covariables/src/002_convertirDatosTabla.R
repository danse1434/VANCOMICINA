##------------------------------------------------------------------------------#
## Nombre del Script: Revisión de trayectorias de algoritmos automáticos --------
##  
## Propósito del Script: extracción de trayectorias de búsqueda de algoritms 
## automáticos.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 11-02-2021
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(ggplot2)
require(gridExtra)
require(gganimate)
require(patchwork)
require(ggrepel)
require(rlang)
require(plotly)

source(file.path('src', '020_funcionExtraccion.R'), encoding = 'UTF-8')
source(file.path('src', '021_funcionAnalisis.R'), encoding = 'UTF-8')
#-------------------------------------------------------------------------------#
# 1. Introducción -----------------------------------------------------
#-------------------------------------------------------------------------------#

tablaOrig <- file.path('ModelSelection', 'modelBuilding.txt')

df_covSAMBA_LRT <- conversionTabular(file.path('01_covSAMBA_LRT', tablaOrig))
df_covSAMBA_BIC <- conversionTabular(file.path('02_covSAMBA_BIC', tablaOrig))
df_COSSAC_LRT   <- conversionTabular(file.path('03_COSSAC_LRT', tablaOrig))
df_COSSAC_BIC   <- conversionTabular(file.path('04_COSSAC_BIC', tablaOrig))
df_SCM_BIC      <- conversionTabular(file.path('05_SCM_LRT', tablaOrig))

#-------------------------------------------------------------------------------#
# 2. Gráfico de Tilas ---------------------------------------------
#-------------------------------------------------------------------------------#
# Cuando no se incluye el par no se muestra
criterio <- c("1" = 'red', "0" = 'gray99')

# Configurar el tema de gráficos sin leyendas
theme_set(theme_bw() + 
            theme(panel.border = element_rect(fill = NA, colour = 'black'),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  legend.position = "none" ))

g_covSAMBA_LRT <- evalParesCov(df_covSAMBA_LRT) %>% graficoTilas()
g_covSAMBA_BIC <- evalParesCov(df_covSAMBA_BIC) %>% graficoTilas()
g_COSSAC_LRT   <- evalParesCov(df_COSSAC_LRT) %>% graficoTilas()
g_COSSAC_BIC   <- evalParesCov(df_COSSAC_BIC) %>% graficoTilas()
g_SCM_BIC      <- evalParesCov(df_SCM_BIC) %>% graficoTilas()

# 2.1. Gráfico con todos los algoritmos
((
  g_covSAMBA_LRT + g_covSAMBA_BIC + g_COSSAC_LRT + g_COSSAC_BIC + g_SCM_BIC
) +
    plot_layout(nrow = 1, widths = unit(c(1, 1, 1, 1, 5), rep('null', 5)))
) %>%
  ggsave('001_trayectoriaTilas_todos.pdf', ., 'pdf', 'figures', 1, 16, 4)

# 2.2. Gráfico con algoritmos con seguimiento por LRT
((
  g_covSAMBA_LRT + 
  g_COSSAC_LRT + theme(axis.text.y = element_blank()) + 
  g_SCM_BIC + theme(axis.text.y = element_blank())
) +
    plot_layout(nrow = 1, widths = unit(c(1, 1, 5), rep('null', 3))) + 
    plot_annotation(tag_levels = 'A')
) %>%
  ggsave('002_trayectoriaTilas_LRT.pdf', ., 'pdf', 'figures', 1, 10, 4)


# 2.3. Gráfico COSSAC_LRT vs SCM
((
  g_COSSAC_LRT + g_SCM_BIC + theme(axis.text.y = element_blank())
) +
    plot_layout(nrow = 1, widths = unit(c(1, 5), rep('null', 2))) + 
    plot_annotation(tag_levels = 'A')
) %>%
  ggsave('003_trayectoriaTilas_COSSAC_SCM.pdf', ., 'pdf', 'figures', 1, 8, 4)

# Almacenar gráficos en formato ggplotly
ggplotly(g_covSAMBA_BIC) %>%
{htmlwidgets::saveWidget(as_widget(.),
    file.path('figures', '004_g_covSAMBA_BIC.html') %>% normalizePath()
)}
ggplotly(g_covSAMBA_LRT) %>%
{
  htmlwidgets::saveWidget(as_widget(.),
    file.path('figures', '005_g_covSAMBA_LRT.html') %>% normalizePath()
  )
}
ggplotly(g_COSSAC_BIC) %>%
{
  htmlwidgets::saveWidget(as_widget(.),
    file.path('figures', '006_g_COSSAC_BIC.html') %>% normalizePath()
  )
}
ggplotly(g_COSSAC_LRT) %>%
{
  htmlwidgets::saveWidget(as_widget(.),
    file.path('figures', '007_g_COSSAC_LRT.html') %>% normalizePath()
  )
}
ggplotly(g_SCM_BIC) %>%
{
  htmlwidgets::saveWidget(as_widget(.),
    file.path('figures', '008_g_SCM_BIC.html') %>% normalizePath()
  )
}
#-------------------------------------------------------------------------------#
# 3. Seguimiento convergencia -----------------------------------------------------
#-------------------------------------------------------------------------------#
ls1 <- data.frame(
  'model' = c('covSAMBA_LRT', 'covSAMBA_BIC', 
              'COSSAC_LRT',  'COSSAC_BIC', 'SCM_BIC')
  )

ls2 <- ls1 %>%
  as_tibble() %>%
  mutate(model1 = map(model, ~ paste0('df_', .x)),
         model2 = map(model1, ~ trazaOFV(eval(parse_expr(
           .x
         )), .x))) %>%
  select(-model1) %>%
  unnest(model2) %>%
  separate('model', c('model', 'indic'), sep = '\\_') %>%
  mutate(model = factor(model, levels = c('covSAMBA', 'COSSAC', 'SCM')))

gIndic_OFV <- graficoTrayecIndicador(ls2, 'OFV', 
                                     crit = ls2$OFV %>% quantile(., prob = c(15/length(.))), 
                                     xlim = c(5, NA), ylim = c(2910, NA) )

gIndic_BIC <- graficoTrayecIndicador(ls2, 'BICc', 
                                     crit = ls2$BICc %>% quantile(., prob = c(15/length(.))),
                                     xlim = c(5, NA), ylim = c(975, NA))

ggsave('009_trayectoriaAlgoritmosOFV.pdf', gIndic_OFV, 'pdf', 
       'figures', 1, 10, 4)

ggsave('010_trayectoriaAlgoritmosBIC.pdf', gIndic_BIC, 'pdf', 
       'figures', 1, 10, 4)

# Creación de gráficos Plotly

pIndic_OFV <- plotlyTrayectoriaIndicador(ls2, 'OFV')
pIndic_BIC <- plotlyTrayectoriaIndicador(ls2, 'BICc')
  
htmlwidgets::saveWidget(
  as_widget(pIndic_OFV),
  file.path('figures', '011_trayectoriaAlgoritmosOFV.html') %>% normalizePath()
)
htmlwidgets::saveWidget(
  as_widget(pIndic_BIC),
  file.path('figures', '012_trayectoriaAlgoritmosBIC.html') %>% normalizePath()
)

# Creación de gráfico sólo COSSAC y SCM
gIndic_BIC_1 <- ls2 %>% 
  filter(model %in% c('COSSAC', 'SCM')) %>%
  graficoTrayecIndicador(., 'BICc', 
                         crit = ls2$BICc %>% quantile(., prob = c(15/length(.))),
                         xlim = c(5, NA), ylim = c(975, NA))

ggsave('013_trayectoriaAlgoritmosBIC_1.pdf', gIndic_BIC_1, 'pdf', 
       'figures', 1, 8, 4)
