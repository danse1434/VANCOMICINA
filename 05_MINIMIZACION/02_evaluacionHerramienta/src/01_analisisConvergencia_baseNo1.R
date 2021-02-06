##------------------------------------------------------------------------------#
## Nombre del Script: Análisis de datos de convergencia SAEM --------------------
##  
## Propósito del Script: se realiza un análisis de convergencia del algoritmo 
## SAEM con diversos valores iniciales de los parámetros del modelo. Esto para el 
## modelo final con covariables Cl-SCR.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 13-06-2020 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
require(rlang)
require(glue)
require(ggrepel)
require(scales)
require(patchwork)
require(tidyverse)

# Vector con orden de parámetros
level_par <- c('Cl_pop', 'V1_pop', 'Q_pop', 'V2_pop', 
               'omega_Cl', 'omega_V1', 'omega_V2', 'omega_Q', 'a1', 'a2', 'corr_V2_V1')

# Selección directorio auxiliar
aux_dir <- file.path(getwd(), 'Base_corr2', 'Assessment')

#-------------------------------------------------------------------------------#
# 1 Resumen Parámetros de Diseño Factorial --------------------------------------
#-------------------------------------------------------------------------------#
# Lista vacía *d_fct_ls*
d_fct_ls <- vector('list', 60)
# Lista vacía con Assessment
m_fct_ls <- vector('list', 60)

# Lectura de datos de parámetros estimados
for (i in 1:60) {
  subdir <- file.path(aux_dir, paste0('Run', i))
  # Asignación a vector
  d_fct_ls[[i]] <-
    read_csv(file.path(subdir, 'populationParameters.txt'),
             col_types = cols())
  m_fct_ls[[i]] <- 
    read_csv(file.path(subdir, 'Assessment', 'assessment.txt'),
             col_types = cols())
}
# Conversión en data.frame unificado
d_fct_ls1 <- d_fct_ls %>%
  map_dfr( ~ as_tibble(.x), .id = 'ID') %>%
  mutate(ID = as.double(ID),
         parameter = factor(parameter, levels = level_par))

# Ajuste configuración de gráfico
theme_set(theme_bw())

# Gráfico de resumen de parámetros por corrida
G1 <- d_fct_ls1 %>% 
  ggplot(aes(x = ID, y = value, col = ID)) +
  geom_point() + 
  geom_errorbar(aes(ymin = value-se_lin, ymax = value+se_lin)) +
  facet_wrap(~parameter, scales = 'free_y') +
  scale_color_gradientn(colours = rainbow(7) ) +
  theme(legend.position = 'none', 
        axis.title = element_blank())

# Almacenamiento de gráficos
if (!file.exists(glue('{getwd()}/figures/01_indicadorParametros.png'))) {
  ggsave('01_indicadorParametros.pdf', G1, 'pdf', 'figures', 1, 7, 5)
  ggsave('01_indicadorParametros.png', G1, 'png', 'figures', 1, 7, 5, dpi = 720*2)
}

#-------------------------------------------------------------------------------#
# 2 Análisis ¨PCA -----------------------------------------------------
#-------------------------------------------------------------------------------#
pca_fct_ls1 <- 
  d_fct_ls1 %>%
  pivot_wider(id_cols = 'ID',
              names_from = 'parameter',
              values_from = 'value') %>%
  mutate(across(-ID, function(x) {(x - mean(x)) / sd(x)})) %>%
  select(-ID) %>% 
  prcomp(., center = FALSE) 

PCdf <- as_tibble(pca_fct_ls1$x) %>% 
  rownames_to_column('ID') 

# almacenamiento de PCdf
write_csv(PCdf, 'figures/02_PCA.csv')

PCAbiplot <- function(data, x = 'PC1', y = 'PC2', col1='red', col2='blue', 
                      threshold = -4) {
  xvar <- rlang::ensym(x)
  yvar <- rlang::ensym(y)
  
  data %>%
    ggplot(aes(!!xvar, !!yvar, col = ifelse(!!xvar < threshold, col1, col2))) +
    geom_point() +
    theme(legend.position = 'none') +
    scale_color_manual(values = c(col1, col2))
}

PCAbiplot_comp <- (
  PCAbiplot(PCdf, x = 'PC1', y = 'PC2') +
    PCAbiplot(PCdf, x = 'PC1', y = 'PC3') +
    PCAbiplot(PCdf, x = 'PC2', y = 'PC3')
) + plot_layout(ncol = 2) &
  coord_cartesian(c(-10,+10), c(-5,+5))

if (!file.exists(glue('{getwd()}/figures/03_resultadosPCA.png'))) {
  ggsave('03_resultadosPCA.pdf', PCAbiplot_comp, 'pdf', 'figures', 1, 7, 5)
  ggsave('03_resultadosPCA.png', PCAbiplot_comp, 'png', 'figures', 1, 7, 5, dpi = 720*2)
}


pca_fct_ls1 %>% `$`('sdev') %>% 
  sapply(function(x){x/sum(x)})

pdf(file.path('figures', '04_biplotPCA.pdf'), 8, 4)
  par(mar = c(3, 4.5, 3, 4.5), mfrow = c(1, 2))
  
  biplot(pca_fct_ls1, cex = rep(0.5, 2))
  biplot(pca_fct_ls1, choices = c(1,3), cex = rep(0.5, 2))
dev.off()
