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
               'omega_Cl', 'omega_V1', 'omega_V2', 'omega_Q', 'b')

# Selección directorio auxiliar
aux_dir <- file.path(getwd(), 'evaluacion')

#-------------------------------------------------------------------------------#
# 1 Resumen Parámetros de Diseño Factorial --------------------------------------
#-------------------------------------------------------------------------------#
# Lista vacía *d_fct_ls*
d_fct_ls <- vector('list', 81)
# Lectura de datos de parámetros estimados
for (i in 1:81) {
  subdir <- file.path(aux_dir, paste0('evaluacion', i), 'M2CPTM_nobs_1_prop')
  # Asignación a vector
  d_fct_ls[[i]] <-
    read_csv(file.path(subdir, 'populationParameters.txt'),
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
  geom_errorbar(aes(ymin = value-se_sa, ymax = value+se_sa)) +
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
# 2 Convergencia Diseño Factorial -----------------------------------------------
#-------------------------------------------------------------------------------#
# Lectura de datos de convergencia SAEM
s_fct_ls <- vector('list', 81) # vector en blanco
# 
for (i in 1:81) {
  subdir <- glue('{aux_dir}/evaluacion{i}/M2CPTM_nobs_1_prop')
  
  s_fct_ls[[i]] <-
    read_csv(glue('{subdir}/ChartsData/Saem/CvParam.txt'),
             col_types = cols())
}
# Conversión en data.frame unificado
s_fct_ls1 <- s_fct_ls %>%
  map_dfr(~ as_tibble(.x), .id = 'I') %>%
  rename(LL = convergenceIndicator) %>% 
  pivot_longer(
    cols = matches('\\_pop|omega\\_|^a$|^b$|^LL$'),
    names_to = 'parameter',
    values_to = 'value'
  ) %>% 
  mutate(I = as.integer(I), 
         parameter = factor(parameter, levels = c(level_par, 'LL')))

# Gráfico de convergencia SAEM por corrida
G2 <- s_fct_ls1[s_fct_ls1$iteration%%10 == 0,] %>%
  ggplot(aes(x = iteration, y = value, group=I, col = I)) +
  geom_line(alpha = 0.8) + 
  facet_wrap(~parameter, scales = 'free_y') +
  scale_color_gradientn(colours = rainbow(7) ) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(legend.position = 'none', 
        axis.title = element_blank(), axis.text.x = element_text(angle = -45))

# Almacenamiento de gráficos
if (!file.exists(glue('{getwd()}/figures/02_conv_indic.png'))) {
  ggsave('02_conv_indic.pdf', G2, 'pdf', 'figures', 1, 7, 5)
  ggsave('02_conv_indic.png', G2, 'png', 'figures', 1, 7, 5, dpi = 720*2)
}

#-------------------------------------------------------------------------------#
# 3 Análisis ¨PCA -----------------------------------------------------
#-------------------------------------------------------------------------------#
pca_fct_ls1 <- d_fct_ls1 %>%
  pivot_wider(id_cols = 'ID',
              names_from = 'parameter',
              values_from = 'value') %>%
  mutate(across(-ID, function(x) {(x - mean(x)) / sd(x)})) %>%
  select(-ID) %>% 
  prcomp(., center = FALSE) 


labelMat <- read_csv('results/01_arregloOrigen.csv')  %>%
  rownames_to_column('ID') %>%
  mutate(across(-ID, ~ case_when(
    .x == min(.x) ~ 'L',
    .x == max(.x) ~ 'H',
    TRUE ~ 'M'
  ))) %>% 
  rowwise() %>% 
  mutate(label = paste(Cl_pop, V1_pop, Q_pop, V2_pop, sep='-'))


PCdf <- as_tibble(pca_fct_ls1$x) %>% 
  rownames_to_column('ID') %>% 
  left_join(labelMat[c('ID', 'label')], by = 'ID')
# almacenamiento de PCdf
write_csv(PCdf, 'results/02_PCA.csv')

PCAbiplot <- function(data, x = 'PC1', y = 'PC2', col1='red', col2='blue', 
                      threshold = -4) {
  xvar <- rlang::ensym(x)
  yvar <- rlang::ensym(y)
  
  data %>%
    ggplot(aes(!!xvar, !!yvar, col = ifelse(!!xvar < threshold, col1, col2))) +
    geom_point() +
    geom_text_repel(data = filter(PCdf,!!xvar < threshold),
                    aes(label = label),
                    xlim = c(-6, NA)) +
    theme(legend.position = 'none') +
    scale_color_manual(values = c(col1, col2))
}

PCAbiplot_comp <- PCAbiplot(PCdf, x = 'PC1', y = 'PC2') +
  PCAbiplot(PCdf, x = 'PC1', y = 'PC3') +
  PCAbiplot(PCdf, x = 'PC2', y = 'PC3') +
  plot_layout(ncol = 2)


if (!file.exists(glue('{getwd()}/figures/03_resultadosPCA.png'))) {
  ggsave('03_resultadosPCA.pdf', PCAbiplot_comp, 'pdf', 'figures', 1, 7, 5)
  ggsave('03_resultadosPCA.png', PCAbiplot_comp, 'png', 'figures', 1, 7, 5, dpi = 720*2)
}
