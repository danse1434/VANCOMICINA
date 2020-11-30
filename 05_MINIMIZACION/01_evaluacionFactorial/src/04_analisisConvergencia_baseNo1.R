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
require(directlabels) # Rectas al final de gráficos

source('./src/10_funciones_PCA_R.R', encoding = 'UTF-8')

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
         parameter = factor(parameter, levels = level_par)) %>% 
  # Indicador de outlier
  group_by(parameter) %>% 
  mutate(
    outlier_1 = abs(value - mean(value)) > 3*sd(value), 
    outlier_2 = abs((value - median(value))/mad(value)) > 6, 
    ) %>% 
  ungroup()

# Etiquetas con correspondencia entre valor inicial de cada parámetro
labelMat <- read_csv('results/01_arregloOrigen.csv')  %>%
  rownames_to_column('ID') %>%
  mutate(across(-ID, ~ case_when(
    .x == min(.x) ~ 'L',
    .x == max(.x) ~ 'H',
    TRUE ~ 'M'
  ))) %>% 
  rowwise() %>% 
  mutate(label = paste(Cl_pop, V1_pop, Q_pop, V2_pop, sep='-'))

# Unir etiquetas a los ID
d_fct_ls1 <- d_fct_ls1 %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(labelMat[, c('ID', 'label')], by = 'ID')

# Ajuste configuración de gráfico
theme_set(theme_bw())

# Gráfico de resumen de parámetros por corrida
G1 <- d_fct_ls1 %>% 
  ggplot(aes(x = ID, y = value, col=outlier_2)) +
  geom_point() + 
  geom_errorbar(aes(ymin = value-se_sa, ymax = value+se_sa)) +
  facet_wrap(~parameter, scales = 'free_y') +
  scale_color_manual(values=c('gray20', 'red')) + 
  geom_label_repel(data = d_fct_ls1[d_fct_ls1$outlier_2==TRUE,],
                  aes(label=label), size=2, 
                  box.padding=0.1, label.padding=0.1) + 
  theme(legend.position = 'none', axis.title = element_blank())

# Almacenamiento de gráficos
if (!file.exists(glue('{getwd()}/figures/01_indicadorParametros.png'))) {
  ggsave('01_indicadorParametros.pdf', G1, 'pdf', 'figures', 1, 10, 7)
  ggsave('01_indicadorParametros.png', G1, 'png', 'figures', 1, 10, 7, dpi = 720*2)
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
         parameter = factor(parameter, levels = c(level_par, 'LL'))) %>% 
  # Determinación de individuos que tienen valores finales de convergencia atípicos
  group_by(parameter, I) %>% 
  mutate(last_value = last(value)) %>% 
  group_by(parameter) %>% 
  mutate(
    outlier_1 = abs(last_value - mean(last_value)) > 3*sd(last_value),
    outlier_2 = abs((last_value - median(last_value))/mad(last_value)) > 6,
  ) %>% 
  ungroup()

# Unir etiquetas a los ID
s_fct_ls1 <- s_fct_ls1 %>% 
  mutate(I = as.character(I)) %>% 
  left_join(labelMat[, c('ID', 'label')], by = c('I'='ID'))

# Gráfico de convergencia SAEM por corrida
G2 <- s_fct_ls1[s_fct_ls1$iteration%%10 == 0,] %>%
  ggplot(aes(x = iteration, y = value, group=I)) +
  geom_line(col=alpha('gray50', 0.4)) + 
  facet_wrap(~parameter, scales = 'free_y') +
  geom_point(
    data = s_fct_ls1[(s_fct_ls1$outlier_1==TRUE) & (abs(s_fct_ls1$value)>0.2),] %>% 
      group_by(I) %>% slice(which.max(iteration)), shape='x', color='red', size=3
  ) +
  geom_dl(
    data = s_fct_ls1[(s_fct_ls1$outlier_1==TRUE) & (abs(s_fct_ls1$value)>0.2),],
    aes(label = label), color='red',
    method = list(dl.trans(x = x + 0.13), "last.bumpup", cex = 0.35)) +
  
  coord_cartesian(xlim=c(0, 5e3)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(legend.position = 'none', 
        axis.title = element_blank(), axis.text.x = element_text(angle = -45))

# Almacenamiento de gráficos
if (!file.exists(glue('{getwd()}/figures/02_conv_indic.png'))) {
  ggsave('02_conv_indic.pdf', G2, 'pdf', 'figures', 1, 10, 7)
  ggsave('02_conv_indic.png', G2, 'png', 'figures', 1, 7, 5, dpi = 720*2)
}

#-------------------------------------------------------------------------------#
# 3 Análisis PCA -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Estandarización de datos por parámetro
pca_fct_ls1 <- d_fct_ls1 %>%
  pivot_wider(id_cols = 'ID',
              names_from = 'parameter',
              values_from = 'value') %>%
  mutate(across(-ID, function(x) {(x - mean(x)) / sd(x)})) %>%
  select(-ID)

# Almacenamiento de datos estandarizados
pca_fct_ls1 %>% write_csv('results/03_datosEstandarizados.csv')

# Cálculo de Componentes Principales 
pca_fct_ls1 <- pca_fct_ls1 %>% 
  prcomp(., center = FALSE) 

# Contribución a la variabilidad
pca_fct_ls1$sdev %>% 
  cumsum() %>% 
  sapply(., function(x) {1.0-(x/5.646853)}) %>% 
  sapply(., function(x){sprintf("%.3f", x, 'f')})

# 
PCdf <- as_tibble(pca_fct_ls1$x) %>% 
  rownames_to_column('ID') %>% 
  left_join(labelMat[c('ID', 'label')], by = 'ID')
# almacenamiento de PCdf
write_csv(PCdf, 'results/02_PCA.csv')



PCAbiplot_comp <- 
  PCAbiplot(PCdf, x='PC1', y='PC2', xlab='PC1 (56.8%)', ylab='PC2 (32.1%)') +
  PCAbiplot(PCdf, x='PC1', y='PC3', xlab='PC1 (56.8%)', ylab='PC3 (17.9%)') +
  PCAbiplot(PCdf, x='PC2', y='PC3', xlab='PC2 (32.1%)', ylab='PC3 (17.9%)') +
  plot_layout(ncol = 2)


if (!file.exists(glue('{getwd()}/figures/03_resultadosPCA.png'))) {
  ggsave('03_resultadosPCA.pdf', PCAbiplot_comp, 'pdf', 'figures', 1, 7, 5)
  ggsave('03_resultadosPCA.png', PCAbiplot_comp, 'png', 'figures', 1, 7, 5, dpi = 720*2)
}

#-------------------------------------------------------------------------------#
# 4. Análisis de Varianza (ANOVA) ----
#-------------------------------------------------------------------------------#

# > 4.1 Respuesta: distancias al centroide de distribución --------
# Crear una matriz con los componentes
compPCA_mat <- PCdf %>% 
  select(starts_with('PC')) %>% 
  as.matrix()

# Calcular distancias respecto a la media de cad dimensión
A <- apply(compPCA_mat, 2, function(x) abs(x - mean(x))) %>% 
  apply(., c(1,2), function(x) x^2) %>% 
  apply(., 1, function(x) sqrt(sum(x)))


aov_1 <- PCdf[,c('label')] %>% 
  add_column(d = A) %>% 
  separate(label, c('Cl', 'V1', 'Q', 'V2'), '\\-') %>% 
  aov(d ~ Cl+V1+Q+V2, .) 

# > 4.2 Respuesta: logaritmo de verosimilitud --------
aov_2 <- s_fct_ls1[s_fct_ls1$parameter=='LL',] %>% 
  distinct(I, last_value) %>% 
  left_join(d_fct_ls1[, c('ID', 'label')], by=c('I'='ID')) %>% 
  separate(label, c('Cl', 'V1', 'Q', 'V2'), '\\-') %>% 
  aov(last_value ~ Cl+V1+Q+V2, .)
  

g1b <- plotTukeyComparisons(aov_1,'box')$Plot + 
  labs(title = bquote('Variable distancias'~(italic(d)[i]~-~bar(italic(d))) ))
g2b <- plotTukeyComparisons(aov_2,'box')$Plot +
  labs(title=bquote('Variable'~-2*log(italic(L))))

gtb <- (g1b + g2b) +
  plot_annotation(
    title = 'Efecto de valores iniciales en resultados de convergencia',
    subtitle = 'Comparaciones múltiples de Tukey',
    theme = theme(plot.title = element_text(face = 'bold'))
  )

if (!file.exists(glue('{getwd()}/figures/06_resultados_Tukey_HSD_1.pdf'))) {
  ggsave('06_resultados_Tukey_HSD_1.pdf', gta, 'pdf', 'figures', width=8, height=5)
  ggsave('06_resultados_Tukey_HSD_2.pdf', gtb, 'pdf', 'figures', width=8, height=5)
}


# > 4.3 Visualización RADVIZ --------
require(reticulate)
use_python("C:/Users/ACER/AppData/Local/Programs/Python/Python38", T)

yb <- import('yellowbrick.features')
np <- import('numpy')

s_fct_ls1_DF <- s_fct_ls1 %>%
  distinct(I, label, parameter, last_value) %>% 
  pivot_wider(id_cols = c('I', 'label'), names_from=parameter, values_from=last_value)

X <- s_fct_ls1_DF %>% select(-I, -label, -LL) %>% as.matrix()
Y <- s_fct_ls1_DF %>% pull(label)

Y1 <- Y %>% 
  as.factor() %>% 
  forcats::fct_anon() %>% 
  sapply(function(x) as.integer(x)-1) %>% 
  np$array()

visualizer <- yb$radviz(
  X = X,
  y = Y1,
  features = X %>% attr('dimnames') %>% `[[`(2),
  colormap = 'viridis',
  classes = Y
)

visualizer$show(outpath = './figures/07_radviz.png')
