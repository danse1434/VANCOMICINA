##------------------------------------------------------------------------------#
## Nombre del Script: análisis de outliers mediante jacknife --------------------
##  
## Propósito del Script: análisis de influencia de individuos
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 30-12-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(rlang)
require(tidyverse)
require(ggrepel)
require(tidymodels)
require(patchwork)
require(bigutilsr)
require(ggfortify)

# Lectura de archivo de funciones
source(file.path('src', '011_funciones_analisis.R'), encoding = 'UTF-8')

# Órden de parámetros
par_lev = c('Cl_pop', 'Q_pop', 'V1_pop', 'V2_pop',
            'omega_Cl', 'omega_Q', 'omega_V1', 'omega_V2', 'b')

sel_par = c('CLHat', 'QHat', 'V1Hat', 'V2Hat', 
            'omega[1]', 'omega[2]', 'omega[3]', 'omega[4]', 'b')

sel_par = setNames(sel_par, par_lev) # Convertir en vector con nombres 

modelName <- '090_Base_Model'
#-------------------------------------------------------------------------------#
# 1. Lectura de archivos con parámetros Pop ----------------------------
#-------------------------------------------------------------------------------#
# 1.1 Crear lista con parámetros POP estimados con Jacknife.
#................................................................................
#  1 Prealocar la lista *data_vector*
#  2 En cada elemento de la lista, colocar la tabla con los parámetros en cada 
#     carpeta.
#................................................................................

data_vector = vector('list', 14L)

for (i in 1:14) {
  load(file = file.path('models', paste0(modelName, '_del', i, "Fit.Rsave")))
  data_vector[[i]] = rstan::summary(fit, pars=sel_par)
}

# 1.2 Convertir lista a formato tabular

data_df <- data_vector %>%
  map(., 'summary') %>% 
  map_df(~as_tibble(.x, rownames='Parameter'), .id = 'Sujeto') %>% 
  mutate(
    Parameter = factor(Parameter),
    Parameter = fct_recode(Parameter, !!!sel_par),
    Sujeto = as.integer(Sujeto)
    ) %>% 
  rename(
    P2.5 = `2.5%`,
    P25 = `25%`,
    P50 = `50%`,
    P75 = `75%`,
    P97.5 = `97.5%`,
  )

# 1.3 Creación de matriz de diseño

data_mn <- data_df %>%
  pivot_wider(id_cols = Sujeto,
              names_from = Parameter,
              values_from = P50) 

# 1.4 Crear receta, normalizar, y preparar

rec <- recipe(Sujeto ~ ., data = data_mn)
nor <- rec %>% step_normalize(all_predictors()) 
pre <- nor %>% prep(data_mn)

# Selección de tema
theme_set(
  theme_bw() + theme(panel.border = element_rect(fill = NA, colour = 'black'))
  )

# Gráfico de cambio en cada parámetro en forma univariada 
G1 <- bake(pre, data_mn) %>%
  pivot_longer(
    cols = !matches('Sujeto'),
    names_to = 'parameter',
    values_to = 'std_value'
  ) %>% 
  mutate(parameter = factor(parameter, levels = par_lev)) %>% 
  ggplot(aes(x = Sujeto, y = std_value, 
             fill = ifelse(abs(std_value) > 1.960, 'out', 'in'))) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", col = 'black', size = 0.04) + 
  geom_text(data = . %>% filter(abs(std_value) > 1.960),
            aes(y = std_value * 1.2, label = paste0('ID ', Sujeto)), 
            size = 2.4, hjust = 0.75) +
  geom_hline(yintercept = c(-1.960, 1.960), lty = 'dashed') +
  facet_wrap(~parameter, ncol = 4) + 
  scale_x_continuous(breaks = seq(1,16,3)) +
  scale_fill_manual(values = c('green1', 'red')) +
  ylab("Valor Estandarizado") + 
  coord_cartesian(ylim = c(-4,4)) +
  theme(legend.position = "none", axis.title.x = element_blank())

# G1
ggsave("1_valores_estandarizados.pdf", G1, 'pdf', 'figures', 1, 5, 4)


#-------------------------------------------------------------------------------#
# 2. Análisis de Componentes Principales -------------------------
#-------------------------------------------------------------------------------#
# Realización de análisis de componentes principales (PCA)
# 
# Se utilizó la función "prcomp" de la librería stats, se elimina la variable 
# sujeto ya que sólo se aceptan las variables que componen las características 
# del sujeto. Se almacena todo en el objeto *pca1*.

pca1 <- bake(pre, data_mn) %>%
  select(-Sujeto) %>%
  stats::prcomp(.)

# Selección de la matriz con las variables transformadas en los componentes 
# principales. *pca2*

((pca1$sdev)^2 *100/ sum((pca1$sdev)^2)) %>% 
  # `[`(1:4) %>% sum()
  map_dbl( ~ round(.x, 3))
  
pca1 %>% autoplot(x = 1, y =2)
# Los cuatro primeros componentes explican 60.46%, 15.53%, 10.95%, y 6.47% de la 
# variabilidad del modelo (en total 93.4%).

pca2 <- pca1$x

#-------------------------------------------------------------------------------#
# 3. Aplicación de criterios de identificación de outliers -------------------
#-------------------------------------------------------------------------------#
# Criterio de distancia de al menos 6 desviaciones estándar desde la media.
#................................................................................
#  1 Seleccionar la matriz
#  2 A cada columna (2), aplicar una función que identifique cuales valores 
#  dentro de la matriz cumplen la condición que el valor abs de la 
#  diferencia con la media son mayores a 6 veces sd.
#................................................................................

apply(pca2, 2, function(x)
  which(abs(x - mean(x)) > (6 * sd(x))))

apply(pca2, 2, function(x)
  which((abs(x - median(x)) / mad(x)) > 6)) 

# Para PC1 se identifica al individuo 7 como posible individuo atípico.

# El criterio no es capaz de identificar algún outlier variando el umbral de 
# desviación estándar, debido a que no considera ninguno de los datos outlier 
# o los identifica a todos como tal.

dist <- apply(pca2, 2, function(x)
  (abs(x - median(x)) / mad(x) > 6)) %>%
  apply(1, max)

#-------------------------------------------------------------------------------#
# Aplicación de la distancia robusta de Mahalanobis
# 
# Se aplica la función dist_ogk del paquete "bigutilsr" que permite calcular 
# la distancia de Mahalanobis para cada uno de los sujetos. Esta medida 
# permite calcular la distancia entre un Punto P y una distribución D. 
# 
# Es una generalización multidimensional de la idea de medir cuantas desvest 
# esta lejos de la media de P. 

dist2 <- bigutilsr::dist_ogk(pca2, 20)
qplot(dist, sqrt(dist2))

pval <- pchisq(dist2, df = 9, lower.tail = FALSE)
hist(pval)

# Corrección de Bonferroni
is.out <- (pval < (0.05 / length(dist2)))  

gPCA_Maha <- 
  qplot(pca2[, 1], pca2[, 2], color = is.out, size = I(3)) + 
  geom_text_repel(aes(label=1:14)) +
  theme(legend.position = c(0.8, 0.8)) + 
  xlab(expression(PC[1])) + ylab(expression(PC[2])) +
  labs(title = 'Identificación Outliers', subtitle = 'Distancia Mahalonobis')

# G1
ggsave("3_distanceMahalonnobis.pdf", gPCA_Maha, 'pdf', 'figures', 1, 5, 4)

#-------------------------------------------------------------------------------#
# Aplicación de LOF
LOF_1 <- LOF(U = pca2, seq_k = c(3:10))

gPCA_LOF <- ggplot(pca2, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = 'Observaciones', shape='Observaciones'), size=0.1) +
  geom_point(
    aes(color = 'LOF Score', shape='LOF Score'),
    size = LOF_1*0.1/min(LOF_1) # El valor de LOF define el tamaño del punto 
  )  +
  xlab("PC1 (56.4%)") + ylab("PC2  (17.2%)") +
  scale_color_manual(values = c('black', 'red'), breaks = c('Observaciones', 'LOF Score'), name='L') +
  scale_shape_manual(values = c(16, 1), breaks = c('Observaciones', 'LOF Score'), name='L') +
  geom_text_repel(aes(label = ifelse(LOF_1 >= 1, round(LOF_1, 2), NA_real_) )) +
  # labs(title = 'Identificación Outliers', subtitle = 'Local Outlier Factor') +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank())

# gPCA_LOF
ggsave("4_LOF_identification.pdf", gPCA_LOF, 'pdf', 'figures', 1, 5, 4)

#-------------------------------------------------------------------------------#
# 4. Gráficos finales PCA ----------------------------------------------------
#-------------------------------------------------------------------------------#
# Gráficos con PCA para cada par (PC1,PC2)-(PC2,PC3)-(PC1,PC3)
# 
coords <- list(coord_cartesian(xlim = c(-3.0, 8.0), ylim = c(-3.0, 3.0)))

gm1 <- pcout(pca2, PC1, PC2)$graph +
  elipsogk(pca2, c(1, 2), 0.10) +
  elipsogk(pca2, c(1, 2), 0.01) +
  elipsogk(pca2, c(1, 2), 0.05) +
  geom_point(aes(x = bigutilsr::covrob_ogk(pca2)$center[1], 
                 y = bigutilsr::covrob_ogk(pca2)$center[2]), 
             color = "red", size = 3, shape = 8) +
  coords + 
  geom_text(
    pcout(pca2, PC1, PC2)$data %>%
      add_column(is.out) %>%
      filter(is.out == TRUE),
    mapping = aes(label = ID),
    nudge_x = 0.5
  ) + xlab('PC1 (56.4%)') + ylab('PC2 (17.2%)')

gm2 <- pcout(pca2, PC1, PC3)$graph +
  elipsogk(pca2, c(1, 3), 0.10) +
  elipsogk(pca2, c(1, 3), 0.01) +
  elipsogk(pca2, c(1, 3), 0.05) +
  geom_point(aes(x = bigutilsr::covrob_ogk(pca2)$center[1], 
                 y = bigutilsr::covrob_ogk(pca2)$center[3]), 
             color = "red", size = 3, shape = 8) +
  coords + 
  geom_text(
    pcout(pca2, PC1, PC3)$data %>%
      add_column(is.out) %>%
      filter(is.out == TRUE),
    mapping = aes(label = ID),
    nudge_x = 0.5
  ) + xlab('PC1 (56.4%)') + ylab('PC3 (9.9%)')

gm3 <- pcout(pca2, PC2, PC3)$graph +
  elipsogk(pca2, c(2, 3), 0.10) +
  elipsogk(pca2, c(2, 3), 0.01) +
  elipsogk(pca2, c(2, 3), 0.05) +
  geom_point(aes(x = bigutilsr::covrob_ogk(pca2)$center[2], 
                 y = bigutilsr::covrob_ogk(pca2)$center[3]), 
             color = "red", size = 3, shape = 8) +
  coords + 
  geom_text(
    pcout(pca2, PC2, PC3)$data %>%
      add_column(is.out) %>%
      filter(is.out == TRUE),
    mapping = aes(label = ID),
    nudge_x = 0.5
  ) + xlab('PC2 (17.2)') + ylab('PC3 (9.9%)')

#-------------------------------------------------------------------------------#
# Gráfico 3D --------------------------------------------------------------
#-------------------------------------------------------------------------------#
W         <- pcout(pca2, PC1, PC3)$data
rot       <- seq(-90, 100, length.out = 20)
rot_index <- seq(1, 20, length.out = 20)

plot3D_PCA <- function() {
  par(xpd=TRUE, mar=rep(0,4), bg=NA)
  
  plot3D::scatter3D(
    x = W$PC1, y = W$PC2, z = W$PC3,
    colvar = NULL, col = "blue", pch = 19, cex = 0.5,
    bty = 'b2', type = 'h', theta = rot[14], phi = 20,
    xlab = 'PC1', ylab = 'PC2', zlab = 'PC3', ticktype = "detailed",
    xlim = c(-3.0, 8.0), ylim = c(-3.0, 3.0), zlim = c(-3.0, 2.0)
  )
  
  W[W$ID %in% c(2,8,7,10), ] %>% 
    plot3D::text3D(x = .$PC1 + 0.05, 
                   y = .$PC2 + 0.05, z = .$PC3 + 0.5,  
                   labels = .$ID, add = TRUE, colkey = FALSE, cex = 1.3) 
}

gT <- (gm1 + gm2 + gm3 +
         (
           wrap_elements(panel =  ~ plot3D_PCA(), clip = FALSE) +
             theme(plot.margin = margin(0, 0, 0, 0))
         )) +
  plot_layout(ncol = 2) + plot_annotation(tag_levels = 'A')

# gT

ggsave('2_CompuestoPC.pdf', gT, 'pdf', 'figures', 1, 5*2, 4*2)
ggsave('2_CompuestoPC.png', gT, 'png', 'figures', 2, dpi = 'print')

# Gráfico compuesto con inclusión de puntuaciones LOF

gT1 <- (gm1 + gm2 + (gPCA_LOF + coords) +
         (
           wrap_elements(panel =  ~ plot3D_PCA(), clip = FALSE) +
             theme(plot.margin = margin(0, 0, 0, 0))
         )) +
  plot_layout(ncol = 2) + plot_annotation(tag_levels = 'A')

# gT1

ggsave('5_CompuestoPC_LOF.pdf', gT1, 'pdf', 'figures', 1, 5*2, 4*2)
