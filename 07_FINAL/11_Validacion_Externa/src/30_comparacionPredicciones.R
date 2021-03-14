##------------------------------------------------------------------------------#
## Nombre del Script: Comparación de perfiles plasmáticos Simulaciones
## 
##  
## Propósito del Script: La definición de NF es diferente para este tipo.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 04 - marzo - 2021
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# 1. Introducción ---------------------------------------
#-------------------------------------------------------------------------------#

# Carga de código
source(file.path('src', '69_cargaPaquetes.R'), encoding = 'UTF-8')

vectorEstudios <- c("01_Bury", "02_SantosBuelga", "03_Hirai", "04_Haeseker", 
                    "05_Jarkowski", "06_AlKhofide", "07_LeNormand")

mdir <- file.path('model', 'FINAL.mlxtran')
rdir <- file.path('model', 'reference_model.mlxtran')

#-------------------------------------------------------------------------------#
# 2. Apertura y modificación perfiles --------------------------------
#-------------------------------------------------------------------------------#

perfilLS <- vector(mode = 'list', length(vectorEstudios))

#-------------------------------------------------------------------------------#
# Lectura de perfiles plasmáticos simulados
for (i in 1:length(vectorEstudios)) {
  estudio <- vectorEstudios[i]
  
  perfilLS[[i]] <- 
    read_csv(file.path(estudio, 'perfilPlasmatico.csv'), col_types = cols())
}

#-------------------------------------------------------------------------------#
# Cálculo de percentiles para perfil plasmático N. 7
# Este perfil está disgregado por individuos, pero se pueden calcular 
# estadísticos de posición.
# 
perfilLS[[7]] <- perfilLS[[7]] %>% 
  group_by(time) %>% 
  summarise(
    `..2.5.`  = quantile(`..50.`, probs = 0.025, type = 4),
    `..50....3`  = quantile(`..50.`, probs = 0.5, type = 4),
    `..97.5.`  = quantile(`..50.`, probs = 0.975, type = 4)
    ) 

# Unificación de perfiles en todas las simulaciones
perfilDF <- perfilLS %>% 
  map(~ rename(.x, tiempo = time, 
               M = `..50....3`, 
               L = `..2.5.`,
               S = `..97.5.`)) %>% 
  map_dfr(~.x, .id= 'estudio') %>% 
  select(- `...4`) %>% 
  mutate(estudio  = as.numeric(estudio),
         estudio1 = vectorEstudios[estudio])


#-------------------------------------------------------------------------------#
# 3. Simulación para este modelo --------------------------
#-------------------------------------------------------------------------------#
# Simulación para modelo en estudio
Cc <- list(name = 'y1', time=seq(0, 12, length.out =100) + (12 * 6))
param <- list(time = seq(0, 12 * 11, by = 12), amount = 1000, tinf = 2)
sim.param <- c(CLCRMLMIN = 120, WTKG = 80)

res <- simulx(
  project   = file.path(mdir),
  output    = list(Cc),
  group     = list(size = 1e3), 
  parameter = sim.param,
  treatment = param,
  settings  = list(seed = 123456, digits = 3)
)

# Manipulación de percentiles de este estudio
modeloEstudio <- prctilemlx(res$y1, number = 2, 
                            level = 95, plot=FALSE) %>% 
  `$`('y') %>%
  as_tibble(.name_repair = 'universal') %>%
  mutate(across(!matches('time'), ~ round(.x, 1))) %>% 
  rename(tiempo = time,
         M = `..50....3`,
         L = `..2.5.`,
         S = `..97.5.`) %>% 
  select(-`..50....4`) %>% 
  add_column(estudio = 8, estudio1 = '08_Este')

# Alisamiento
modeloEstudio['L'] <- with(modeloEstudio,
     ksmooth(tiempo, L, kernel = 'normal'))$y
modeloEstudio['M'] <- with(modeloEstudio,
      ksmooth(tiempo, M, kernel = 'normal'))$y
modeloEstudio['S'] <- with(modeloEstudio,
      ksmooth(tiempo, S, kernel = 'normal'))$y

# Gráfico compuesto
nombresEstudios <- c(
  "Bury D et al (2019) - NF", 
  "Santos Buelga D et al (2005) - NF", 
  "Hirai K et al (2016)", 
  "Haeseker M et al (2014)", 
  "Jarkowski III A et al (2012)", 
  "Al-Khofide H et al (2010)", 
  "LeNormand Y et al (1994) - NF",
  "Este (2021)"
)
# 
paletaColores <-
  c('yellow', 'gray', 'green4', 'green3', 
    'blue4', 'red3', 'purple', 'black')
paletaColores <- setNames(paletaColores, nombresEstudios)

paletaLineas <- c(rep(1, 7), 2)
paletaLineas <- setNames(paletaLineas, nombresEstudios)

paletaAlfas <- c(rep(0.05, 7), 0.3)
paletaAlfas <- setNames(paletaAlfas, nombresEstudios)

dfPerfil <- perfilDF %>%
  bind_rows(modeloEstudio) %>% 
  mutate(estudio2 = nombresEstudios[estudio])

gPerfil1 <- dfPerfil %>% 
  ggplot(aes(x = tiempo)) +
  geom_line(aes(y = M, color = estudio2, linetype = estudio2)) +
  geom_ribbon(aes(ymin = L, ymax = S, fill = estudio2, alpha = estudio2)) +
  xlab('Tiempo (hr)') + ylab('Conc. plasmática (mg/L)')  +
  scale_linetype_manual(values = paletaLineas) +
  scale_color_manual(values = paletaColores) +
  scale_fill_manual(values = paletaColores) +
  scale_alpha_manual(values = paletaAlfas) + 
  theme(legend.title = element_blank())

ggsave('001_perfilCompuestoSIM.pdf', gPerfil1, 'pdf', 
       'figures', 1, 8, 5)

# Conversión a plotly
htmlwidgets::saveWidget(
  plotly::ggplotly(gPerfil1) %>% plotly::as_widget(),
  file.path('figures', '002_perfilCompuestoSIM.html') %>% normalizePath(), 
  selfcontained = TRUE
)

# Figura en mosaico
paletaAlfas <- c(rep(0.2, 7), 0.6)
paletaAlfas <- setNames(paletaAlfas, nombresEstudios)

gPerfil2 <- gPerfil1 + 
  facet_wrap(. ~ estudio2, ncol = 4) + 
  scale_alpha_manual(values = paletaAlfas) + 
  theme(legend.position = 'none')

ggsave('003_perfilMosaicoSIM.pdf', gPerfil2, 'pdf', 
       'figures', 1, 8, 5)

# Conversión a plotly
htmlwidgets::saveWidget(
  plotly::ggplotly(gPerfil2) %>% plotly::as_widget(),
  file.path('figures', '004_perfilMosaicoSIM.html') %>% normalizePath(), 
  selfcontained = TRUE
)


#-------------------------------------------------------------------------------#
# 4. Estadísticos rendimiento predictivo ----------------------------
#-------------------------------------------------------------------------------#
require(patchwork)

predictivoLS <- vector(mode = 'list', length(vectorEstudios))

# Lectura de estadísticos predictivos
for (i in 1:length(vectorEstudios)) {
  estudio <- vectorEstudios[i]
  
  predictivoLS[[i]] <- 
    read_csv(file.path(estudio, 'resRendimientoPred.csv'), col_types = cols())
}

pred_df <- predictivoLS %>%
  map_df(~ .x, .id = 'estudio') %>%
  mutate(estudio  = as.numeric(estudio),
         estudio2 = nombresEstudios[estudio]) 

G_PRED <- pred_df %>%
  mutate(
    PE = MAPE * 100,
    orden = case_when(
      estudio == 1 ~ 1,
      estudio == 2 ~ 2,
      estudio == 3 ~ 7,
      estudio == 4 ~ 3,
      estudio == 5 ~ 4,
      estudio == 6 ~ 5,
      estudio == 7 ~ 6
    )
    estudio = factor(estudio, level = c(1:2, 7, 3:6)),
    estudio2 = reorder(estudio2, estudio)
  ) %>%
  ggplot(aes(x = PE, y = estudio2))+
  geom_boxplot(fill = alpha('gray', 0.4)) + 
  coord_cartesian(xlim = c(-20, 300)) + 
  geom_vline(xintercept = c(-20, +20), lty = 'dashed', col = 'red3') +
  xlab('MAPE (%)') +
  theme(axis.title.y = element_blank())

ggsave('005_predMAPE.pdf', G_PRED, 'pdf', 'figures', 1, 6.5, 4)


G_COMP_1 <- ((gPerfil1 + theme(legend.position = 'bottom')) + G_PRED) +
  plot_annotation(tag_levels = 'A')
ggsave('006_predMAPE_comp1.pdf', G_COMP_1, 'pdf', 'figures', 1, 8*2, 3*2)

G_COMP_2 <- (gPerfil2 + G_PRED) + 
  plot_layout(widths = c(1.5, 1)) +
  plot_annotation(tag_levels = 'A')
ggsave('007_predMAPE_comp2.pdf', G_COMP_2, 'pdf', 'figures', 1, 14, 6)
