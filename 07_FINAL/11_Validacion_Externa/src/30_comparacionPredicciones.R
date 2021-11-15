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
  
  if (i == 7) {
    colnames1 <- c('tiempo', 'p2.5', 'p50', 'p50_1', 'p97.5', 'grupo')
  } else {
    colnames1 <- c('tiempo', 'p2.5', 'p50', 'p50_1', 'p97.5')
  }
  
  perfilLS[[i]] <- 
    read_csv(file.path(estudio, 'perfilPlasmatico.csv'), 
             col_names = colnames1, col_types = cols(), skip=1)
}

#-------------------------------------------------------------------------------#
# Cálculo de percentiles para perfil plasmático N. 7
# Este perfil está disgregado por individuos, pero se pueden calcular 
# estadísticos de posición.
# 
perfilLS[[7]] <- perfilLS[[7]] %>% 
  group_by(tiempo) %>% 
  summarise(
    p2.5   = quantile(p50_1, probs = 0.025, type = 4),
    p50    = quantile(p50_1, probs = 0.5, type = 4),
    p97.5  = quantile(p50_1, probs = 0.975, type = 4)
    ) 

# Unificación de perfiles en todas las simulaciones
perfilDF <- perfilLS %>% 
  map(~ rename(.x, 
               tiempo = tiempo, 
               M = p50, 
               L = p2.5,
               S = p97.5)) %>% 
  map_dfr(~.x, .id= 'estudio') %>% 
  select(- p50_1) %>% 
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
  mutate(hover = glue::glue(
    "Estudio: {estudio2}<br>Tiempo: {round(tiempo,2)}<br>Concentración: {round(M,1)}")) %>% 
  ggplot(aes(x = tiempo, text = hover)) +
  geom_line(aes(y = M, color = estudio2, linetype = estudio2, text=NULL)) +
  geom_line(aes(y = M, color = estudio2, linetype = estudio2)) +
  geom_ribbon(aes(ymin = L, ymax = S, fill = estudio2, alpha = estudio2, text=NULL)) +
  xlab('Tiempo (hr)') + ylab('Conc. plasmática (mg/L)')  +
  scale_linetype_manual(values = paletaLineas) +
  scale_color_manual(values = paletaColores) +
  scale_fill_manual(values = paletaColores) +
  scale_alpha_manual(values = paletaAlfas) + 
  theme(legend.title = element_blank())

gPerfil1

ggsave('001_perfilCompuestoSIM.pdf', gPerfil1, 'pdf', 
       'figures', 1, 8, 5)

# Conversión a plotly
htmlwidgets::saveWidget(
  plotly::ggplotly(gPerfil1) %>% plotly::as_widget(),
  file.path('figures', '002_perfilCompuestoSIM.html') %>% normalizePath(), 
  selfcontained = TRUE
)

plotly::ggplotly(gPerfil1, tooltip = 'text') %>% 
  plotly::config(displayModeBar = FALSE) %>%
  saveRDS(file.path('figures', '002_perfilCompuestoSIM.rds'))

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


pred_df %>%
  mutate(PE = MAE, orden = ov[estudio],
         estudio2 = reorder(estudio2, orden)) %>%
  ggplot(aes(x = PE, y = fct_rev(estudio2)))+
  annotate(geom='rect', ymin = -Inf,ymax = Inf, xmin = 3.45, xmax = 4.33,
           fill = alpha('red1', 0.2), colour=NA, size=0) +
  geom_vline(xintercept = c(3.901), lty = 'dashed', col = 'red4') +
  geom_boxplot(fill = alpha('gray', 0.4)) + 
  coord_cartesian(xlim = c(2, 16)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) + 
  xlab('MAE (mg/L)') +
  theme(axis.title.y = element_blank())


ov <- c(1:2,4:7,3)

G_PRED <- pred_df %>%
  mutate(
    PE = MAPE * 100,
    orden = ov[estudio],
    estudio2 = reorder(estudio2, orden)
  ) %>%
  ggplot(aes(x = PE, y = fct_rev(estudio2)))+
  annotate(geom='rect', ymin = -Inf,ymax = Inf, xmin = 24.5, xmax = 32.9, 
           fill = alpha('red1', 0.2), colour=NA, size=0) +
  geom_vline(xintercept = c(+27.6), lty = 'dashed', col = 'red4') +
  geom_boxplot(fill = alpha('gray', 0.4)) + 
  coord_cartesian(xlim = c(20, 400)) + 
  scale_x_continuous(expand = expansion(mult = c(0, 0))) + 
  xlab('MAPE (%)') +
  theme(axis.title.y = element_blank())

ggsave('005_predMAPE.pdf', G_PRED, 'pdf', 'figures', 1, 6.5, 4)


layout <- '
AABB
AABB
AABB
AABB
CCCC
'

G_COMP_1 <- ((gPerfil1 + theme(legend.position = 'bottom')) + G_PRED + guide_area()) +
  plot_layout(design = layout, guides = 'collect') +
  plot_annotation(tag_levels = 'A')

ggsave('006_predMAPE_comp1.pdf', G_COMP_1, 'pdf', 'figures', 1, 8.5, 4)

G_COMP_2 <- ((gPerfil2 + theme(strip.text = element_text(size=5))) + G_PRED) + 
  plot_layout(widths = c(1.5, 1)) +
  plot_annotation(tag_levels = 'A')

ggsave('007_predMAPE_comp2.pdf', G_COMP_2, 'pdf', 'figures', 1, 8.5, 4)



require(boot)

#-------------------------------------------------------------------------------#
# Resumen de parámetros
#' Calcular resumen de parámetros de predicción
#'  funciones para calcular bootstrap
statBoot <- function(data, stat, indices) {
  d <- data[indices, ]
  m <- mean(pull(d, stat))
  return(m)
}

pred_df1 <- pred_df %>%
  filter(MAPE != Inf) %>%
  group_by(estudio, estudio2) %>%
  summarise(across(c(MAE, RMSE, MAPE), list(
    'mn' = mean, 'sd' = sd, 'med' = median
  ))) %>%
  pivot_longer(cols = matches('(mn|sd|med)$')) %>%
  separate(name,
           into = c('parameter', 'statistic'),
           sep = '\\_') %>%
  pivot_wider(names_from = 'statistic', values_from = 'value') %>%
  ungroup()


pred_df2 <- pred_df %>% 
  group_by(estudio) %>% 
  nest() %>% 
  mutate(
    MAE_bt = map(data, ~boot(.x, statistic = statBoot, 1E3, stat = 'MAE')),
    RMSE_bt= map(data, ~boot(.x, statistic = statBoot, 1E3, stat = 'RMSE')),
    MAPE_bt= map(data, ~boot(.x, statistic = statBoot, 1E3, stat = 'MAPE')),
    MAE_ci = map(MAE_bt,  ~boot.ci(.x, type = 'perc')$perc[4:5]),
    RMSE_ci= map(RMSE_bt,  ~boot.ci(.x, type = 'perc')$perc[4:5]),
    MAPE_ci= map(MAPE_bt,  ~boot.ci(.x, type = 'perc')$perc[4:5])
  ) %>% 
  unnest(c('MAE_ci', 'RMSE_ci', 'MAPE_ci')) %>% 
  add_column(L = rep(c('li', 'ls'), 7)) %>% 
  pivot_wider(names_from = L, values_from = c(MAE_ci, RMSE_ci, MAPE_ci))


pred_dfT <- pred_df2 %>% 
  select(-data, -matches('bt$')) %>% 
  pivot_longer(cols = matches('(li|ls)$')) %>% 
  separate(name, c('parameter', 'type', 'statistic'), sep = '\\_') %>% 
  select(-type) %>% 
  pivot_wider(names_from = statistic, values_from = value) %>% 
  right_join(pred_df1, by = c('estudio', 'parameter')) %>% 
  relocate(c(li, ls), .after = mn)

nota_pie <- glue::glue(
  "
  [1] Bury D, ter Heine R, van de Garde EMW, Nijziel MR, Grouls RJ, Deenen MJ. The effect of neutropenia on the clinical pharmacokinetics of vancomycin in adults. European Journal of Clinical Pharmacology 2019;75:921–8. https://doi.org/10.1007/s00228-019-02657-6.
  [2] Hirai K, Ishii H, Shimoshikiryo T, Shimomura T, Tsuji D, Inoue K, et al. Augmented renal clearance in patients with febrile neutropenia is associated with increased risk for subtherapeutic concentrations of vancomycin. Therapeutic Drug Monitoring 2016;38:706–10. https://doi.org/10.1097/FTD.0000000000000346.
  [3] Haeseker MB, Croes S, Neef C, Bruggeman CA, Stolk LML, Verbon A. Vancomycin dosing in neutropenic patients. PLoS ONE 2014;9. https://doi.org/10.1371/journal.pone.0112008.
  [4] Jarkowski III A, Forrest A, Sweeney RP, Tan W, Segal BH, Almyroudis N, et al. Characterization of vancomycin pharmacokinetics in the adult acute myeloid leukemia population. Journal of Oncology Pharmacy Practice 2012;18:91–6. https://doi.org/10.1177/1078155211402107.
  [5] Al-Kofide H, Zaghloul I, Al-Naim L. Pharmacokinetics of vancomycin in adult cancer patients. Journal of Oncology Pharmacy Practice 2010;16:245–50. https://doi.org/10.1177/1078155209355847.
  [6] Santos Buelga D, Del Mar Fernandez De Gatta M, Herrera E V, Dominguez-Gil A, García MJ. Population pharmacokinetic analysis of vancomycin in patients with hematological malignancies. Antimicrobial Agents and Chemotherapy 2005;49:4934–41. https://doi.org/10.1128/AAC.49.12.4934-4941.2005.
  [7] Le Normand Y, Milpiedb N, Kergueris M-F, Harousseau J. Pharmacokinetic parameters of vancomycin for therapeutic regimens in neutropenic adult patients. International Journal of Bio-Medical Computing 1994;36:121–5.
  "
)

pred_gt <- pred_dfT %>%
  ungroup() %>%
  select(-estudio) %>%
  gt(groupname_col = 'estudio2') %>%
  fmt_number(columns = c('mn', 'sd', 'med', 'li' , 'ls'),
             n_sigfig = 3) %>%
  cols_merge(columns = c('li', 'ls'), pattern = '[{1}, {2}]') %>%
  cols_label(
    parameter = md('**Parameter**'),
    mn = md('**Media**'),
    sd = md('**Desv. Est.**'),
    med = md('**Mediana**'),
    li = md('**IC95%**')
  ) %>%
  tab_footnote(locations = cells_column_labels(columns = vars(parameter)),
               footnote = nota_pie) %>%
  opt_row_striping(row_striping = TRUE) %>%
  tab_header('Estadísticos de Redimiento predictivo VAN - Modelo Final') %>%
  tab_options(
    table.font.size = '12px',
    footnotes.font.size = px(10),
    container.width = pct(50)
  )

gtsave(pred_gt,
       "008_tabla_resultados.html",
       normalizePath(file.path(getwd(), "figures")),
       inline_css = TRUE)
