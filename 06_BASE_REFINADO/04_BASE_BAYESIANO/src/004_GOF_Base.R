##------------------------------------------------------------------------------#
## Nombre del Script: Obtención de gráficos a partir de datos de figuras ---
## generados por Monolix GUI
##  
## Propósito del Script: crear y almacenar gráficos generados a partir de 
## los datos generados por la suite de Monolix, se debe colocar en la misma 
## carpeta en la que se encuentra el proyecto. Este script lee en la carpeta 
## ChartsData, que tiene como subdirectorios a cada gráfico generado por la 
## suite de Monolix
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 04-feb-2020
## Fecha de modificación: 29-may-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# 1. Introducción -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Carga de paquetes
require(ggrepel)
require(patchwork)

# Apertura de fuentes
source(file.path('src', '001_Preprocesamiento_Base.R'), encoding = 'UTF8')
source(file.path('src', '050_fun_funcion2Cptm.R'), encoding = 'UTF8')
source(file.path('src', '051_fun_graficos.R'), encoding = 'UTF8')
source(file.path('src', '052_fun_predictivePerformance.R'), encoding = 'UTF8')

# Abrir CODA de modelo bayesiano previos difusos
set.seed(2021)
modelName <- '080_modeltwoCptmDiagProp_NoInfo'
load(file = file.path('models', paste0(modelName, "Fit.Rsave")))

#-------------------------------------------------------------------------------#
# 2. Preprocesamiento de datos ----------------
#-------------------------------------------------------------------------------#

# > 2.1. Evaluación estadísticos resumen ----------------------------------
#................................................................................
#' 1- Leer predicciones poblacionales *cObsCond* e individuales *cObsPred*.
#' 2- Expandir columnas
#' 3- Convertir en _NA_ valores con formato -99
#' 4- Agrupar por nombre (cada tipo de predicción para cada tiempo)
#' 5- Calcular mediana y cuantiles inferior / superior
#' 6- Convertir a nombre de formato "param[3]" a "param_3"
#' 7- Separar columna en tipo de predicción y tiempo (t_{ij})
#' 8- Ordenar por nombre y número
#' 9- Expandir por tipo y valores
#' 10- Adicionar observaciones iniciales *cObs*, tiempos y sujetos correspondietes
#................................................................................

predictions1 <-
  as.data.frame(fit, pars = c("cHat", "cObsCond", "cHatPred", "cObsPred")) %>%
  pivot_longer(cols=everything()) %>%
  mutate(value = ifelse(value == -99, NA, value)) %>%
  group_by(name) %>%
  summarize(lwr_q  = quantile(value, probs = 0.05, na.rm = TRUE),
            median = quantile(value, probs = 0.5, na.rm = TRUE),
            upr_q  = quantile(value, probs = 0.95, na.rm = TRUE),
            mean   = mean(value),
            var    = var(value)
            ) %>% 
  mutate(name = str_replace(name, '\\[', '_'), 
         name = str_replace(name, '\\]', '')) %>% 
  separate(col='name', into=c('name', 'number'), sep='\\_', convert = TRUE) %>% 
  arrange(name, number) %>% 
  pivot_wider(
    id_cols = number,
    names_from = name,
    values_from = c(lwr_q, median, upr_q, mean, var)
  ) %>% 
  mutate(
    cObs = stan_d$cObs,
    time = stan_d$time,
    subjects = stan_d$subject
  )

# > 2.2. Crear gráfico de cinética Cp vs t --------------------------------

gPRED <- predictions1 %>% 
  rename(ID=subjects) %>% 
  ggplot(aes(x=time))+
  geom_ribbon(aes(ymin=lwr_q_cObsPred, ymax=upr_q_cObsPred), fill=alpha('blue', 0.2)) +
  geom_line(aes(y=median_cObsPred), col='blue', lty='dashed') +
  geom_ribbon(aes(ymin=lwr_q_cObsCond, ymax=upr_q_cObsCond), fill=alpha('green', 0.3)) +
  geom_line(aes(y=median_cObsCond), col='green4') +
  geom_point(aes(y=cObs))+
  facet_wrap(. ~ ID, ncol = 4, labeller = labeller(.cols = label_both)) + 
  theme_bw()+
  xlab('Tiempo (hr)') + ylab(bquote(C[PRED]~(mg/L)))

# Almacenamiento en figuras
ggsave(file.path('figures', paste0(modelName, 'CPRED.pdf')),
       plot = gPRED, 
       device = 'pdf', 
       width = 6, height = 7)
    
# Almacenamiento de datos
write_csv(summary(fit)$summary %>% as_tibble(rownames = 'P'), 
          file.path('reports', paste0(modelName, '_Results.txt')))

#-------------------------------------------------------------------------------#
# 3. Bondad de ajuste -----------------------------------------------------
#-------------------------------------------------------------------------------#


# > 3.1. Extracción de parámetros de modelo -------------------------------

# Crear lista con nombre de parámetros para 
parameters <- as.matrix(fit) %>% 
  colnames() %>% 
  keep(~ str_detect(.x, 'CLHat|QHat|V1Hat|V2Hat|omega|rho|^b$')) %>% 
  discard(~ str_detect(.x, 'rho\\[1,1\\]|rho\\[2,2\\]|rho\\[3,3\\]|rho\\[4,4\\]'))%>% 
  discard(~ str_detect(.x, 'rho\\[2,1\\]|rho\\[3,1\\]|rho\\[4,1\\]|rho\\[3,2\\]|rho\\[4,2\\]|rho\\[4,3\\]'))

# Crear dataFrame con datos de parámetros
#................................................................................
#' 1- Convertir objeto fit de Stan en dataFrame seleccionado *parameters*
#' 2- Para todas las columnas calcular media, desvest, mediana, e intervalo 
#' cuartílico.
#' 3- Colapsar para mostrar parámetro y estadístico
#' 4- Expandir por estadístico
#................................................................................

model_Specs <- 
  as.data.frame(fit, pars = parameters) %>%
  summarise(
    across(everything(), 
           list(
             mn = mean, 
             sd = sd, 
             med = median, 
             q2.5 = ~ quantile(.x, 0.025),
             q97.5 = ~ quantile(.x, 1-0.025)))) %>%
    # as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = c('parameter', 'statistic'),
               names_pattern = "(.*)_(.*)", values_to = 'vals') %>% 
  pivot_wider(id_cols='parameter', names_from = 'statistic', values_from='vals')


# > 3.2. Calcular residuales ----------------------------------------------

# Calcular residuales NPDE 
#................................................................................
#' 1- Extraer predicciones poblacionales
#' 2- Colapsar por parámetros
#' 3- Cambiar formato de parámetros
#' 4- Separar por parámetro y número
#' 5- Adicionar observaciones iniciales
#' 6- Calcular condicional de F_{ij}
#' 7- Agrupar por número
#' 8- Calcular F_{ij}
#' 9- Convertir a la inversa de dist. normal
#................................................................................

NPDE_df <- as.data.frame(fit, pars = c("cObsCond")) %>%
  pivot_longer(cols=everything()) %>% 
  mutate(name = str_replace(name, '\\[', '_'), 
         name = str_replace(name, '\\]', '')) %>% 
  separate(col='name', into=c('name', 'number'), sep='\\_', convert = TRUE) %>% 
  left_join(predictions1 %>% select(number, cObs), by='number') %>% 
  mutate(Fij = ifelse(value < cObs, 1, 0)) %>% 
  group_by(number) %>% 
  summarise(Fij = mean(Fij)) %>% 
  mutate(NPDE = qnorm(Fij))

# Extraer el valor de b (modelo de error residual) como parámetro
b <- model_Specs[model_Specs$parameter=='b',]$mn

# Calcular IWRES y PWRES
predictions2 <- predictions1 %>%
  mutate(
    IWRES = (log(cObs) - log(mean_cHat)) / (b * log(mean_cHat)),
    PWRES = (cObs - mean_cHat)/sqrt(var_cHat)
  ) %>% 
  # Anexar NPDE
  left_join(NPDE_df %>% select(number, NPDE), by = 'number')


# Seleccionar tema de gráficos
theme_set(theme_bw())

# GOF poblacional
G_PRED_OBS_PRED <- 
  GOF_PRED(predictions2, mean_cObsPred, 'cObs', 'blue', 'PRED', 'OBS', 
         xlim = c(0,45), ylim = c(0,45)) 
  
# GOF individual
G_PRED_OBS_IPRED <- 
GOF_PRED(predictions2, mean_cObsCond, 'cObs', 'red', 'IPRED', 'OBS', 
         xlim = c(0,45), ylim = c(0,45))


G_PRED_OBS_PRED <- G_PRED_OBS_PRED + predictivePerformaceLabel(
  predictions2, 'mean_cObsPred', 'cObs', x = 0.05, round = 3, size=2.3, y=0.8,
  boot = TRUE, R = 1e3, xlim = c(0,45), ylim = c(0,45)
)
  
G_PRED_OBS_IPRED <- G_PRED_OBS_IPRED + predictivePerformaceLabel(
  predictions2, 'mean_cObsCond', 'cObs', x = 0.05, round = 3, size=2.3, y=0.8,
  boot = TRUE, R = 1e3, xlim = c(0,45), ylim = c(0,45)
)

G1 <- (G_PRED_OBS_PRED + G_PRED_OBS_IPRED) &
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray95"))

G1 <- G1 + plot_annotation(tag_levels = 'A')

ggsave(file.path('figures','G_GOF.pdf'), G1, 
       device = 'pdf', width = 8, height = 4, units = 'in')  

#-------------------------------------------------------------------------------#
# 4. Gráficos de Residuales ------------------
#-------------------------------------------------------------------------------#
# Gráficos de residuales para tiempo
G_RES_T_PWRES <- RES_TSFD(
  predictions2,
  x = time,
  y = PWRES,
  id = subjects,
  xlab = 'TSFD (h)',
  ylab = 'PWRES',
  bins = 7
)

G_RES_T_IWRES <- RES_TSFD(
  predictions2,
  x = time,
  y = IWRES,
  id = subjects,
  xlab = 'TSFD (h)',
  ylab = 'IWRES',
  bins = 7
)

G_RES_T_NPDE <- RES_TSFD(
  predictions2,
  x = time,
  y = NPDE,
  id = subjects,
  xlab = 'TSFD (h)',
  ylab = 'NPDE',
  bins = 7
)

# Gráficos de residuales para concentración

G_RES_C_PWRES <- RES_TSFD(
  predictions2,
  x = mean_cObsCond,
  y = PWRES,
  id = subjects,
  xlab = 'PRED (mg/L)',
  ylab = 'PWRES',
  bins = 7
)

G_RES_C_IWRES <- RES_TSFD(
  predictions2,
  x = mean_cObsPred,
  y = IWRES,
  id = subjects,
  xlab = 'IPRED (mg/L)',
  ylab = 'IWRES',
  bins = 7
)

G_RES_C_NPDE <- RES_TSFD(
  predictions2,
  x = mean_cObsCond,
  y = NPDE,
  id = subjects,
  xlab = 'PRED (mg/L)',
  ylab = 'NPDE',
  bins = 7
)

# Gráfico compuesto
G2 <- 
  (G_RES_T_PWRES + G_RES_C_PWRES) / 
  (G_RES_T_IWRES + G_RES_C_IWRES) /
  (G_RES_T_NPDE  + G_RES_C_NPDE)  +
  plot_annotation(tag_levels = 'A')

ggsave(file.path('figures', 'G_RES.pdf'), G2, 'pdf', 
       width = 8, height = 8, units = 'in')











