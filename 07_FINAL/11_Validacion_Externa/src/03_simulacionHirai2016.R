##------------------------------------------------------------------------------#
## Nombre del Script: Simulación de muestras de estudio de Hirai 2016 que 
## comparó individuos con neutropenia febril, con o sin ARC.
##  
## Propósito del Script: La definición de NF es diferente para este tipo.
## 
## Autor: Daniel S. Parra González 
## Fecha de creación: 11 - marzo - 2021
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

# Carga de código
source(file.path('src', '69_cargaPaquetes.R'), encoding = 'UTF-8')
source(file.path('src', '80_funConversionLog.R'), encoding = 'UTF-8')

# Definición de directorios de trabajo
wdir <- file.path('03_Hirai')
mdir <- file.path(wdir, '03_Hirai.mlxtran')
nIndiv <- 244 # N. pacientes en estudio
nPob <- 50

#-------------------------------------------------------------------------------#
# 1. Especificación de parámetros   ---------------------
#-------------------------------------------------------------------------------#
p <- list()

# Poblacionales
# Parámetros Estructurales

p['mCl'] = (wangScenario3(0.047, 0.037, 0.061, nIndiv) %>%
              {paramMoments(.$x, {.$s}^2)})$mu # L/h/kg
p['gCl'] = (wangScenario3(0.047, 0.037, 0.061, nIndiv) %>%
              {paramMoments(.$x, {.$s}^2)})$sigma2 # L/h/kg

# wangScenario3(0.047, 0.037, 0.061, nIndiv) %>%
#   {graficoDistr(.$x, p$mCl, .$s, sqrt(p$gCl), c(0, 0.2))}
# abline(v = 0.047, lty = 4, col='green4')
# abline(v = 0.037, lty = 4, col='green')
# abline(v = 0.061, lty = 4, col='green')

p['mV'] = (wangScenario3(1.8, 1.5, 2.1, nIndiv) %>%
{paramMoments(.$x, {.$s}^2)})$mu # L/kg
p['gV'] = (wangScenario3(1.8, 1.5, 2.1, nIndiv) %>%
{paramMoments(.$x, {.$s}^2)})$sigma2 # L/kg

# wangScenario3(1.8, 1.5, 2.1, nIndiv) %>%
#   {graficoDistr(.$x, p$mV, .$s, sqrt(p$gV), c(0, 4))}
# abline(v = 1.8, lty = 4, col='green4')
# abline(v = 1.5, lty = 4, col='green')
# abline(v = 2.1, lty = 4, col='green')

p['mTBW'] = (wangScenario3(52.1, 44.6, 60.6, nIndiv) %>%
{paramMoments(.$x, {.$s}^2)})$mu # kg
p['gTBW'] = (wangScenario3(52.1, 44.6, 60.6, nIndiv) %>%
{paramMoments(.$x, {.$s}^2)})$sigma2 # kg

# wangScenario3(52.1, 44.6, 60.6, nIndiv) %>%
#   {graficoDistr(.$x, p$mTBW, .$s, sqrt(p$gTBW), c(0, 100))}
# abline(v = 52.1, lty = 4, col='green4')
# abline(v = 44.6, lty = 4, col='green')
# abline(v = 60.6, lty = 4, col='green')

#-------------------------------------------------------------------------------#
# 2. Definición de dosis y t. administración ----------------------
#-------------------------------------------------------------------------------#
# Cc    <- list(name = 'Cc', time=seq(1, 30, length.out =100))
Cc    <- list(name = 'Cc', time = c(2, 10, 12, 14, 25))
ind   <- list(name = c('Cl', 'V', 'TBW'))
param <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)

# Dosificación
{
  pdf(file.path(wdir, 'distribuciones.pdf'), width = 6, height = 4)
  doseSSim <- wangScenario3(27.1, 19.3, 36.6, nIndiv) %>% {rnorm(1e3, .$x, .$s)}
  weightSim <- wangScenario3(52.1, 44.6, 60.6, nIndiv) %>% {rnorm(1e3, .$x, .$s)}
  doseDailySim <- doseSSim * weightSim
  # 
  layout(matrix(c(1, 3, 2, 3), 2, 2, byrow = TRUE))
  hist(doseSSim, border = 'blue', col = 'lightblue1',
       main = NULL, xlab = 'Dosis (mg/kg/d)', ylab = 'Frecuencia')
  hist(weightSim, border = 'red', col = 'lightpink1',
       main = NULL, xlab = 'Peso (kg)', ylab = 'Frecuencia')
  hist(doseDailySim, border = 'black', 
       main = NULL, xlab = 'Dosis (mg/d)', ylab = 'Frecuencia')
  dev.off()
}

#-------------------------------------------------------------------------------#
# 3. Simulación -------------------------------------------
#-------------------------------------------------------------------------------#

res <- simulx(model     = file.path(mdir), 
              parameter = unlist(p), 
              output    = list(Cc, ind),
              group     = list(
                size = c(nIndiv, nPob),
                level = c('covariate', 'population')),
              treatment = param,
              settings  = list(seed = 123456))

#-------------------------------------------------------------------------------#
# 4. Gráficos y almacenamiento  -----------------------------------
#-------------------------------------------------------------------------------#
# Almacenar perfiles
res$Cc %>% 
  mutate(Cc = round(Cc, 2)) %>% 
  write_csv(., file.path(wdir, 'simulated_OBS.csv'))


# Almacenar parametros
res$parameter %>%
  mutate(across(!contains('id'), ~ round(.x, 4))) %>%
  write_csv(., file.path(wdir, 'parameters.csv'))

#-------------------------------------------------------------------------------#
# 5. Perfil promedio --------------------------------------------
#-------------------------------------------------------------------------------#
Cc <- list(name = 'Cc', time=seq(0, 12, length.out =100) + (12 * 6))
param <- list(time = seq(0, 12 * 11, by = 12), amount = 1000, tinf = 2)

res <- simulx(
  model     = file.path(mdir),
  parameter = unlist(p),
  output    = list(Cc),
  group     = list(
    size = c(1000),
    level = c('population')
  ),
  treatment = param,
  settings  = list(seed = 123456, digits = 3)
)
# res$y %>% ggplot(aes(x=time, y=y, color = id)) + geom_line()
prctilemlx(res$Cc, number = 2, level = c(95), plot=FALSE) %>% 
  `$`('y') %>%
  as_tibble(.name_repair = 'universal') %>%
  mutate(across(!matches('time'), ~ round(.x, 1))) %>% 
  write_csv(., file.path(wdir, 'perfilPlasmatico.csv'))

gProf <- prctilemlx(res$Cc, number = 80, level = c(95), plot=T, color='yellow4') +
  geom_hline(yintercept = c(8.9, 16.3), linetype = 2) +
  theme(legend.position = 'none')

ggsave('perfilPlasmatico.pdf', gProf, 'pdf', wdir, 1, 6, 5)
