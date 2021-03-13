##------------------------------------------------------------------------------#
## Nombre del Script: Simulación de muestras de estudio de Jarkowski III A et al 
## 2014 que comparó individuos sanos con pacientes con neutropenia.
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

# Carga de código
source(file.path('src', '69_cargaPaquetes.R'), encoding = 'UTF-8')
source(file.path('src', '80_funConversionLog.R'), encoding = 'UTF-8')

# Definición de directorios de trabajo
wdir <- file.path('05_Jarkowski')
mdir <- file.path(wdir, '05_Jarkowski.mlxtran')
nIndiv <- 25 # N. pacientes en estudio

#-------------------------------------------------------------------------------#
# 1. Especificación de parámetros   ---------------------
#-------------------------------------------------------------------------------#
p <- list()

# Poblacionales
# Parámetros Estructurales
p['Cl_slope'] = 0.59  # (mL/min)/(mL/min)
p['Cl_nr']    = 7.0   # mL/min/65kg
p['Q_mn']     = 9.32  # L/65kg
p['V1_mn']    = 15.0  # L/65kg
p['Vss_mn']   = 38.9  # L/65kg

p['gCrCl'] = paramMoments(85.72, 37.28^2)$mu      # mL/min
p['sCrCl'] = paramMoments(85.72, 37.28^2)$sigma2  # mL/min

# graficoDistr(85.72, p$gCrCl, 37.28, sqrt(p$sCrCl), c(0, 200))
# abline(v = 85.72, lty = 4, col='green4')

p['gTBW']  = paramMoments(86.05, 19.42^2)$mu      # kg
p['sTBW']  = paramMoments(86.05, 19.42^2)$sigma2  # kg

# graficoDistr(86.05, p$gTBW, 19.42, sqrt(p$sTBW), c(0, 200))
# abline(v = 86.05, lty = 4, col='green4')

#-------------------------------------------------------------------------------#
# 2. Definición de dosis y t. administración ----------------------
#-------------------------------------------------------------------------------#
Cc    <- list(name = 'Cc', time = c(2, 10, 12, 14, 25))
ind   <- list(name = c('Cl', 'Q', 'V1', 'V2', 'CrCl', 'TBW'))

param <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)

#-------------------------------------------------------------------------------#
# 3. Simulación -------------------------------------------
#-------------------------------------------------------------------------------#

res <- simulx(model     = file.path(mdir), 
              parameter = unlist(p), 
              output    = list(Cc, ind),
              group     = list(
                size = c(nIndiv * nPob), 
                level = c('covariate')),
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
    level = c('covariate')
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

gProf <- prctilemlx(res$Cc, number = 80, level = c(95), plot=T, color='blue4') +
  geom_hline(yintercept = c(15, 20), linetype = 4) +
  theme(legend.position = 'none')

ggsave('perfilPlasmatico.pdf', gProf, 'pdf', wdir, 1, 6, 5)
