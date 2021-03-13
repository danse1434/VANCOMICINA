##------------------------------------------------------------------------------#
## Nombre del Script: Simulación de muestras de estudio de Santos-Buelga 2005 que 
## comparó individuos sanos con pacientes con neutropenia.
##  
## Propósito del Script: La definición de NF es diferente para este tipo.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 09 - marzo - 2021
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

# Carga de funciones
source(file.path('src', '69_cargaPaquetes.R'), encoding = 'UTF-8')
source(file.path('src', '80_funConversionLog.R'), encoding = 'UTF-8')

# Definición de directorios de trabajo
wdir <- file.path('02_SantosBuelga')
mdir <- file.path(wdir, '02_SantosBuelga.mlxtran')
nIndiv <- 215 # N. pacientes en estudio
nPob <- 50

#-------------------------------------------------------------------------------#
# 1. Especificación de parámetros   ---------------------
#-------------------------------------------------------------------------------#
p <- list()

# Poblacionales
# Parámetros Estructurales
p['mCl_pop'] = paramMoments(1.08, (1.08*.0212)^2)$mu      # L/h
p['gCl_pop'] = paramMoments(1.08, (1.08*.0212)^2)$sigma2  # L/h

# paramMoments(1.08, (1.08*.0212)^2) %>%
# {graficoDistr(1.08, .$mu, 1.08*.0212, sqrt(.$sigma2),
#               interv = c(0.5, 1.8),
#               legpos = list(pos ='topright', inset=0.01))}

p['mV_pop'] = paramMoments(0.98, (0.98*.0743)^2)$mu     # L
p['gV_pop'] = paramMoments(0.98, (0.98*.0743)^2)$sigma2 # L

# paramMoments(0.98, (0.98*.0743)^2) %>%
# {graficoDistr(0.98, .$mu, (0.98*.0743), sqrt(.$sigma2),
#               interv = c(0.5, 1.5),
#               legpos = list(pos ='topright', inset=0.01))}

# Variabilidad interindividual
p['mOmega_Cl'] = paramMoments(cvAOmega(28.16), (cvAOmega(28.16)*.1475)^2)$mu
p['gOmega_Cl'] = paramMoments(cvAOmega(28.16), (cvAOmega(28.16)*.1475)^2)$sigma2

# paramMoments(cvAOmega(28.16), (cvAOmega(28.16)*.1475)^2) %>%
# {graficoDistr(cvAOmega(28.16), .$mu, cvAOmega(28.16)*.1475, sqrt(.$sigma2),
#               interv = c(0.0, 0.15),
#               legpos = list(pos ='topright', inset=0.01))}

p['mOmega_V']  = paramMoments(cvAOmega(37.15), (cvAOmega(37.15)*.4812)^2)$mu
p['gOmega_V']  = paramMoments(cvAOmega(37.15), (cvAOmega(37.15)*.4812)^2)$sigma2

# paramMoments(cvAOmega(37.15), (cvAOmega(37.15)*.4812)^2) %>%
# {graficoDistr(cvAOmega(37.15), .$mu, cvAOmega(37.15)*.4812, sqrt(.$sigma2),
#               interv = c(0.0, 0.6),
#               legpos = list(pos ='topright', inset=0.01))}

p['corr_V_Cl'] = 0.2312

# Variabilidad residual
p['mA'] = paramMoments(3.52, (3.52*.1512)^2)$mu
p['gA'] = paramMoments(3.52, (3.52*.1512)^2)$sigma2

# paramMoments(3.52, (3.52*.1512)^2) %>%
# {graficoDistr(3.52, .$mu, (3.52*.1512), sqrt(.$sigma2),
#               interv = c(0.5, 5.5),
#               legpos = list(pos ='topright', inset=0.01))}

# Covariables
# Esta se debe pasar a L/h en el archivo mlxtran
# p['mClCr'] = log(89.4)
# p['gClCr'] = aproximarSigma(log(89.4), 39.2)$minimum

c(89.4, 39.2) * (60 / 1000) # pasar a L/h

p['mClCr'] = paramMoments(5.364, 2.352^2)$mu
p['gClCr'] = paramMoments(5.364, 2.352^2)$sigma2

# {graficoDistr(5.364, p$mClCr, 2.352, sqrt(p$gClCr),
#               interv = c(0, 15),
#               legpos = list(pos ='topright', inset=0.01))}

p['mTBW']  = paramMoments(64.7, 11.3^2)$mu
p['gTBW']  = paramMoments(64.7, 11.3^2)$sigma2

# graficoDistr(64.7, p$mTBW, 11.3, sqrt(p$gTBW),
#               interv = c(0, 150),
#               legpos = list(pos ='topright', inset=0.01))

#-------------------------------------------------------------------------------#
# 2. Definición de dosis y t. administración ----------------------
#-------------------------------------------------------------------------------#
Cc    <- list(name = 'Cc', time = c(2, 10, 12, 14, 25), lloq = 0.6)
ind   <- list(name = c('Cl', 'V', 'omega_Cl', 'omega_V',
                       'ClCr', 'TBW', 'a'))

param <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)

#-------------------------------------------------------------------------------#
# 3. Simulación -------------------------------------------
#-------------------------------------------------------------------------------#

res <- simulx(
  model     = file.path(mdir),
  parameter = unlist(p),
  output    = list(Cc, ind),
  group     = list(
    size = c(nIndiv, nPob),
    level = c('covariate', 'population')
  ),
  treatment = param,
  settings  = list(seed = 123456, digits = 3)
)

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
    size = c(nPob),
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


gProf <-
  prctilemlx(res$Cc, number = 80, level = c(95), plot=T, color='gray') +
  geom_hline(yintercept = c(10, 20, 30), linetype = 2) +
  theme(legend.position = 'none')

ggsave('perfilPlasmatico.pdf', gProf, 'pdf', wdir, 1, 6, 5)
