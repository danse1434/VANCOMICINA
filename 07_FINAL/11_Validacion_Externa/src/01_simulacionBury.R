##------------------------------------------------------------------------------#
## Nombre del Script: Simulación de muestras de estudio de Bury 2019 que 
## comparó individuos sanos con pacientes con neutropenia.
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
wdir <- file.path('01_Bury')
mdir <- file.path(wdir, '01_Bury.mlxtran')
nIndiv <- 116 # N. pacientes en estudio
nPob <- 50

#-------------------------------------------------------------------------------#
# 1. Especificación de parámetros   ---------------------
#-------------------------------------------------------------------------------#
p <- list()

# Poblacionales
# Parámetros Estructurales
p['mtheta_1'] = paramMoments(3.22, desvDeInt(3.22, 2.97, 3.48, nIndiv) ^ 2)$mu # L/h
p['gtheta_1'] = paramMoments(3.22, desvDeInt(3.22, 2.97, 3.48, nIndiv) ^ 2)$sigma2 # L/h

# paramMoments(3.22, desvDeInt(3.22, 2.97, 3.48, nIndiv) ^ 2) %>%
# {graficoDistr(3.22, .$mu, desvDeInt(3.22, 2.97, 3.48, nIndiv), sqrt(.$sigma2),
#               interv = c(0.5, 10),
#               legpos = list(pos ='topright', inset=0.01))}
# abline(v = 2.97, lty = 4, col='green')
# abline(v = 3.48, lty = 4, col='green')

p['mTheta_2'] = paramMoments(8.34E-3, desvDeInt(8.34E-3, 7.37E-3, 9.28E-3, nIndiv) ^ 2)$mu
p['gTheta_2'] = paramMoments(8.34E-3, desvDeInt(8.34E-3, 7.37E-3, 9.28E-3, nIndiv) ^ 2)$sigma2

# paramMoments(8.34E-3, desvDeInt(8.34E-3, 7.37E-3, 9.28E-3, nIndiv) ^ 2) %>%
# {graficoDistr(
#   8.34E-3, .$mu,
#   desvDeInt(8.34E-3, 7.37E-3, 9.28E-3, nIndiv), sqrt(.$sigma2),
#   interv = c(0., 0.04),
#   legpos = list(pos ='topright', inset=0.01))}

p['mTheta_3'] = 1.277 %>% {paramMoments(., desvDeInt(., 1.102, 1.462, nIndiv) ^ 2)$mu}
p['gTheta_3'] = 1.277 %>% {paramMoments(., desvDeInt(., 1.102, 1.462, nIndiv) ^ 2)$sigma2}

# 1.277 %>%
#   {paramMoments(., desvDeInt(., 1.102, 1.462, nIndiv) ^ 2)} %>%
#   {graficoDistr(1.277, .$mu, desvDeInt(1.277, 1.102, 1.462, nIndiv), sqrt(.$sigma2),
#               interv = c(0.0, 4),
#               legpos = list(pos ='topright', inset=0.01))}

p['mQ'] = paramMoments(4.03 + 2.5, desvDeInt(4.03, 2.81, 5.22, nIndiv) ^ 2)$mu
p['gQ'] = paramMoments(4.03 + 2.5, desvDeInt(4.03, 2.81, 5.22, nIndiv) ^ 2)$sigma2

# paramMoments(4.03+2.5, desvDeInt(4.03, 2.81, 5.22, nIndiv) ^ 2) %>%
#   {graficoDistr(4.03, .$mu, desvDeInt(4.03, 2.81, 5.22, nIndiv), sqrt(.$sigma2),
#               interv = c(0.0, 12),
#               legpos = list(pos ='topright', inset=0.01))}

p['mV1'] = 45.8 %>% {paramMoments(., desvDeInt(., 40.8, 51.1, nIndiv) ^ 2)$mu}
p['gV1'] = 45.8 %>% {paramMoments(., desvDeInt(., 40.8, 51.1, nIndiv) ^ 2)$sigma2}

# paramMoments(45.8, desvDeInt(45.8, 40.8, 51.1, nIndiv) ^ 2) %>%
#   {graficoDistr(45.8, .$mu, desvDeInt(45.8, 40.8, 51.1, nIndiv), sqrt(.$sigma2),
#               interv = c(0.0, 100),
#               legpos = list(pos ='topright', inset=0.01))}

p['mV2'] = paramMoments(51.7+30, desvDeInt(51.7, 37.2, 66.2, nIndiv) ^ 2)$mu
p['gV2'] = paramMoments(51.7+30, desvDeInt(51.7, 37.2, 66.2, nIndiv) ^ 2)$sigma2

# paramMoments(51.7+30, desvDeInt(51.7, 37.2, 66.2, nIndiv) ^ 2) %>%
# {graficoDistr(51.7, .$mu, desvDeInt(51.7, 37.2, 66.2, nIndiv), sqrt(.$sigma2),
#               interv = c(0.0, 200),
#               legpos = list(pos ='topright', inset=0.01))}

# Variabilidad interindividual
p['mOmega_Cl'] = paramMoments(cvAOmega(31), 
                              cvAOmega(c(31., 25.1, 35.3)) %>% 
                              {desvDeInt(.[1], .[2], .[3], nIndiv)^2})$mu
p['gOmega_Cl'] = paramMoments(cvAOmega(31), 
                              cvAOmega(c(31., 25.1, 35.3)) %>% 
                              {desvDeInt(.[1], .[2], .[3], nIndiv)^2})$sigma2

# graficoDistr(cvAOmega(31),
#              p$mOmega_Cl,
#              cvAOmega(c(31., 25.1, 35.3)) %>% {desvDeInt(.[1], .[2], .[3], nIndiv)},
#              p$gOmega_Cl,
#              interv = c(0.0, 0.6),
#              legpos = list(pos ='topright', inset=0.01))
# 
# abline(v = cvAOmega(c(25.1, 35.3)), lty = 4, col='green')

p['mOmega_V1'] = paramMoments(cvAOmega(35.2), 
                              cvAOmega(c(35.2, 25, 43.8)) %>% 
                              {desvDeInt(.[1], .[2], .[3], nIndiv)^2})$mu
p['gOmega_V1'] = paramMoments(cvAOmega(35.2), 
                              cvAOmega(c(35.2, 25, 43.8)) %>% 
                              {desvDeInt(.[1], .[2], .[3], nIndiv)^2})$sigma2

# graficoDistr(cvAOmega(35.2),
#              p$mOmega_V1, # Se debe colocar un offset de 10 unid
#              cvAOmega(c(35.2, 25, 43.8)) %>%
#                {desvDeInt(.[1], .[2], .[3], nIndiv)},
#              sqrt(p$gOmega_V1),
#              interv = c(0.0, 0.6),
#              legpos = list(pos ='topright', inset=0.01))
# 
# abline(v = cvAOmega(c(25, 43.8)), lty = 4, col='green')

p['mOmega_V2'] = paramMoments(cvAOmega(97.8), 
                              cvAOmega(c(97.8, 60.9, 124)) %>% 
                              {desvDeInt(.[1], .[2], .[3], nIndiv)^2})$mu
p['gOmega_V2'] = paramMoments(cvAOmega(97.8), 
                              cvAOmega(c(97.8, 60.9, 124)) %>% 
                              {desvDeInt(.[1], .[2], .[3], nIndiv)^2})$sigma2

# graficoDistr(cvAOmega(97.8),
#              p$mOmega_V2, # Se debe colocar un offset de 10 unid
#              cvAOmega(c(97.8, 60.9, 124)) %>%
#                {desvDeInt(.[1], .[2], .[3], nIndiv)},
#              sqrt(p$gOmega_V2),
#              interv = c(0.0, 10),
#              legpos = list(pos ='topright', inset=0.01))
# abline(v = cvAOmega(c(60.9, 124)), lty = 4, col='green')
# abline(v = cvAOmega(124), lty = 4, col='green')

# Variabilidad residual
p['mA'] = paramMoments(2.07+.4, desvDeInt(2.07, 1.70, 2.44, nIndiv) ^ 2)$mu
p['gA'] = paramMoments(2.07+.4, desvDeInt(2.07, 1.70, 2.44, nIndiv) ^ 2)$sigma2

# paramMoments(2.07+0.4, desvDeInt(2.07, 1.70, 2.44, nIndiv) ^ 2) %>%
#   {graficoDistr(2.07, .$mu, desvDeInt(2.07, 1.70, 2.44, nIndiv), sqrt(.$sigma2),
#               interv = c(0.0, 10),
#               legpos = list(pos ='topright', inset=0.01))}
# abline(v = 1.70, lty = 4, col='green')
# abline(v = 2.44, lty = 4, col='green')

p['mB'] = paramMoments(0.167, desvDeInt(0.167, 0.147, 0.184, nIndiv) ^ 2)$mu
p['gB'] = paramMoments(0.167, desvDeInt(0.167, 0.147, 0.184, nIndiv) ^ 2)$sigma2

# paramMoments(0.167, desvDeInt(0.167, 0.147, 0.184, nIndiv) ^ 2) %>%
#   {graficoDistr(0.167, .$mu, desvDeInt(0.167, 0.147, 0.184, nIndiv), sqrt(.$sigma2),
#               interv = c(0.0, 1),
#               legpos = list(pos ='topright', inset=0.01))}
# abline(v = 0.147, lty = 4, col='green')
# abline(v = 0.184, lty = 4, col='green')

# Covariables
p['lnClCr_pop'] = 4.53
p['sd_lnClCr']  = 0.69/1.35

# seq(0, 200, length.out = 1e3) %>%
#   plot(x = ., y = dlnorm(., p$lnClCr_pop, p$sd_lnClCr), type = 'l')
# # graficoDistr(exp(4.53), 4.53, exp(0.511), 0.511, c(0, 200))
# abline(v = exp(4.53-0.69), lty = 4, col='green')
# abline(v = exp(4.53+0.69), lty = 4, col='green')

p['FFM_pop']    = 59.1
p['omega_FFM']  = 5.7

# seq(40, 80, length.out = 1e2) %>%
#   plot(x = ., y = dnorm(., p$FFM_pop, p$omega_FFM), type = 'l')
# abline(v = 59.1 - 5.7, lty = 4, col='green')
# abline(v = 59.1 + 5.7, lty = 4, col='green')

p['ANC_li'] = 1e-6
p['ANC_ls'] = 1E5

# seq(0, 1.01E5, length.out = 1e2) %>%
#   plot(x = ., y = dunif(., p$ANC_li, p$ANC_ls), type = 'l')
# abline(v = 0, lty = 4, col='green')
# abline(v = 1e5, lty = 4, col='green')

#-------------------------------------------------------------------------------#
# 2. Definición de dosis y t. administración ----------------------
#-------------------------------------------------------------------------------#
y     <- list(name = 'Cc', time = c(2, 10, 12, 14, 25))
Cc    <- list(name = 'Cc', time=seq(1, 30, length.out =100))
ind   <- list(name = c('Cl', 'Q', 'V1', 'V2', 'a', 'b', 
                       'ANC', 'ClCr', 'FFM', 
                       'theta_1', 'theta_2', 'theta_3', 
                       'Q_pop', 'V1_pop', 'V2_pop', 
                       'omega_Cl', 'omega_V1', 'omega_V2'))

param <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)

#-------------------------------------------------------------------------------#
# 3. Simulación -------------------------------------------
#-------------------------------------------------------------------------------#

res <- simulx(model     = file.path(mdir), 
              parameter = unlist(p), 
              output    = list(y, ind),
              group     = list(
                size = c(nIndiv, nPob), 
                level = c('covariate', 'population')),
              treatment = param,
              settings  = list(seed = 123456))

#-------------------------------------------------------------------------------#
# 4. Gráficos y almacenamiento  -----------------------------------
#-------------------------------------------------------------------------------#
# Almacenar perfiles
res$y %>% 
  mutate(y = round(y, 2)) %>% 
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
    size = c(1e3),
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
  prctilemlx(res$Cc, number = 80, level = c(95), plot=T, color='orange') +
  geom_hline(yintercept = c(15, 20), linetype = 2) +
  theme(legend.position = 'none')

ggsave('perfilPlasmatico.pdf', gProf, 'pdf', wdir, 1, 6, 5)
