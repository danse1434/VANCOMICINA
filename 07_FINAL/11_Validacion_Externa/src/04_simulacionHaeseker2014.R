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
wdir <- file.path('04_Haeseker')
mdir <- file.path(wdir, '04_Haeseker.mlxtran')
nIndiv <- 56 # N. pacientes en estudio

#-------------------------------------------------------------------------------#
# 1. Especificación de parámetros   ---------------------
#-------------------------------------------------------------------------------#
p <- list()

# Poblacionales
# Parámetros Estructurales
p['mCl'] = paramMoments(4.02, 1.56^2)$mu      # L/h
p['gCl'] = paramMoments(4.02, 1.56^2)$sigma2  # L/h

# graficoDistr(4.02, p$mCl, 1.56, sqrt(p$gCl), c(0, 10))
# abline(v = 4.02, lty = 4, col='green4')

p['mV'] = paramMoments(62+10, 32^2)$mu       # L
p['gV'] = paramMoments(62+10, 32^2)$sigma2   # L

# graficoDistr(62, p$mV, 32, sqrt(p$gV), c(0, 200))
# abline(v = 62, lty = 4, col='green4')

#-------------------------------------------------------------------------------#
# 2. Definición de dosis y t. administración ----------------------
#-------------------------------------------------------------------------------#
# Cc    <- list(name = 'Cc', time=seq(1, 30, length.out =100))
Cc    <- list(name = 'Cc', time = c(2, 10, 12, 14, 25))
ind   <- list(name = c('Cl', 'V'))
param <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)

{
  pdf(file.path(wdir, 'distribuciones.pdf'), width = 8, height = 6)
  par(mfrow = c(1, 2))
  
  # mtext("Parámetros reportados Haeseker 2014", cex=1, col='black', adj = .5, padj = .9)
  paramMoments(113, 57^2) %>% 
    {rlnorm(1e3, .$mu, sqrt(.$sigma2))} %>% 
    hist(border = 'blue', col = 'lightblue1',
         main = NULL, xlab = 'ClCr (mL/min)', ylab = 'Frecuencia')
  # 
  paramMoments(2017, 719^2) %>% 
    {rlnorm(1e3, .$mu, sqrt(.$sigma2))} %>% 
    hist(border = 'red', col = 'lightpink1',
         main = NULL, xlab = 'Dosis D (mg)', ylab = 'Frecuencia')
  #
  dev.off()
}

#-------------------------------------------------------------------------------#
# 3. Simulación -------------------------------------------
#-------------------------------------------------------------------------------#

res <- simulx(model     = file.path(mdir), 
              parameter = unlist(p), 
              output    = list(Cc, ind),
              group     = list(
                size = c(nIndiv * nPob),
                level = c('population')),
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

gProf <- prctilemlx(res$Cc, number = 80, level = c(95), plot=T, color = 'green2') +
  theme(legend.position = 'none')+
  geom_hline(yintercept = c(13, 21, 49), linetype = 2) +
  theme(legend.position = 'none')
ggsave('perfilPlasmatico.pdf', gProf, 'pdf', wdir, 1, 6, 5)
