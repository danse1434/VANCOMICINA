##------------------------------------------------------------------------------#
## Nombre del Script: Simulación de muestras de estudio de Al Khofide H et al 
## 2016 que comparó individuos con cáncer frente a sin cáncer.
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
wdir <- file.path('06_AlKhofide')
mdir <- file.path(wdir, '06_AlKhofide.mlxtran')
nIndiv <- 31 # N. pacientes en estudio
nPob <- 50

#-------------------------------------------------------------------------------#
# 1. Especificación de parámetros   ---------------------
#-------------------------------------------------------------------------------#
p <- list()

# Poblacionales
# Parámetros Estructurales
p['mCl'] = paramMoments(6.61, 2.52^2)$mu      # L/h
p['gCl'] = paramMoments(6.61, 2.52^2)$sigma2  # L/h

# graficoDistr(6.61, p$mCl, 2.52, sqrt(p$gCl), c(0, 18))
# abline(v = 6.61, lty = 4, col='green4')

p['mV'] = paramMoments(70+20, 45^2)$mu       # L
p['gV'] = paramMoments(70+20, 45^2)$sigma2   # L
 
# graficoDistr(70, p$mV, 45, sqrt(p$gV), c(0, 200))
# abline(v = 70, lty = 4, col='green4')

#-------------------------------------------------------------------------------#
# 2. Definición de dosis y t. administración ----------------------
#-------------------------------------------------------------------------------#
# Cc    <- list(name = 'Cc', time=seq(1, 30, length.out =100))
Cc    <- list(name = 'Cc', time = c(2, 10, 12, 14, 25))
ind   <- list(name = c('Cl', 'V'))
param <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)

{
  pdf(file.path(wdir, 'distribuciones.pdf'), width = 6, height = 4)
  par(mfrow = c(1, 2))

  # mtext("Parámetros reportados Haeseker 2014", cex=1, col='black', adj = .5, padj = .9)
  paramMoments(105.4, 62.3^2) %>%
  {rlnorm(1e3, .$mu, sqrt(.$sigma2))} %>%
    hist(border = 'blue', col = 'lightblue1',
         main = NULL, xlab = 'ClCr (mL/min)', ylab = 'Frecuencia')
  abline(v = 105.4, lty = 2, col='black')
  #
  paramMoments(986.1, 159.8^2) %>%
  {rlnorm(1e3, .$mu, sqrt(.$sigma2))} %>%
    hist(border = 'red', col = 'lightpink1',
         main = NULL, xlab = 'Dosis D (mg)', ylab = 'Frecuencia')
  abline(v = 986.1, lty = 2, col='black')
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

gProf <- prctilemlx(res$Cc, number = 80, level = c(95), plot=T, color = 'red') +
  geom_hline(yintercept = c(8.5, 27.2), linetype = 2) +
  theme(legend.position = 'none')

ggsave('perfilPlasmatico.pdf', gProf, 'pdf', wdir, 1, 6, 5)
