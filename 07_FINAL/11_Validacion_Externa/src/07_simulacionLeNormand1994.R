##------------------------------------------------------------------------------#
## Nombre del Script: Simulación de muestras de estudio de Le Normand Y et al
## 1994 en pacientes neutropénicos
##  
## Propósito del Script: Para este estudio se tienen los parámetros individuales,
## pero no se cuenta con una estimación de la incertidumbre.
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
wdir <- file.path('07_LeNormand')
mdir <- file.path(wdir, '07_LeNormand.mlxtran')
nIndiv <- 10 # N. pacientes en estudio
nPob <- 50

#-------------------------------------------------------------------------------#
# 1. Especificación de parámetros   ---------------------
#-------------------------------------------------------------------------------#
Cl_ls <- c(216L, 157L, 270L, 127L, 127L, 163L, 158L, 106L, 106L, 154L) * 6/100
V_ls <- c(26.3, 36.7, 29.8, 40, 5.6, 28.6, 22.1, 9.1, 16.9, 14.3)
ClCr_ls <- c(163L, 114L, 152L, 87L, 112L, 157L, 191L, 102L, 190L, 144L)

# plot(ClCr_ls ~ Cl_ls, type='p', ylim=c(80,266), xlim=c(4.8,16))
# abline(0, 100/6)

p <- list()

for (i in 1:10) {
  p[[i]] <- list(parameter = c(Cl = Cl_ls[i], V = V_ls[i]))
}
#-------------------------------------------------------------------------------#
# 2. Definición de dosis y t. administración ----------------------
#-------------------------------------------------------------------------------#
# Cc    <- list(name = 'Cc', time=seq(1, 30, length.out =100))
Cc    <- list(name = 'Cc', time = c(2, 10, 12, 14, 25))
ind   <- list(name = c('Cl', 'V'))
param <- list(time = seq(0, 8 * 6, by = 8), amount = 1800/3, tinf = 1)

# {
#   pdf(file.path(wdir, 'distribuciones.pdf'), width = 6, height = 4)
#   par(mfrow = c(1, 2))
#   
#   # mtext("Parámetros reportados Haeseker 2014", cex=1, col='black', adj = .5, padj = .9)
#   paramMoments(105.4, 62.3^2) %>%
#   {rlnorm(1e3, .$mu, sqrt(.$sigma2))} %>%
#     hist(border = 'blue', col = 'lightblue1',
#          main = NULL, xlab = 'ClCr (mL/min)', ylab = 'Frecuencia')
#   abline(v = 105.4, lty = 2, col='black')
#   #
#   paramMoments(986.1, 159.8^2) %>%
#   {rlnorm(1e3, .$mu, sqrt(.$sigma2))} %>%
#     hist(border = 'red', col = 'lightpink1',
#          main = NULL, xlab = 'Dosis D (mg)', ylab = 'Frecuencia')
#   abline(v = 986.1, lty = 2, col='black')
#   #
#   dev.off()
# }

#-------------------------------------------------------------------------------#
# 3. Simulación -------------------------------------------
#-------------------------------------------------------------------------------#

res <- simulx(model     = file.path(mdir),
              output    = list(Cc, ind),
              group     = list(p),
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
# res$parameter %>%
#   mutate(across(!contains('id'), ~ round(.x, 4))) %>%
#   write_csv(., file.path(wdir, 'parameters.csv'))

#-------------------------------------------------------------------------------#
# 5. Perfil promedio --------------------------------------------
#-------------------------------------------------------------------------------#
Cc <- list(name = 'Cc', time=seq(0, 12, length.out =100) + (12 * 6))
param <- list(time = seq(0, 12 * 11, by = 12), amount = 1000, tinf = 2)

res <- simulx(
  model     = file.path(mdir),
  output    = list(Cc),
  group     = p,
  treatment = param,
  settings  = list(seed = 123456, digits = 3)
)
# res$y %>% ggplot(aes(x=time, y=y, color = id)) + geom_line()
prctilemlx(res$Cc, number = 2, level = c(95), plot=FALSE) %>% 
  `$`('y') %>%
  as_tibble(.name_repair = 'universal') %>%
  mutate(across(!matches('time|group'), ~ round(.x, 1))) %>% 
  write_csv(., file.path(wdir, 'perfilPlasmatico.csv'))

gProf <- prctilemlx(res$Cc, level = c(95), plot=T, group = 'id', 
                    facet = FALSE, color = 'purple') +
  geom_hline(yintercept = c(61.3), linetype = 2) +
  theme(legend.position = 'none')

ggsave('perfilPlasmatico.pdf', gProf, 'pdf', wdir, 1, 6, 5)
