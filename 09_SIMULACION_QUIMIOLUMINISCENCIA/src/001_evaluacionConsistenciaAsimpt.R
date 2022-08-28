##------------------------------------------------------------------------------#
## Nombre del Script: Evaluación de consistencia asintótica de estimadores -------
##
## Propósito del Script: Este procedimiento se realiza para encontrar el número 
## mínimo de configuraciones para estimar de forma apropiada a estadísticos de 
## exposición AUC, y Cmax.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  14-03-2021
## Fecha de modificación:  28-08-2022
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

source(file.path('src', '069_paquetesSimulacion.R'), encoding = 'UTF-8')
require(patchwork)

rdir <- file.path('model', 'run200.txt')

# Vector de parámetros
path_model_eval <- file.path("..", "07_FINAL", "50_Quimioluminiscencia")

p_DF <- read_csv(file.path(path_model_eval, "run200", 'populationParameters.txt'))
p <- setNames(p_DF$value, p_DF$parameter)
p['CLCRMLMIN'] = 120
# p['WTKG'] = 62

#-------------------------------------------------------------------------------#
# 1. Determinación de N.Dosis para Estado Estacionario -------------------
#-------------------------------------------------------------------------------#
# Administración
adm <- list(tfd=0, ii=12, amount=1000, tinf = 2)
nDosis <- 1:20
nDosis_ls <- vector(mode = 'list', length = length(nDosis))

N = 5e2
#  
for (i in 1:length(nDosis)) {
  out <- list(name = 'Cc', time = "steady.state", ngc = nDosis[i])
  res <- exposure(model = rdir, output = out, 
                  treatment = adm, parameter = p, 
                  group = list(size = N, level = 'individual'))
  nDosis_ls[[i]] <- res$Cc
}

nDosis_DF <- nDosis_ls %>%
  map_df( ~ .x, .id = 'ID') %>%
  tibble() %>%
  mutate(nDosis = nDosis[as.double(ID)],
         nDosis = factor(nDosis))

g1 <- nDosis_DF %>% 
  ggplot() + 
  geom_boxplot(aes(x = nDosis, y = auc)) + 
  xlab('N. Dosis') + ylab('AUC (mg*h/L)')

g2 <- nDosis_DF %>% 
  ggplot() + 
  geom_boxplot(aes(x = nDosis, y = cmax)) + 
  xlab('N. Dosis') + ylab(expression(C[max]~(mg/L)))

g3 <- nDosis_DF %>% 
  ggplot() + 
  geom_boxplot(aes(x = nDosis, y = cmin)) + 
  xlab('N. Dosis') + ylab(expression(C[min]~(mg/L)))

gT <- g1 + g2 + g3 + 
  plot_layout(ncol = 2) + 
  plot_annotation(
    title = 'N. de dosis para estado estacionario',
    subtitle = 'Simulaciones Vancomicina 1000mg q12 tinf 2h')

ggsave('001_Convergencia_Ndosis.pdf', gT, 'pdf', 'figures', 1, 
       8, 6)

# Con 4 dosis se puede encontrar el estado estacionario para 1g q12h de VAN
# 
#-------------------------------------------------------------------------------#
# 2. Determinación de puntos de tiempo MIN para convergencia ----------------
#-------------------------------------------------------------------------------#
# Administración
adm <- list(tfd=0, ii=12, amount=1000, tinf = 2)

nPuntos <- seq(10, 120, 10)
nPuntos_ls <- vector(mode = 'list', length = length(nPuntos))
N = 5e2
# 
for (i in 1:length(nPuntos)) {
  out <- list(name = 'Cc', time = "steady.state", ngc = 5, ntp = nPuntos[i])
  res <- exposure(model = rdir, output = out, 
                  treatment = adm, parameter = p, 
                  group = list(size = N, level = 'individual'))
  nPuntos_ls[[i]] <- res$Cc
}

nPuntos_DF <- nPuntos_ls %>%
  map_df( ~ .x, .id = 'ID') %>%
  tibble() %>%
  mutate(nPuntos = nPuntos[as.double(ID)],
         nPuntos = factor(nPuntos))

gP1 <- nPuntos_DF %>% 
  ggplot() + 
  geom_boxplot(aes(x = nPuntos, y = auc)) + 
  xlab('N. puntos de C vs t') + ylab('AUC (mg*h/L)')

gP2 <- nPuntos_DF %>% 
  ggplot() + 
  geom_boxplot(aes(x = nPuntos, y = cmax)) + 
  xlab('N. puntos de C vs t') + ylab(expression(C[max]~(mg/L)))

gP3 <- nPuntos_DF %>% 
  ggplot() + 
  geom_boxplot(aes(x = nPuntos, y = cmin)) + 
  xlab('N. puntos de C vs t') + ylab(expression(C[min]~(mg/L)))

gPT <- gP1 + gP2 + gP3 + 
  plot_layout(ncol = 2) + 
  plot_annotation(
    title = 'N. de puntos de perfil para estado estacionario',
    subtitle = 'Simulaciones Vancomicina 1000mg q12 tinf 2h')

ggsave('002_Convergencia_NpuntosPerfil.pdf', gPT, 'pdf', 'figures', 1, 
       8, 6)

# Se determina que 30 puntos puede ser suficiente para estimar Cmax

#-------------------------------------------------------------------------------#
# 3. N. de individuos para estimar estadístico --------------------------
#-------------------------------------------------------------------------------#
# Administración
adm <- list(tfd=0, ii=12, amount=1000, tinf = 2)
out <- list(name = 'Cc', time = "steady.state", ngc = 4, ntp = 30)

nIndiv <- c(seq(1e1, 1e2, by = 2e2),
            seq(2e2, 1e3, by = 2e2),
            seq(2e3, 5e3, by = 2e3))
# seq(2e4, 1e5, by = 2e4)


# 

d <- Sys.time()

res <- exposure(model = rdir, output = out, 
                treatment = adm, parameter = p, 
                group = list(size = 5e3, level = 'individual'))

print(Sys.time() - d)

resDF <- res$Cc %>% 
  select(id, auc, cmax, cmin) %>% 
  mutate(auc_i = ifelse(auc > 100, 1, 0)) %>% 
  tibble()

nIndiv_ls <- tibble(nIndiv = nIndiv)

for (i in 1:length(nIndiv)) {
  auc_ls <- vector()
  auc_i_ls <- vector()
  cmax_ls <- vector()
  cmin_ls <- vector()
  
  for (j in 1:100) {
    intDF <- resDF[sample(nrow(resDF), nIndiv[1], replace = T), ] %>%
      summarise(across(c(auc, auc_i, cmax, cmin), mean))
    
    auc_ls <- append(auc_ls, pull(intDF, 'auc'))
    auc_i_ls <- append(auc_i_ls, pull(intDF, 'auc_i'))
    cmax_ls <- append(cmax_ls, pull(intDF, 'cmax'))
    cmin_ls <- append(cmin_ls, pull(intDF, 'cmin'))
  }
  
  nIndiv_ls[[i, 'data']] <-
    list(data.frame(
      auc = auc_ls,
      auc_i = auc_i_ls,
      cmin = cmin_ls,
      cmax = cmax_ls
    ))
}

nIndiv_DF <- nIndiv_ls %>%
  unnest(data) %>%
  mutate(
    nIndiv1 = formatC(nIndiv, format = 'e', digits = 0),
    nIndiv1 = factor(nIndiv1),
    nIndiv1 = fct_reorder(nIndiv1, nIndiv)
  )

gI1 <- ggplot(nIndiv_DF) + 
  geom_boxplot(aes(x = nIndiv1, y = auc)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  xlab('N. de individuos simulados') + ylab('AUC (mg*h/L)')
  
gI2 <- ggplot(nIndiv_DF) + 
  geom_boxplot(aes(x = nIndiv1, y = cmax)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  xlab('N. de individuos simulados') + ylab(expression(C[max]~(mg/L)))

gI3 <- ggplot(nIndiv_DF) + 
  geom_boxplot(aes(x = nIndiv1, y = cmin)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  xlab('N. de individuos simulados') + ylab(expression(C[min]~(mg/L)))

gI4 <- ggplot(nIndiv_DF) + 
  geom_boxplot(aes(x = nIndiv1, y = auc_i)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  xlab('N. de individuos simulados') + ylab('Indicador PK-PD')

gIT <- gI1 + gI2 + gI3 + gI4 + 
  plot_layout(ncol = 2) + 
  plot_annotation(
    title = 'N. de individuos simulados para SS',
    subtitle = 'Simulaciones Vancomicina 1000mg q12 tinf 2h')

ggsave('003_Convergencia_NIndividuos.pdf', gIT, 'pdf', 'figures', 1, 
       8, 6)
