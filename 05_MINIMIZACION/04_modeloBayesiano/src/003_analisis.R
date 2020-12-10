
# Algoritmo - Intento 2 ---------------------------------------------------

# En este script se aplica un algoritmo corregido que sólo funciona con 
# tasas.

require(tidyverse)
require(mlxR)
source('./src/051_fun_funcion2Cptm.R', encoding = 'UTF8')


#-------------------------------------------------------------------------------#

amt <- list(time=8*(seq(length.out= 4L)-1), 
            dose=rep(2000, 4L),
            tinf=rep(2, 4L))
parameters = c(Cl=10, Q=10, V=45, V2=50)

df <- data.frame('time' = seq(0, max(amt$time)+8, length.out = 1000)) %>% as_tibble()

df <- df %>%
  mutate(pred = map_dbl(
    time,
    ~ funcion2Cptm(
      parameters = parameters,
      amt = amt,
      cmt = 1,
      dt = .x
    )
  ))

# microbenchmark::microbenchmark(funcion2Cptm(parameters, amt, 1, 88))

#-------------------------------------------------------------------------------#
adm <- list(time=amt$time, amount=amt$dose, tinf=amt$tinf)
Cc  <- list(name='Cc', time=seq(0, max(amt$time)+8, length.out = 1000))

mod1 <- mlxR::inlineModel(
  "
[LONGITUDINAL]
input = {V, Cl, Q, V2}

PK:
compartment(cmt=1, amount=Ac, volume=V)
peripheral(k12=Q/V, k21=Q/V2)
iv(cmt=1, adm=1)
elimination(cmt=1, Cl)
Cc = Ac/V
"
)

res <- simulx(model=mod1, 
              parameter=parameters, 
              output=Cc, 
              treatment=adm, settings = list('load.design'=TRUE))

# Adición de datos
df$P_correcto = res$Cc$Cc

g <- df %>% 
  pivot_longer(cols = c('pred', 'P_correcto')) %>% 
  mutate(name = ifelse(name=='pred', 'Función UDF', 'mlxR')) %>%
  ggplot(aes(x=time)) +
  geom_line(aes(y=value), col='blue') +
  theme_bw() + 
  facet_wrap(. ~ name, ncol=2) +
  xlab('Tiempo (h)') + ylab(bquote(Cp~(mg/L))) +
  coord_cartesian(xlim = c(0,32), ylim=c(0,40))

ggsave(filename = 'figures/001_Prueba_Funcion_2CPTM_R.pdf', g, device= 'pdf',width = 8, height = 4)



df %>% 
  ggplot(aes(x=p, y=P_correcto))+
  geom_point()
