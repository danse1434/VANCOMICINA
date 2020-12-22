##------------------------------------------------------------------------------#
## Nombre del Script: Comparación de resultados de análisis bivariado de LL con
## resultado de muestreo bayesiano.
##  
## Propósito del Script: Se realiza una comparación de los resultados del análisis 
## del perfil de verosimilitud en dos dimensiones con los resultados del análisis 
## bayesiano para el modelo base.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  17-12-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#

source('./src/002_prepro_102_modeltwoCptmDiagProp.R', encoding = 'UTF8')
source('./src/051_fun_funcion2Cptm.R', encoding = 'UTF8')
source('./src/052_fun_funcionesVerosimilitud.R', encoding = 'UTF8')

modelName <- '104_modeltwoCptmDiagProp_errResNor'
load(file = file.path('models', paste0(modelName, "Fit.Rsave")))

#-------------------------------------------------------------------------------#
# 1. Lectura de gráficos con perfiles LL -------
#-------------------------------------------------------------------------------#
wd <- getwd()
setwd(file.path('..', '03_mapeoFuncionLL'))
source2(file.path('src', '11_analisis_resultados_2D.R'), 1,84)
setwd(wd)

#-------------------------------------------------------------------------------#
# 2. Crear gráfico con inserto ----------
#-------------------------------------------------------------------------------#

fit_mcmc <- As.mcmc.list(fit)

ls_opt_1 <- list()
ls_opt_2 <- list(left = 0.6, bottom = 0.6, right = 1, top = 1, clip = TRUE)

blank_theme <- theme_minimal() + theme(legend.position = 'none', 
                                       axis.title = element_blank(), 
                                       axis.text = element_blank(), 
                                       axis.ticks = element_blank(),
                                       plot.background = element_rect(fill=NA, colour = NA))

lista_2D <- list()

#' Crear una gráfica con inserto y la distribución de muestras 
#' obtenidas mediante modelamiento bayesiano
#'
#' @param grafico gráfico original con el mapeo bivariado de 
#'        función de verosimilitud.
#' @param datos datos con distribución de muestras obtenidas 
#'        por estimación bayesiana.
#' @param par1 parámetro a graficar 1
#' @param par2 parámetro a graficar 2 
#' @param xlim límite de eje X
#' @param ylim límite de eje Y
#'
#' @return
#' gráfico de tipo ggplot2
#' 
#' @export
#'
#' @examples
#' 
#' 
modificarInserto <- function(grafico, datos, par1, par2, xlim, ylim) {
  gBase <- grafico + 
    
      geom_density2d(
        data = getEstimParamBayes(datos, par1, par2),
        mapping = aes(x = !!ensym(par1), y = !!ensym(par2)),
        bins = 10, color = "#A19412", size=0.1, inherit.aes = FALSE
      )
  
  gInserto <- gBase + 
    blank_theme +
    coord_cartesian(xlim=xlim, ylim=ylim)  
  
  gTotal <- (gBase + coord_cartesian(expand = FALSE)) + 
    eval(expr(inset_element(gInserto, !!!ls_opt_2)))
  
  return('Modelo'= gTotal)
}


# > 2.1. Generación de gráficos -------------------------------------------

lista_2D[[1]] <- modificarInserto(grafico_2D_LS[[1]], fit_mcmc, 
                 'CLHat', 'V1Hat', 
                 xlim=c(7,13), ylim=c(38,55))


lista_2D[[2]] <- modificarInserto(grafico_2D_LS[[2]], fit_mcmc, 
                 'V1Hat', 'V2Hat', 
                 xlim=c(40,55), ylim=c(40,70))

lista_2D[[3]] <- modificarInserto(grafico_2D_LS[[3]], fit_mcmc, 
                 'CLHat', 'QHat', 
                 xlim=c(6,15), ylim=c(6,18))

lista_2D[[4]] <- modificarInserto(grafico_2D_LS[[4]], fit_mcmc, 
                 'QHat', 'V2Hat', 
                 xlim=c(8,20), ylim=c(40,100))

# Creación de composición de gráficos
(lista_2D %>% 
  reduce(., `+`)+
  plot_layout(ncol=2)) %>% 
  ggsave('figures/110_comparacion_Bayesiana_Mas_Verosimilitud.pdf', plot = ., 'pdf', 
         width = 8, height = 6)


# > 2.2. Mapeo bivariado Omegas
# Crear gráfico para comparación de Omegas

graficoOmegas <- ggplot() +
stat_density_2d(
  data = getEstimParamBayes(fit_mcmc, 'omega[3]', 'omega[4]'),
  mapping = aes(x = `omega[3]`, y = `omega[4]`, fill=stat(level)), geom = 'polygon',
  bins = 10, color = "black", size=0.1, inherit.aes = FALSE
) + 
  geom_point(
    data=df_omega_V1_omega_V2 %>% slice_min(order_by =  LL1),
    mapping = aes(x=omega_V1, y=omega_V2), color='red', shape='+'
    ) +
 geom_point(
   data = popParam, aes(omega_V1, omega_V2), color='green', shape='*'
 ) 

ggsave('111_comparacion_Bayesiana_Omegas_LL.pdf', graficoOmegas, 'pdf', 
       'figures', 1, 8, 6)  


