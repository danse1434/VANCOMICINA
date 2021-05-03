wd1 <- getwd()

setwd(
  file.path(
    'F:',
    'Documentos',
    '(Proyecto)_Estudio_PKPD',
    'VANCOMICINA',
    '07_FINAL',
    '06_Minimizacion',
    '03_mapeoFuncionLL'
  )
)


#'-------------------------------------------------------------------------------
# 1. Lectura de datos univariados ------------------
#'-------------------------------------------------------------------------------
source("./src/10_analisis_resultados_1D.R", encoding = 'UTF8')

G_ll_1 + G_ll_2 + G_ll_3

#'-------------------------------------------------------------------------------
# 2. Lectura de datos bivariados ------------------
#'-------------------------------------------------------------------------------
setwd(wd1)
source("./src/020_comparacionML.R", encoding = 'UTF8')


layoutp <- "
AAABBBCCC
AAABBBCCC
DDDDEEEE#
DDDDEEEE#
DDDDEEEE#
"


G_total <- G_ll_1 + 
  (G_ll_2 + xlab(expression(theta[1] ~ Cl-logtClCr))) + 
  (G_ll_3 + xlab(expression(rho ~ V[1] - V[2]))) + 
  (lista_2D[[1]]) + (lista_2D[[2]]) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(design = layoutp, widths = 1)



ggsave("./mosaicoVerosimilitud.pdf", G_total, 'pdf', 'figures', 1, 8, 6)

