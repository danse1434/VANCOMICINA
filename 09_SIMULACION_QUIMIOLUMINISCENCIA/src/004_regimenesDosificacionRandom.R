
require(data.table)
require(progress)

source(file.path('.', 'src', '069_paquetesSimulacion.R'), encoding = 'UTF-8')
source(file.path('.', 'src', '080_fun_ConversionLog.R'), encoding = 'UTF-8')
source(file.path('.', 'src', '082_fun_Simulacion.R'), encoding = 'UTF-8')
source(file.path('.', 'src', '002_creacionDataFrame.R'), encoding = 'UTF-8')

setwd("C:/Users/Daniel/OneDrive/Documents/(Proyecto)_Estudio_PKPD/VANCOMICINA/09_SIMULACION_QUIMIOLUMINISCENCIA")

set.seed(123044)

# N. regímenes
Nreg = 3000
# N. individuos a simular por régimen
N <- 1000

regimenes <- tibble(
  DD = runif(Nreg, 1000, 4000),
  II = runif(Nreg, 2, 24),
  CLCR = runif(Nreg, 80, 150)
) %>% mutate(tinf = map_dbl(II, ~ runif(1, 1.5, .x)))


listaIndicadores <- list()

# Directorio de modelo
rdir <- file.path('model', 'run200.txt')

path_model_eval <- file.path("..", "07_FINAL", "50_Quimioluminiscencia")
# Carga de parámetros del modelo (sin incertidumbre)
p_DF <- read_csv(file.path(path_model_eval, "run200", 'populationParameters.txt'))
p <- setNames(p_DF$value, p_DF$parameter)
# Liberar memoria de p_DF
rm(p_DF)

# Seguimiento de tiempo
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent) eta: (:eta)",
                       total = dim(regimenes)[1])

Sys.time() -> ptm

for (k in 1:dim(regimenes)[1]) {
  
  pb$tick()
  
  # Simulación de parámetros y covariables para cada individuo
  p_DataFrame <- creacionDF(p, N) %>% 
    add_column(CLCRMLMIN = regimenes$CLCR[k])
  
  out <- list(name = 'Cc', time = seq(72, 96, length = 30))

  par <- as.vector(as.data.frame(p_DataFrame)[1, ])
  
  dP_ls <- list()

  for (i in 1:N) {
    # 3.1.1. Selección de parámetros en la fila de cada individuo
    par <- as.vector(as.data.frame(p_DataFrame)[i, ])
    # 3.1.2. Creación de régimen de dosificación para cada individuo
    amountLS = listaTratamiento(regimenes$DD[k],
                                regimenes$II[k],
                                regimenes$tinf[k],
                                16)
    # 3.1.3. Crear para un elemento en la lista de simulación
    dP_ls[[i]] <- list(
      parameter = par,
      treatment = list(amountLS),
      size      = 1,
      level     = 'individual'
    )
  }
  
  res <- exposure(model = rdir,
                  output = out,
                  group = dP_ls)
  
  exposureDF <- as.data.table(res$Cc) 
  
  # Liberación de memoria
  rm(res, dP_ls)
  
  exposureDF[, `:=`(
    t1   = NULL,
    t2   = NULL,
    step = NULL,
    tmin = NULL,
    tmax = NULL,
    auc  = round(auc, 3),
    cmax = NULL,
    cmin = round(cmin, 3)
  )] 
  
  listaRes <- data.frame(
    'AUC_target'  = sum(exposureDF$auc >= 400 & exposureDF$auc <= 600),
    'cmin_target' = sum(exposureDF$cmin >= 15 & exposureDF$cmin <= 20)
  )
  
  listaIndicadores <- append(listaIndicadores, list(listaRes))
  rm(exposureDF)
}

d <- Sys.time() - ptm
d

regimenesDF <- regimenes %>%
  add_column(ind = listaIndicadores) %>%
  unnest(ind) %>%
  mutate(AUC_target = AUC_target / N,
         cmin_target = cmin_target / N)

fwrite(regimenesDF, file.path('.', 'results', 'evaluacionRegimenes.csv'))
