source('./src/000_preprocesamiento_Datos.R', encoding = 'UTF8')
source('./src/051_fun_funcion2Cptm.R', encoding = 'UTF8')

#-------------------------------------------------------------------------------#
# 3. Modelamiento bayesiano ----------------
#-------------------------------------------------------------------------------#
# Ejecución de pruebas
test = rstan::stan(
  "src/100_mod_2CPTM_Inf_MM_stan.stan", # Especificación Modelo Stan
  data = stan_d,                        # Especificación Datos
  # pars = params_monitor,       # Parámetros a monitorizar
  chains = 1,                           # Cadenas
  init = init,
  iter = 10                             # Iteraciones
)


