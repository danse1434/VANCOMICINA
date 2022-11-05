require(rstan)
require(tidyverse)
require(patchwork)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(20201209)


#-------------------------------------------------------------------------------#
# 1. Preprocesamiento de datos ----------
#-------------------------------------------------------------------------------#
data <- read_csv(file.path('data', 'data_TAD.csv'), na = '.', 
                 locale = locale(encoding = 'UTF-8'))

# > 1.1. Eventos de administración
nb_doses <- 5 # Dosis adicionales

dataDosificacion <- filter(data, EVID==1) %>% 
  mutate(ADDL = 5) %>% 
  rowwise() %>% 
  do(as.data.frame(.)[rep(1, .$ADDL),]) %>% 
  group_by(ID) %>%
  mutate(TAD = TAD + c(0:(ADDL[1]-1))*as.numeric(as.character(II)),
         ADDL = 0,
         II = 0) %>% 
  arrange(ID, TAD)

# > 1.2. Eventos de observación
seleccionUltimaDosis <- function(x, data) {
  data %>%              # Datos
    slice_tail() %>%    # Seleccionar última dosis
    select(ID, TAD)%>%  # Seleccionar ID y sujeto
    filter(ID == x) %>% # Filtrar por sujeto 
    `$`(TAD)            # Seleccionar TAD
}


dataObservacion <- data %>% 
  filter(EVID==0) %>%
  mutate(
    TAD = map2_dbl(ID, TAD, ~(seleccionUltimaDosis(.x, dataDosificacion) + .y))
  )

# Creación de dataframe con IDs y TAD a simular
dataObservacionSimulacion <- unique(dataObservacion$ID) %>% 
  expand.grid(ID = ., TAD = seq(48, 56, length.out = 1E2)) %>% 
  tibble() %>% 
  arrange(ID, TAD)

#-------------------------------------------------------------------------------#
# 2. Creación de inputs modelo Bayesiano -------------------
#-------------------------------------------------------------------------------#

eventoDosificacion <- list(
  nAdmEv     = dim(dataDosificacion)[1],
  startAdmEv = group_by(dataDosificacion, ID) %>%
    rownames_to_column() %>%
    slice_head() %>%
    `$`(rowname) %>%
    as.integer(),
  endAdmEv   = group_by(dataDosificacion, ID) %>%
    rownames_to_column() %>%
    slice_tail() %>%
    `$`(rowname) %>%
    as.integer(),
  timeAdmEv  = dataDosificacion$TAD,
  doseAdmEv  = as.numeric(as.character(dataDosificacion$AMT)),
  tinfAdmEv  = as.numeric(as.character(dataDosificacion$TINF)),
  logtWTKG   = filter(data, EVID == 1) %>% pull(WTKG) %>% {log(./62)},
  logtCLCR   = filter(data, EVID == 1) %>% pull(CLCRMLMIN) %>% {log(./144)}
)

eventoSimulacionObservacion <- dataObservacionSimulacion %>% 
  list(
    nobsSim    = dim(.)[1],
    time       = as.numeric(.$TAD),
    nSubjects  = length(unique(.$ID)),
    start      = group_by(., ID) %>%
      rownames_to_column() %>%
      slice_head() %>%
      `$`(rowname) %>%
      as.integer(),
    end        = group_by(., ID) %>%
      rownames_to_column() %>%
      slice_tail() %>%
      `$`(rowname) %>%
      as.integer(),    
    subjectID  = as.integer(.$ID)
  )

#-------------------------------------------------------------------------------#
# 3. Lectura de modelo  -------------------
#-------------------------------------------------------------------------------#
modelName <- '240_modelo_final_block2_prior_informativo'
run_file <- "run200"
fig_path <- file.path(run_file, "figures", "bayesiano")

load(file = file.path(run_file, str_glue("{modelName}_fit.Rsave")))

?stan

colnames(as.matrix(fit))


nSimulaciones <- length(as.matrix(fit)[, "thetaHat[1]"]) 


thetaHat <- rstan::extract(fit, c("thetaHat[1]", "thetaHat[2]", "thetaHat[3]", "thetaHat[4]"))

thetaHat <- thetaHat %>% 
  {c(.[[1]], .[[2]], .[[3]], .[[4]])} %>% 
  matrix(., nrow = nSimulaciones, ncol = 4, byrow = F)



omegaMat <- rstan::extract(fit, c("Omega[1,1]", "Omega[1,2]", "Omega[2,1]", "Omega[2,2]"))

omegaMat <- omegaMat %>% 
  {c(.[[1]], .[[2]], .[[3]], .[[4]])} %>% 
  matrix(., nrow = nSimulaciones, ncol = 4, byrow = F)


omegaQ <- rstan::extract(fit, c("omega_Q"))$omega_Q

beta_Cl_logtCLCR <- rstan::extract(fit, c("beta_Cl_logtCLCR"))$beta_Cl_logtCLCR









list(
  nSimulaciones = length(as.matrix(fit)[, "thetaHat[1]"])
)









# Lista general con parámetros
stan_d <- list()
stan_d <- append(stan_d, discard(eventoSimulacionObservacion, is.data.frame))
stan_d <- append(stan_d, eventoDosificacion)
stan_d





source(file.path('src', '241_preparacion_modelo_final_block2_prior_informativo.R'), encoding = 'UTF8')
source(file.path('src', '280_fun_funcion2Cptm.R'), encoding = 'UTF8')
















































#-------------------------------------------------------------------------------#
# Modelamiento bayesiano ----------------
#-------------------------------------------------------------------------------#

# Ejecución Stan
if(!file.exists(file.path(run_file, str_glue("{modelName}_fit.Rsave")))){
  # Ejecución de pruebas
  test <- stan(
    file.path('src', paste0(modelName, '.stan')), # Modelo Stan
    data = stan_d, # Datos
    chains = 1, # Cadenas
    init = init,
    iter = 100 # Iteraciones
  )
  
  d <- Sys.time()
  nChains <- 4
  nPost <- 2000  ## Número de muestras de cadenas (2000)
  nBurn <- 500  ## Número de muestras Burn-In (500)
  nThin <- 10    ## Gráfico ACF no muestra correlación (10)
  
  nIter <- (nPost + nBurn) * nThin
  nBurnin <- nBurn * nThin
  
  fit <- stan(file    = file.path('src', paste0(modelName, ".stan")),
              data    = stan_d,
              iter    = nIter,
              warmup  = nBurnin,
              thin    = nThin, 
              init    = init,
              chains  = nChains, 
              control = list(adapt_delta=0.95))
  
  print(Sys.time() - d)
  
  save(fit, file = file.path(run_file, str_glue("{modelName}_fit.Rsave")))
  
} else {
load(file = file.path(run_file, str_glue("{modelName}_fit.Rsave")))
}


# Parámetros a evaluar
parameters <- as.matrix(fit) %>% 
  colnames() %>% 
  keep(~ str_detect(.x, 'CLHat|QHat|V1Hat|V2Hat|omega|^b$|^beta|^rho'))

# Gráficos Diagnósticos

options(bayesplot.base_size = 12,
        bayesplot.base_family = "sans")
color_scheme_set(scheme = "viridis")
temaBayes <-
  theme(text = element_text(size = 12), axis.text = element_text(size = 12))

pdf(file = file.path(fig_path, paste0(modelName,"Plots%03d.pdf")),
    width = 8, height = 6, onefile = F)

# RHAT
rhats <- rhat(fit, parameters)
mcmc_rhat(rhats, size = 3) + yaxis_text() + temaBayes +
  theme(legend.position = c(0.7, 0.5))

# NEFF RATIO
ratios1 <- neff_ratio(fit, pars = parameters)
mcmc_neff(ratios1, size = 3) + yaxis_text() + temaBayes +
  theme(legend.position = c(0.8, 0.5))
# ACF
mcmc_acf(fit, pars = parameters, lags = 10)
# TRAZAS
mcmc_trace(fit, parameters)

mcmc_intervals(fit, parameters[grepl('Hat', parameters)]) + 
  mcmc_intervals(fit, parameters[!grepl('Hat', parameters)])

mcmc_dens(fit, parameters)

par(mfrow = c(1, 2))
mcmc_areas_ridges(fit, parameters[grepl('Hat', parameters)]) + 
  mcmc_areas_ridges(fit, parameters[!grepl('Hat', parameters)])

pairs(fit, pars = parameters[!grepl("rho", parameters)])
invisible(dev.off())

# Predicciones

predictions1 <-
  as.data.frame(fit, pars = c("cObsCond", "cObsPred")) %>%
  pivot_longer(cols=everything()) %>%
  mutate(value = ifelse(value == -99, NA, value)) %>%
  group_by(name) %>%
  summarize(lwr_q  = quantile(value, probs = 0.05, na.rm = TRUE),
            median = quantile(value, probs = 0.5, na.rm = TRUE),
            upr_q  = quantile(value, probs = 0.95, na.rm = TRUE)) %>% 
  mutate(name = str_replace(name, '\\[', '_'), 
         name = str_replace(name, '\\]', '')) %>% 
  separate(col='name', into=c('name', 'number'), sep='\\_', convert = TRUE) %>% 
  arrange(name, number) %>% 
  pivot_wider(id_cols = number, names_from=name, values_from=c(lwr_q, median, upr_q))%>% 
  mutate(
    OBS = stan_d$OBS,
    time = stan_d$time,
    subject = stan_d$subject
  )

gPRED1 <-
  predictions1 %>% 
  ggplot(aes(x=time))+
  geom_ribbon(aes(ymin=lwr_q_cObsPred, ymax=upr_q_cObsPred), fill=alpha('blue', 0.2)) +
  geom_line(aes(y=median_cObsPred), col='blue') +
  geom_ribbon(aes(ymin=lwr_q_cObsCond, ymax=upr_q_cObsCond), fill=alpha('green', 0.3)) +
  geom_line(aes(y=median_cObsCond), col='green4') +
  geom_point(aes(y=OBS))+
  facet_wrap(. ~ subject, ncol = 5, labeller = labeller(subject = \(x) str_glue("ID = {x}"))) + 
  theme_bw()+
  coord_cartesian(ylim = c(0, 65))+
  xlab('Tiempo (hr)') + ylab(bquote(C[PRED]~(mg/L)))

gPRED1

ggsave(file.path(fig_path, paste0(modelName, '_CPRED.pdf')),
       plot = gPRED1, 
       device = 'pdf', 
       width = 8, height = 8*0.65)

write_csv(summary(fit)$summary %>% as_tibble(rownames = 'P'), 
          file.path(fig_path, paste0(modelName, '_Results.txt')))


# Parámetros de dataframe
fun_summarise <- function(vector, na.rm = TRUE) {
  mn <- mean(vector, na.rm = na.rm)
  sd <- sd(vector, na.rm = na.rm)
  mi <- min(vector, na.rm = na.rm)
  q1 <- quantile(vector, probs = 0.25, na.rm = na.rm)
  md <- median(vector, na.rm = na.rm)
  ci <- quantile(vector, c(0.025, 0.975), na.rm = na.rm)
  q3 <- quantile(vector, probs = 0.75, na.rm = na.rm)
  ma <- max(vector, na.rm = na.rm)
  rse <- sd/mn
  
  tibble(mn, sd, rse, ci_li = ci[1], ci_ls = ci[2], mi, q1, md, q3, ma) %>% 
    mutate(across(everything(), ~signif(.x, 3)))
}

df_out <- as.data.frame(fit, pars = parameters) %>% 
  tibble() %>% 
  mutate(across(contains("omega"), ~ sqrt(exp(.x^2) - 1))) %>% 
  summarise(across(everything(), ~list(fun_summarise(.x)))) %>% 
  pivot_longer(everything(), names_to = "parameter") %>% 
  unnest_wider(value) %>% 
  setNames(c("Parameter", "Mean", "SD", "RSE", "ci_li", "ci_ls", 
             "Minimum", "Q1", "Median", "Q3", "Maximum")) %>% 
  filter(!Parameter %in% c("rho[1,1]", "rho[2,2]")) %>% 
  filter(!Parameter %in% c("rho[1,2]"))  



df_out["Parameter"] <- c("CL (L/h)", "V1 (L)", "Q (L/h)", "V2 (L)",
                         "theta CL ~ CLCR", 
                         "Omega CL (CV%)", "Omega V1 (CV%)",
                         "Omega Q (CV%)",
                         "rho (CL,V1)", 
                         "b (%)")

gt_out <- gt(df_out) %>% 
  fmt_percent(!matches("Parameter|RSE"), str_detect(Parameter, "Omega|^b")) %>% 
  fmt_percent(columns = c(RSE)) %>% 
  fmt_number(!matches("parameter"), n_sigfig = 3) %>% 
  cols_merge(matches("ci\\_"), pattern = "{1}, {2}") %>% 
  cols_label(ci_li = "Conf. Int. (95%)") %>% 
  tab_options(table.font.size = 12) %>% 
  tab_header(title = "Modelo bayesiano - Run 240", 
             subtitle = "Matriz IIV con CL, V1 y Q (sin correlación con Q), error proporcional")

gtsave(gt_out, file.path(fig_path, str_glue("{modelName}_parameters.html")))
gtsave(gt_out, file.path(fig_path, str_glue("{modelName}_parameters.docx")))


# shinystan::launch_shinystan(fit)



