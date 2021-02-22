require(bayesplot)
require(shinystan)
require(patchwork)

source(file.path('src', '001_pre_101_modeloFinal.R'), encoding = 'UTF8')
source(file.path('src', '050_fun_funcion2Cptm.R'), encoding = 'UTF8')

modelName <- '101_modeloFinal'

#-------------------------------------------------------------------------------#
# Modelamiento bayesiano ----------------
#-------------------------------------------------------------------------------#

# Ejecución Stan
if(!file.exists(file.path('models', paste0(modelName, "Fit.Rsave")))){
  # Ejecución de pruebas
  test <- stan(
    file.path('src', paste0(modelName, '.stan')), # Modelo Stan
    data = stan_d, # Datos
    chains = 1, # Cadenas
    init = init,
    iter = 10 # Iteraciones
  )
  
  d <- Sys.time()
  nChains <- 4
  nPost <- 2000 ## Número de muestras de cadenas (2000)
  nBurn <- 500  ## Número de muestras Burn-In (500)
  nThin <- 10   ## Gráfico ACF no muestra correlación con 10 (1)
  
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
  
  save(fit, file = file.path('models', paste0(modelName, "Fit.Rsave")))
  
} else {
load(file = file.path('models', paste0(modelName, "Fit.Rsave")))
}

# Parámetros a evaluar
parameters <- as.matrix(fit) %>% 
  colnames() %>% 
  keep(~ str_detect(.x, 'CLHat|QHat|V1Hat|V2Hat|omega|rho|^a\\d$|^beta')) %>% 
  discard(~ str_detect(.x, 'rho\\[1,1\\]|rho\\[2,2\\]|rho\\[3,3\\]|rho\\[4,4\\]')) %>%
  discard(~ str_detect(.x, 'rho\\[1,2\\]|rho\\[1,3\\]|rho\\[1,4\\]')) %>%
  discard(~ str_detect(.x, 'rho\\[2,1\\]|rho\\[2,3\\]|rho\\[2,4\\]')) %>%
  discard(~ str_detect(.x, 'rho\\[3,1\\]|rho\\[3,2\\]|rho\\[4,1\\]|rho\\[4,2\\]')) %>% 
  discard(~ str_detect(.x, 'rho1'))

# Gráficos Diagnósticos

options(bayesplot.base_size = 12,
        bayesplot.base_family = "sans")
color_scheme_set(scheme = "viridis")
temaBayes <-
  theme(text = element_text(size = 12), axis.text = element_text(size = 12))

pdf(file = file.path('figures', paste0(modelName,"Plots%03d.pdf")),
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
  as.data.frame(fit, pars = c("cObsCond1", "cObsPred1", "cObsCond2", "cObsPred2")) %>%
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
    cObs1 = stan_d$cObs1,
    time1 = stan_d$time1,
    cObs2 = stan_d$cObs2,
    time2 = stan_d$time2,
    subjects1 = stan_d$subject1,
    subjects2 = stan_d$subject2
  )

gPRED1 <-
  predictions1 %>% 
  ggplot(aes(x=time1))+
  geom_ribbon(aes(ymin=lwr_q_cObsPred1, ymax=upr_q_cObsPred1), fill=alpha('blue', 0.2)) +
  geom_line(aes(y=median_cObsPred1), col='blue') +
  geom_ribbon(aes(ymin=lwr_q_cObsCond1, ymax=upr_q_cObsCond1), fill=alpha('green', 0.3)) +
  geom_line(aes(y=median_cObsCond1), col='green4') +
  geom_point(aes(y=cObs1))+
  facet_wrap(. ~ subjects1, ncol = 4, labeller = labeller(label_both)) + 
  theme_bw()+
  coord_cartesian(ylim = c(0, 100))+
  xlab('Tiempo (hr)') + ylab(bquote(C[PRED]~(mg/L)))

gPRED2 <-
  predictions1 %>% 
  ggplot(aes(x=time2))+
  geom_ribbon(aes(ymin=lwr_q_cObsPred2, ymax=upr_q_cObsPred2), fill=alpha('blue', 0.2)) +
  geom_line(aes(y=median_cObsPred2), col='blue') +
  geom_ribbon(aes(ymin=lwr_q_cObsCond2, ymax=upr_q_cObsCond2), fill=alpha('green', 0.3)) +
  geom_line(aes(y=median_cObsCond2), col='green4') +
  geom_point(aes(y=cObs2))+
  facet_wrap(. ~ subjects2, ncol = 4, labeller = labeller(label_both)) + 
  theme_bw()+
  coord_cartesian(ylim = c(0, 100))+
  xlab('Tiempo (hr)') + ylab(bquote(C[PRED]~(mg/L)))

ggsave(file.path('figures', paste0(modelName, 'CPRED_met_microb.pdf')),
       plot = gPRED1, 
       device = 'pdf', 
       width = 6, height = 7)

ggsave(file.path('figures', paste0(modelName, 'CPRED_met_quimio.pdf')),
       plot = gPRED2, 
       device = 'pdf', 
       width = 6, height = 7)
    
write_csv(summary(fit)$summary %>% as_tibble(rownames = 'P'), 
          file.path('reports', paste0(modelName, '_Results.txt')))

print(Sys.time() - d)
#-------------------------------------------------------------------------------#
# shinystan::launch_shinystan(fit)













