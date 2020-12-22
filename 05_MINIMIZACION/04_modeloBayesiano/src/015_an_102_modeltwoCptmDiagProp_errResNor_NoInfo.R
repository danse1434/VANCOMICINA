source('./src/005_prepro_105_modeltwoCptmDiagPropCov_errResNor_NoInfo.R', encoding = 'UTF8')
source('./src/051_fun_funcion2Cptm.R', encoding = 'UTF8')

modelName <- '105_modeltwoCptmDiagProp_errResNor_NoInfo'

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
  nPost <- 2000 ## Número de muestras de cadenas
  nBurn <- 500  ## Número de muestras Burn-In
  nThin <- 10   ## Gráfico ACF no muestra correlación con 10
  
  nIter <- (nPost + nBurn) * nThin
  nBurnin <- nBurn * nThin
  
  fit <- stan(file = file.path('src', paste0(modelName, ".stan")),
              data = stan_d,
              iter = nIter,
              warmup = nBurnin,
              thin = nThin, 
              init = init,
              chains = nChains)
  
  d <- d- Sys.time(); print(d)
  
  save(fit, file = file.path('models', paste0(modelName, "Fit.Rsave")))
  
} else {
load(file = file.path('models', paste0(modelName, "Fit.Rsave")))
}

# Parámetros a evaluar
parameters <- as.matrix(fit) %>% 
  colnames() %>% 
  keep(~ str_detect(.x, 'CLHat|QHat|V1Hat|V2Hat|omega|rho|^b$')) %>% 
  discard(~ str_detect(.x, 'rho\\[1,1\\]|rho\\[2,2\\]|rho\\[3,3\\]|rho\\[4,4\\]'))%>% 
  discard(~ str_detect(.x, 'rho\\[2,1\\]|rho\\[3,1\\]|rho\\[4,1\\]|rho\\[3,2\\]|rho\\[4,2\\]|rho\\[4,3\\]'))

# Gráficos Diagnósticos
require(bayesplot)
options(bayesplot.base_size = 12,
        bayesplot.base_family = "sans")
color_scheme_set(scheme = "brightblue")
temaBayes <- theme(text = element_text(size = 12), axis.text = element_text(size = 12))

pdf(file = file.path('figures', paste0(modelName,"Plots%03d.pdf")),
    width = 6, height = 6, onefile = F)

rhats <- rhat(fit, parameters)
mcmc_rhat(rhats)  + yaxis_text() + temaBayes

ratios1 <- neff_ratio(fit, pars = parameters)
mcmc_neff(ratios1) + yaxis_text() + temaBayes
mcmc_intervals(fit, parameters)

mcmc_dens(fit, parameters)
mcmc_areas_ridges(fit, parameters)
pairs(fit, pars = parameters[!grepl("rho", parameters)])
mcmc_acf_bar(fit, pars = parameters)

dev.off()

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
    cObs = stan_d$cObs,
    time = stan_d$time,
    subjects = stan_d$subject
  )


gPRED <- predictions1 %>% 
  ggplot(aes(x=time))+
  geom_ribbon(aes(ymin=lwr_q_cObsPred, ymax=upr_q_cObsPred), fill=alpha('blue', 0.2)) +
  geom_line(aes(y=median_cObsPred), col='blue') +
  geom_ribbon(aes(ymin=lwr_q_cObsCond, ymax=upr_q_cObsCond), fill=alpha('green', 0.3)) +
  geom_line(aes(y=median_cObsCond), col='green4') +
  geom_point(aes(y=cObs))+
  facet_wrap(. ~ subjects, ncol = 4, labeller = labeller(label_both)) + 
  theme_bw()+
  xlab('Tiempo (hr)') + ylab(bquote(C[PRED]~(mg/L)))

ggsave(file.path('figures', paste0(modelName, 'CPRED.pdf')),
       plot = gPRED, 
       device = 'pdf', 
       width = 6, height = 7)
    

write_csv(summary(fit)$summary %>% as_tibble(rownames = 'P'), 
          file.path('reports', paste0(modelName, '_Results.txt')))

#-------------------------------------------------------------------------------#

require(shinystan)

shinystan::launch_shinystan(fit)













