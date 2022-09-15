
# Parámetros a evaluar
parameters <- as.matrix(fit) %>% 
  colnames() %>% 
  keep(~ str_detect(.x, 'CLHat|QHat|V1Hat|V2Hat|Omega|^b\\d$|^beta'))

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
  facet_wrap(. ~ subject, ncol = 4, labeller = labeller(label_both)) + 
  theme_bw()+
  coord_cartesian(ylim = c(0, 100))+
  xlab('Tiempo (hr)') + ylab(bquote(C[PRED]~(mg/L)))

gPRED1


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

