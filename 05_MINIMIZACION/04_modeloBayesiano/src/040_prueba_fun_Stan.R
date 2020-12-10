source('./src/000_preprocesamiento_Datos.R', encoding = 'UTF8')
source('./src/051_fun_funcion2Cptm.R', encoding = 'UTF8')

#-------------------------------------------------------------------------------#
# 3. Modelamiento bayesiano ----------------
#-------------------------------------------------------------------------------#
# Ejecución de pruebas
# test = rstan::stan(
#   "src/020_mod_2CPTM_Inf_MM_stan.stan", # Especificación Modelo Stan
#   data = stan_d,                        # Especificación Datos
#   # pars = params_monitor,       # Parámetros a monitorizar
#   chains = 1,                           # Cadenas
#   init = init,
#   iter = 10                             # Iteraciones
# )







stan_mod <- '
functions{
real twoCPTM_model_simple(real[] time, real[] dose, real[] tinf, real dt, 
        real CL, real Q, real V1, real V2){
    // Longitud total de matriz de historial de dosis
    int n_bolus = size(time);
    // Longitud filtrada
    int nb = 0;
    
    // Parámetros secundarios
    real k10;
    real k12;
    real k21;
    real ksum;
    vector[2] A;
    vector[2] alpha;
    // Predicciones
    real Cp = 0;
    // Variables auxiliares
    real D = 0;
    real t0 = 0;
    real t_inf = 0;
    
    // Acumula el N. de dosis administradas a dt
    for(n in 1:n_bolus){
      if(!(time[n]>dt)){
        nb += 1;
      }
    }
    
    k10 = CL / V1;
    k12 = Q / V1;
    k21 = Q / V2;
    ksum = k10 + k12 + k21;
    
    alpha[1] = (ksum + sqrt((ksum^2) - (4.0 * k10 * k21)))/2.0;
    alpha[2] = (ksum - sqrt((ksum^2) - (4.0 * k10 * k21)))/2.0;

    A[1] = (1/V1)*(alpha[1]-k21)/(alpha[1]-alpha[2]);
    A[2] = (1/V1)*(k21-alpha[2])/(alpha[1]-alpha[2]);
    
    //print( [dt, time[nb], dt - time[nb], nb] );
    
    if( !((dt - time[nb]) > (tinf[nb])) ){
      // Predomina la acumulación del fármaco
      
      for(i in 1:nb){
        D = dose[i];
        t0 = time[i];
        t_inf = tinf[i];
        
        if(i==nb){
          // en la última dosis
          Cp += (D/t_inf) * 
          ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*(dt-t0))) + 
           (A[2]/alpha[2]) * (1 - exp(-alpha[2]*(dt-t0))));
         } else {
         Cp += (D/t_inf) * 
         ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*t_inf)) * exp(-alpha[1]*(dt-t0-t_inf)) + 
         (A[2]/alpha[2]) * (1 - exp(-alpha[2]*t_inf)) * exp(-alpha[2]*(dt-t0-t_inf))
         );
          }
      }
    } else {
      // Predomina la eliminación del fármaco
      for(i in 1:nb){
        D = dose[i];
        t0 = time[i];
        t_inf = tinf[i];
        
        Cp += (D/t_inf) * 
            ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*t_inf)) * exp(-alpha[1]*(dt-t0-t_inf)) + 
               (A[2]/alpha[2]) * (1 - exp(-alpha[2]*t_inf)) * exp(-alpha[2]*(dt-t0-t_inf)) ); 
      }
    }
    
    return (max([Cp, 1e-6]));
  }
}
'



rstan::expose_stan_functions(stanc(model_code = stan_mod))

amt = list(
  time = stan_d$timeAdm[stan_d$startAdmEv[stan_d$subject[3]]:stan_d$endAdmEv[stan_d$subject[3]]],
  dose = stan_d$dose[stan_d$startAdmEv[stan_d$subject[3]]:stan_d$endAdmEv[stan_d$subject[3]]],
  tinf = stan_d$tinf[stan_d$startAdmEv[stan_d$subject[3]]:stan_d$endAdmEv[stan_d$subject[3]]]
)

parameters = c(stan_d$muClHat, stan_d$muQHat, stan_d$muV1Hat, stan_d$muV2Hat)



df <- tibble(t=seq(0, 60, length.out = 1e3))

df1 <- df %>% 
  mutate(
    Stan = map_dbl(t, ~twoCPTM_model_simple(
      amt$time, amt$dose, amt$tinf, .x, 
      parameters[1], parameters[2], parameters[3], parameters[4])),
    Function_UDF = map_dbl(t, ~funcion2Cptm(
      parameters, amt, 1, .x))
  )

df1 %>% 
  pivot_longer(cols = c('Stan', 'Function_UDF')) %>% 
  ggplot(aes(x=t, y=value, col=name)) +
  geom_line() +
  facet_wrap(. ~ name, ncol=2) +
  theme_bw() +
  scale_color_manual(values=c('red', 'blue')) +
  xlab('Tiempo (h)') + ylab(bquote(Cp~(mg/L))) +
  coord_cartesian(xlim = c(0,60), ylim=c(0,40))+
  theme(legend.position = 'none')



