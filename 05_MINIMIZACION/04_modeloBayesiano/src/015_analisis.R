require(tidyverse)

twoCPTM_model_simple <- function(parameters, amt, cmt, dt) {
  stopifnot(length(amt$time)==length(amt$dose))
  
  n_bolus = length(amt$time)
  amt1 <- list('time' = c(0), 'dose' = c(0), 'tinf' = c(0))

  if (!'tinf' %in% names(amt)){
    amt$tinf = rep(0, n_bolus)
  }

  for (i in 1:n_bolus) {
    if (amt$time[i]<=dt) {
      amt1$time[i]=amt$time[i]
      amt1$dose[i]=amt$dose[i]
      amt1$tinf[i]=amt$tinf[i]
    }
  }
  amt <- amt1
  n_bolus = length(amt$time)
  
  CL = parameters[1]
  Q  = parameters[2]
  V1 = parameters[3]
  V2 = parameters[4]
  
  k10 = CL / V1
  k12 = Q / V1
  k21 = Q / V2
  ksum = k10 + k12 + k21
  
  alpha = vector(length = 2)
  alpha[1] = (ksum + sqrt(ksum^2L - 4.0 * k10 * k21)) / 2.0
  alpha[2] = (ksum - sqrt(ksum^2L - 4.0 * k10 * k21)) / 2.0
  
  A = vector(length = 2)
  A[1] = 1/V1 * (alpha[1] - k21) / (alpha[1] - alpha[2])
  A[2] = 1/V1 * (k21 - alpha[2]) / (alpha[1] - alpha[2])
  
  Cp = 0
  branch = vector()
  
  t0 = 0
  
  for (i in 1:n_bolus) {
    t0 = t0 + amt$time[i]
    t_inf = amt$tinf[i]
    
    
    
    if(amt$time[i] <= dt){
      D = amt$dose[i]
      # t_inf = amt$tinf[i]
    } else {
      D = 0
      # t_inf = 0
    }
    
    if (t_inf==0) {
      if ((dt-t0)<0) {
        Cp = Cp + 0
        branch <- append(branch, 'A1')
      } else {
        Cp = Cp + D * (A[1] * exp(-alpha[1] * (dt-t0)) + A[2] * exp(-alpha[2] * (dt-t0)))
        branch <- append(branch, 'A2')
        # print(Cp)
      }
    } else {
      if ((dt-t0)<0) {
        Cp = Cp + 0
        # print(Cp)
        branch <- append(branch, paste0(i,'B1'))
      } else {
        if ((dt-t0) <= t_inf) {
          if (i==n_bolus){
            Cp = Cp + (D/t_inf) * 
              ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*(dt-t0))) + 
                 (A[2]/alpha[2]) * (1 - exp(-alpha[2]*(dt-t0))))
            branch <- append(branch, paste0(i,'B2A'))
            # print(Cp)
          } else {
            Cp = Cp + (D/t_inf) * 
              ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*t_inf)) * exp(-alpha[1]*(dt-t0-t_inf)) + 
                 (A[2]/alpha[2]) * (1 - exp(-alpha[2]*t_inf)) * exp(-alpha[2]*(dt-t0-t_inf))
              )
            branch <- append(branch, paste0(i,'B2B'))
            print(c(i, dt, t0, Cp, D/t_inf, dt-t0-t_inf), digits = 3)
            # print(Cp)
          }
        } else {
          Cp = Cp + (D/t_inf) * 
            ((A[1]/alpha[1]) * (1 - exp(-alpha[1]*t_inf)) * exp(-alpha[1]*(dt-t0-t_inf)) + 
               (A[2]/alpha[2]) * (1 - exp(-alpha[2]*t_inf)) * exp(-alpha[2]*(dt-t0-t_inf)) )
          branch <- append(branch, paste0(i,'B3'))
          # print(Cp)
          
        }
      }
    }
  }
  return(list('Cp'=Cp, 'd'=branch))
}

amt <- list(time=8*(seq(length.out= 4L)-1), 
            dose=rep(2000, 4L),
            tinf=rep(2, 4L))

df <- data.frame('time' = seq(0, max(amt$time)+8, length.out = 50)) %>% as_tibble()

df <- df %>%
  mutate(pred = map(
    time,
    ~ twoCPTM_model_simple(
      parameters = c(13, 14, 23, 25, 0.1),
      amt = amt,
      cmt = 1,
      dt = .x
    )
  ),
  p = map_dbl(pred, 'Cp'),
  d = map_chr(pred, ~paste0(.x$d, collapse = '_'))
  )

g<-df %>%
  ggplot(aes(x=time)) +
  geom_line(aes(y=p), col='gray10') +
  geom_text(aes(y=p, label=d), col='blue3', size=2, angle=0) +
  theme_bw() #+
  # coord_cartesian(xlim = c(0,36), ylim=c(0,100))

ggsave(filename = 'figures/m_12.pdf', g, device= 'pdf',width = 8, height = 6)



microbenchmark::microbenchmark(twoCPTM_model_simple(
  parameters = c(13, 14, 23, 25, 0.1),
  amt = amt,
  cmt = 1,
  dt = 88
))
  



#-------------------------------------------------------------------------------#
require(mlxR)

adm <- list(time=amt$time, amount=amt$dose, tinf=amt$tinf)
Cc  <- list(name='Cc', time=seq(0, max(amt$time)+8, length.out = 1000))
p   <- c(Cl=13, Q=14, V=23, V2=25)
mod <- mlxR::inlineModel(
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

res <- simulx(model=mod, 
              parameter=p, 
              output=Cc, 
              treatment=adm)


df$P_correcto = res$Cc$Cc

g<-df %>%
  ggplot(aes(x=time)) +
  geom_line(aes(y=P_correcto), col='blue') +
  geom_line(aes(y=p), col='gray10') +
  # geom_text(aes(y=p, label=d), col='blue3', size=2, angle=90) +
  theme_bw() #+
# coord_cartesian(xlim = c(0,36), ylim=c(0,100))

# ggsave(filename = './m_12.pdf', g, device= 'pdf',width = 8, height = 6)



df %>% 
  ggplot(aes(x=p, y=P_correcto))+
  geom_point()
