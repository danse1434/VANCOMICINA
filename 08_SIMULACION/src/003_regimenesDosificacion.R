require(tidyverse)

#-------------------------------------------------------------------------------#
# Factores de dosificación a administrar

# DD <- c(15.0, 17.5, 20.0, 25.0, 30.0)

# Se cambia a dosificación de dosis diaria sólo en mg
DD <- seq(1000, 4000, 500) 

II <- c(6, 8, 12, 24)
Tinf <- c(2, 4, 24)

#-------------------------------------------------------------------------------#
# Creación de tabla con especificación de administración

tabla <- expand.grid('DD' = DD, 'II' = II, 'Tinf' = Tinf) %>% 
  arrange(DD, II, Tinf) %>% 
  mutate(Allow = ifelse(II >= Tinf, T, F)) %>% 
  filter(Allow) %>% 
  select(-Allow) %>% 
  tibble(.) %>% 
  # Eliminar casos de II 24 pero con tiempos de infusión muy cortos
  filter((II != 24) | (!Tinf %in% c(2, 4))) %>% 
  rownames_to_column('ID')

# Escribir a CSV
write_csv(tabla, file.path('data', 'adm_list.csv'))
