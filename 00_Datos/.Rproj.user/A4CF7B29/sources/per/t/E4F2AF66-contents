##------------------------------------------------------------------------------#
## Nombre del Script: Modificación de datos originales de vancomicina -----------
##  
## Propósito del Script:  
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 07/07/2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
require(tidyverse)
require(readxl)


dataVAN <- read_xlsx('01_Originales/original_200219.xlsx')

dim(dataVAN)
colnames(dataVAN)

#' Eliminar el individuo 15
dataVAN <- dataVAN %>% 
  filter(record_id != 15)

dataVAN <- dataVAN %>% 
  mutate(talla = as.numeric(talla),
         talla = if_else(talla < 30, talla * 100, talla))


dataVAN[,1:23] %>% view()
dataVAN[,24:50] %>% colnames()


dataVAN %>% 
  rowwise(record_id) %>% 
  mutate(antibioticos_recibidos = 
           paste(c_across(contains('antibioticos_recibidos')), 
                 sep = ',', collapse = ', '), .keep = "unused") %>% 
  mutate(cual_antibiotico = 
           paste(c_across(contains('cual_antibiotico_combino')), 
                 sep = ',', collapse = ', '), .keep = "unused") %>% 
  mutate(tipo_de_infeccion = 
           paste(c_across(contains('tipo_de_infeccion')), 
                 sep = ',', collapse = ', '), .keep = "unused") %>% 
  mutate(cual_microorganismo = 
           paste(c_across(contains('cual_microorganismo')), 
                 sep = ',', collapse = ', '), .keep = "unused") %>% 
  mutate(esquema_de_quimioterapia = 
           paste(c_across(contains('esquema_de_quimioterapia')), 
                 sep = ',', collapse = ', '), .keep = "unused") %>% view()


dataVAN1 <- dataVAN %>% 
  select(!matches('antibioticos_recibidos|cual|tipo_de_infeccion|esquema_de'))


dataVAN1a <- dataVAN1 %>% 
  select(matches('id$|resultado_\\d{1,2}_muestra_\\d{1,2}')) %>% 
  pivot_longer(cols = matches('resultado_\\d{1,2}_muestra_\\d{1,2}'), 
               names_to = c('resultado', 'muestra'), 
               names_pattern = "resultado_(.*)_muestra_(.*)",
               values_to = 'valor') %>% 
  mutate(across(c(resultado, muestra, valor), ~as.numeric(.x))) 

dataVAN1b <- dataVAN1 %>% 
  rename('fecha_hora_muestra_1' = 'fecha_hora_muestra') %>% 
  select(matches('id$|fecha_hora_muestra_\\d{1,2}')) %>% 
  pivot_longer(cols = matches('fecha_hora_muestra_\\d{1,2}'),
               names_to = "muestra",
               names_pattern = "fecha_hora_muestra_(.*)",
               values_to = "tiempo") %>% 
  mutate(across(c(muestra), ~as.numeric(.x))) 


dataVAN2 <- dataVAN1b %>% 
  left_join(dataVAN1a, by = c('record_id', 'muestra')) %>% 
  mutate(tipo = if_else(resultado<=5, 'Microbiol.', 'Quimiolumin.')) %>% 
  group_by(record_id, muestra, tiempo, tipo) %>% 
  summarise(mn = mean(valor, na.rm = TRUE),
            sd = sd(valor, na.rm = TRUE),
            n  = n()) %>% 
  group_by(record_id, tipo, .drop = TRUE) %>% 
  mutate(TAD = difftime(tiempo, min(tiempo, na.rm = TRUE), 
                        units = "hours") + 1)

dataVAN2 %>%
  ungroup() %>%
  ggplot(aes(x = TAD, y = mn, col = factor(record_id))) +
  geom_point() + geom_line() +
  coord_cartesian(xlim = c(0, 12)) +
  facet_wrap(. ~ tipo)
  
dataVAN1 <- dataVAN1 %>% 
  select(!matches('fecha_hora_muestra|resultado_\\d{1,2}_muestra_\\d{1,2}'))

dataVAN1 %>% 
  select(-numero_de_formulario, -iniciales_del_participante, -ra,
         -cedula_ciudadania, -fecha_de_nacimiento, -fecha_de_ingreso,
         -regimen_seguridad_social, -fecha_ultima_quimioterapia, -fecha_transplante_medula,
         -iniciales_quien_diligencia,-diligenciamiento_fecha,
         -iniciales, -fecha_diligenciamiento) %>% view(.)

 
dataVAN[c("peso", "dosis_suministrada")] %>% 
  mutate(
    dosis_suministrada = as.double(dosis_suministrada),
    dosis = peso * dosis_suministrada
  )
