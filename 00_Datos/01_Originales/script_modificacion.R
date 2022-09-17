##------------------------------------------------------------------------------#
## Nombre del Script: Modificación de datos originales de vancomicina -----------
##  
## Propósito del Script:  
##  
## Autor: Daniel S. Parra González 
##
## Fecha de creación: 07/07/2020
## Fecha de modificación: 20/01/2021 --- 28/03/2021
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(readxl)

# Lectura de archivo con datos original
dataVAN <- read_xlsx('01_Originales/original_200219.xlsx') %>%
  filter(record_id != 15)

#-------------------------------------------------------------------------------#
# 1 Manipulación inicial --------------------------------
#-------------------------------------------------------------------------------#
#' Manipulación de tabla **dataVAN** en **dataVAN1**
#................................................................................
#' 1 Eliminar el individuo 15 (excluido del estudio)
#' 2 Renombrar variables con la nomenclatura estándar de NONMEM primeras tres/cuatro 
#' letras es variable, y ultimas letras unidades de medición. 
#' 3 Convertir variable _SEXF_ con femenino como 1, y masculino como 0
#' 4 Convertir _HCM_ en variable numérica
#' 5 Convertir _HCM_ en algunos valores que están escritos en metros a cm
#' 6 Agrupar para operaciones por filas
#' 7 Concatenar *antibioticos_recibidos* en variable única
#' 8 Concatenar *cual_antibiotico_combio* en variable única
#' 9 Concatenar *tipo_de_infeccion* en variable única
#' 10 Concatenar *cual_microorganismo* en variable única
#' 11 Concatenar *esquema_de_quimioterapia* en variable única
#' 12 Eliminar variables no necesarias
#................................................................................

dataVAN1 <- dataVAN %>% 
  rename(
    ID        = record_id,
    SEXF      = sexo,
    AGEA      = edad,
    WTKG      = peso,
    HCM       = talla,
    SCRMGDL   = creatinina_serica,
    ALBGDL    = albumina_serica,
    PROGDL    = proteinas_sericas_totales,
    CLCRMLMIN = tasa_filtracion_glomerular,
    RAL       = numero_total_leucocitos,
    RAN       = numero_total_neutrofilos,
    ANTU      = uso_antibiotico_mes_previo
  ) %>%
  mutate(
    SEXF = if_else(SEXF == 1, 0, 1),
    HCM  = as.numeric(HCM),
    HCM  = if_else(HCM < 30, HCM * 100, HCM), 
    AMT  = as.numeric(dosis_suministrada) * WTKG,
  ) %>% 
  rowwise(ID) %>% 
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
                 sep = ',', collapse = ', '), .keep = "unused") %>% 
  select(-numero_de_formulario, -iniciales_del_participante, -ra,
         -cedula_ciudadania, -fecha_de_nacimiento, -fecha_de_ingreso,
         -regimen_seguridad_social, -fecha_ultima_quimioterapia, -fecha_transplante_medula,
         -iniciales_quien_diligencia,-diligenciamiento_fecha,
         -iniciales, -fecha_diligenciamiento, -antibiotico_vancomicina,
         -Codigo, -`...207`, 
         -localizacion_infeccion, 
         # Todos los pacientes tienen infección y cáncer
         -diagnostico_de_infeccion, -diagnostico_oncologico,
         # Se eliminan porque sólo tienen importancia en la BD
         -formato_1_recoleccin_de_datos_para_la_cuantificaci_complete,
         -formato_2_recoleccin_de_muestras_para_la_cuantific_complete,
         # Se eliminan porque no aportan información importante todas son 1
         -tratamiento_radioterapia, -transplante_medula_osea,
         -tratamiento_quimioterapia) %>% 
  select(!matches('fecha_hora_muestra|resultado_\\d{1,2}_muestra_\\d{1,2}'))

#-------------------------------------------------------------------------------#
# 2 Observaciones y fechas ----------------------------
#-------------------------------------------------------------------------------#
#' Crear tabla *dataVAN1a* con valores de concentración de vancomicina
#................................................................................
#' 1 Seleccionar columnas con valores de resultado y muestra, e id en *dataVAN*
#' 2 Expandir con estas columnas, y crear dos columnas _muestra_(id de muestra), 
#' y _resultado_ (id repetición), con valores de Cp en _valor_.
#' 3 Convertir a las tres columnas creadas en números
#................................................................................

dataVAN1a <- dataVAN %>% 
  select(matches('id$|resultado_\\d{1,2}_muestra_\\d{1,2}')) %>% 
  pivot_longer(cols = matches('resultado_\\d{1,2}_muestra_\\d{1,2}'), 
               names_to = c('resultado', 'muestra'), 
               names_pattern = "resultado_(.*)_muestra_(.*)",
               values_to = 'valor') %>% 
  mutate(across(c(resultado, muestra, valor), ~as.numeric(.x))) 

#' Crear tabla *dataVAN1b* con tiempos de cada muestra
#................................................................................
#' 1 Renombrar _*fecha_hora_muestra*_ con un 1 para estandarizar con el resto 
#' 2 Seleccionar columna ID y las fechas de muestra
#' 3 Expandir con estas columnas, y crear columna _muestra_(id de muestra), 
#' con valores de tiempo en _tiempo_.
#' 4 Convertir a _muestra_ en número
#................................................................................

dataVAN1b <- dataVAN %>% 
  rename('fecha_hora_muestra_1' = 'fecha_hora_muestra') %>% 
  select(matches('id$|fecha_hora_muestra_\\d{1,2}')) %>% 
  pivot_longer(cols = matches('fecha_hora_muestra_\\d{1,2}'),
               names_to = "muestra",
               names_pattern = "fecha_hora_muestra_(.*)",
               values_to = "tiempo") %>% 
  mutate(across(c(muestra), ~as.numeric(.x))) 

#' Crear tabla con observaciones **dataVAN2**
#' Se comprobaron las obs. con el registro en la columna de _observaciones_
#................................................................................
#' 1 Unir datos de *dataVAN1b* (concentración) y *dataVAN1a* (tiempo) para cada 
#' _id_ y _muestra_
#' 2 Los resultados 1 a 5 fueron colectados mediante un método de difusión en 
#' agar, y los resultados 6 a 10 fueron colectados por un método de quimiolumi-
#' niscencia, se crea columna *tipo*
#' 3 Agrupar por *record_id* (ID), _muestra_, _tiempo_, y _tipo_
#' 4 Resumir por valor medio, desviación estándar y n
#' 5 Agrupar sólo por *record_id*, y _*tipo*_
#' 6 Calcular _TAD_ que es la diferencia en horas entre el tiempo y el mínimo más 1
#................................................................................

dataVAN2 <- dataVAN1b %>% 
  left_join(dataVAN1a, by = c('record_id', 'muestra')) %>% 
  mutate(tipo = if_else(resultado<=5, 'Microbiol.', 'Quimiolumin.')) %>% 
  group_by(record_id, muestra, tiempo, tipo) %>% 
  summarise(mn = mean(valor, na.rm = TRUE),
            sd = sd(valor, na.rm = TRUE),
            n  = n()) %>% 
  ungroup() %>% 
  group_by(record_id, tipo) %>% 
  mutate(TAD = difftime(tiempo, min(tiempo, na.rm = TRUE), 
                        units = "hours") + 1)

#' Crear *dataVAN2b* que tiene eventos de observación
#................................................................................
#' 1 Desagrupar a *dataVAN2*
#' 2 Renombrar variables ID y DV
#' 3 Determinar _YTYPE_ que es tipo de DV (de acuerdo a método de medición)
#' 4 Convertir a _TAD_ en números
#' 5 Adicionar columna de _EVID_ con valor de 0 (ev. observación)
#' 6 Adicionar columna de _MDV_ con valor de 0 (no quitar dosis)
#' 7 Eliminar columnas que no cumplen con requisitos de NONMEM
#' 8 Eliminar valors de NaN en _DV_
#................................................................................

dataVAN2b <- dataVAN2 %>% 
  ungroup() %>% 
  rename(ID = record_id,
         DV = mn) %>% 
  mutate(YTYPE = if_else(tipo == 'Microbiol.', 1L, 2L),
         TAD   = as.double(TAD)) %>% 
  add_column(EVID = 0L, 
             MDV  = 0L) %>% 
  select(-muestra, -tiempo, -tipo, -sd, -n) %>% 
  filter(!is.na(DV))

#-------------------------------------------------------------------------------#
# 3 Registros de dosis ------------------------------------------------
#-------------------------------------------------------------------------------#
#' Crear *dataVAN3* que tiene datos en formato NONMEM
#................................................................................
#' 1 Desagrupar
#' 2 Seleccionar columnas con valores en mayúsculas, retirar _Aislamiento_
#' 3 Adicionar columnas de NONMEM
#................................................................................

dataVAN3 <- dataVAN1 %>%
  ungroup() %>%
  select(matches("[A-Z]", ignore.case = FALSE), -Aislamiento) %>%
  add_column(
    TAD   = 0.00,      # Tiempos
    DV    = NA_real_,  # Concentración
    MDV   = 0,         # Dosis faltante?
    EVID  = 1L,        # Identificador de DV
    # AMT   = 2000,      # Dosis (cantidad en mg) # Esto estaba mál se colocó la verdadera
    TINF  = 2,         # Tiempo de infusión (hrs)
    ADDL  = NA_integer_, # Dosis adicionales
    II    = 12,          # Intervalos entre dosis
    SS    = 1,           # Estado estacionario
    YTYPE = NA_integer_  # Tipo de DV
  )

#-------------------------------------------------------------------------------#
# 4 Crear tabla final *data_final* -------------------------------------------
#-------------------------------------------------------------------------------#
#' 1 Mezclar filas de observaciones con filas de dosis
#' 2 Ordenar por _ID_, seguido de _TAD_, y _DV_
#' 3 Relocalizar las columnas
#................................................................................

data_final <- dataVAN2b %>%
  bind_rows(dataVAN3) %>% 
  arrange(ID, TAD, DV) %>% 
  relocate(c(AMT:SS), .after = MDV)

# Modificación para el individuo 7, se encuentra que este tiene un tiempo de 
# infusión de 4 horas. Esto fue identificado en la tesis de referencia en la 
# página 21. Esto lo evidenció el autor por el Cmax mostrado por el perfil que 
# coincidía en las determinaciones microbiológicas y por quimioluminiscencia. 
# 
# Este error en especificación del modelo se evidenció durante el refinado del 
# modelo base (20-01-2020).

data_final <- data_final %>% 
  mutate(TINF = ifelse(ID==7 & EVID==1, 4, TINF))

#-------------------------------------------------------------------------------#
# 5 Almacenamiento ---------------------------------------------
#-------------------------------------------------------------------------------#
write_csv(data_final, 'results/data_TAD.csv', na = '.')


#-------------------------------------------------------------------------------#
# Gráfico de spaguetti inicial

dataVAN2 %>% 
  filter(record_id != 15) %>%
  mutate(id = factor(record_id)) %>% 
  ungroup() %>%
  ggplot(aes(x = TAD, y = mn, col = id)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = mn-sd, ymax = mn+sd)) +
  coord_cartesian(xlim = c(0, 12)) +
  facet_wrap(. ~ tipo) + 
  theme_bw() +
  xlab('TAD (hrs)') +
  ylab('Cp (mg/L)') +
  theme(legend.position = 'bottom') +
  scale_color_viridis_d() +
  guides(col = guide_legend(nrow = 2))

ggsave('results/spaguet_original.pdf', device = 'pdf', width = 8, height = 6)


dataVAN2 %>% 
  pivot_wider(id_cols = c(record_id, muestra), 
              names_from = tipo, values_from = mn) %>%
  mutate(ID = factor(record_id)) %>% 
  ggplot(aes(x = Quimiolumin., y = Microbiol., col = ID)) + 
  theme_bw() + 
  geom_abline(slope = 1, intercept = 0, lty = 'dashed') + 
  geom_point() +
  xlab('C. plasmática (mg/L) - Quimiolum.') + 
  ylab('C. plasmática (mg/L) - Microbiológ.') + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 60)) + 
  theme(legend.position = 'bottom') +
  guides(colour=guide_legend(ncol=7))


ggsave('results/correspondenciaMetodos.pdf', device = 'pdf', width = 6, height = 6)

