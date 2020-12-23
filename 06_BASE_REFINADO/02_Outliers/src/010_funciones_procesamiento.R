require(tidyverse)

#-------------------------------------------------------------------------------#
#' Preprocesamiento de Datos para modelamiento Bayesiano
#'
#' @param dataframe dataframe original
#' @param nbdoses número de dosis adicionales
#'
#' @return
#' lista con datos de observaciones y dosificación
#' @export
#'
#' @examples
#' data <- read_csv(file.path('data', paste0('data_TAD_del', 3, '.csv')), na = '.')
#' df1 <- procesamientoDatos(data) 
#' df1$dataDosis %>% view()
#' df1$dataOBS %>% view()
#' 
#' 
procesamientoDatos <- function(dataframe, nbdoses=5) {
  
  #-------------------------------------------------#
  # > 1.1. Eventos de administración
  #..................................................
  #' 1. Filtrar eventos de dosificación EVID==1
  #' 2. Cambiar la columna ADDL especificando número 
  #' de dosis adicionales para llegar a estado 
  #' estacionario.
  #' 3. Cambiar a cálculos por fila
  #' 4. Para cada evento hacer repeticiones en forma 
  #' de dataframe teniendo en cuenta nbdoses.
  #' 5. Agrupar por ID
  #' 6. Calcular el tiempo tras administración (TAD) 
  #' teniendo en cuenta los intervalos entre 
  #' dosificación (II).
  #' 7. Ordenar efectos de administración por ID y TAD
  #..................................................

  dataDosificacion <- dataframe %>% 
    filter(EVID==1) %>% 
    mutate(ADDL = nbdoses) %>% 
    rowwise() %>% 
    do(as.data.frame(.)[rep(1, .$ADDL),]) %>% 
    group_by(ID) %>%
    mutate(TAD = TAD + c(0:(ADDL[1]-1))*as.numeric(as.character(II)),
           ADDL = 0,
           II = 0) %>% 
    arrange(ID, TAD)
  
  
  #-------------------------------------------------#
  # > 1.2. Eventos de observación
  #..................................................
  #' 1. Crear función _seleccionUltimaDosis_
  #' 2. Filtrar eventos teniendo en cuenta sólo 
  #' observaciones EVID==0.
  #' 3. Calcular el tiempo tras administración de 
  #' dosis ajustado a nbdoses
  #..................................................

  seleccionUltimaDosis <- function(x, data) {
    data %>%              # Datos
      slice_tail() %>%    # Seleccionar última dosis
      select(ID, TAD)%>%  # Seleccionar ID y sujeto
      filter(ID == x) %>% # Filtrar por sujeto 
      `$`(TAD)            # Seleccionar TAD
  }
  
  
  dataObservacion <- dataframe %>% 
    filter(EVID==0) %>% 
    mutate(
      TAD = map2_dbl(ID, TAD, ~(seleccionUltimaDosis(.x, dataDosificacion) + .y))
    )
  
  # Retornar lista
  return(list(
    dataDosis = dataDosificacion, 
    dataOBS = dataObservacion)
    )
}

#-------------------------------------------------------------------------------#
#' Transformación de datos Tabulares en formato apropiado para análisis bayesiano
#'
#' @param lsDatosProcesados 
#'
#' @return
#' @export
#'
#' @examples
#' data <- read_csv(file.path('data', paste0('data_TAD_del', 3, '.csv')), na = '.')
#' df1 <- procesamientoDatos(data) 
#' formacionInputs(df1)
#' 
formacionInputs <- function(lsDatosProcesados) {
  dataObservacion  <- lsDatosProcesados$dataOBS
  dataDosificacion <- lsDatosProcesados$dataDosis
  
  eventoObservacion <- dataObservacion %>% 
    filter(YTYPE==1) %>% list(
      nObs      = dim(.)[1],
      cObs      = as.numeric(.$DV),
      time      = as.numeric(.$TAD),
      nSubjects = length(unique(.$ID)),
      
      start     = group_by(., ID) %>% 
        rownames_to_column() %>% 
        slice_head() %>% 
        `$`(rowname) %>% 
        as.integer(),
      
      end       = group_by(., ID) %>% 
        rownames_to_column() %>% 
        slice_tail() %>% 
        `$`(rowname) %>% 
        as.integer(),
      
      subject   = as.integer(.$ID)
    )
  
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
    timeAdm    = dataDosificacion$TAD,
    dose       = as.numeric(as.character(dataDosificacion$AMT)),
    tinf       = as.numeric(as.character(dataDosificacion$TINF))
  )
  return(list(evOBS = eventoObservacion, evDosis = eventoDosificacion))
}