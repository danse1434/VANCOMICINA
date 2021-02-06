#-------------------------------------------------------------------------------#
# Función de extracción de datos de archivos de verosimilitud (*extractor*)
#' @param popVal Parámetro a extraer de los archivos de datos
#' @param n Número de directorios en el perfil
#' @param mode modo de cálculo naive o interval
#' @param n_tail número de puntos para calcular intervalo
#' 
#' @return DataFrame con 
#................................................................................
#  1 Convertir *popVal* en quosure
#  2 Prealocar al vector *X* como una lista de 100 items
#  3 Crear *pop_par* - valor de parámetro de interés 
#    a Seleccionar el archivo *data*
#    b Filtrar el parámetro de interés *popVal*
#    c Seleccionar la columna valor
#    d Tomar el valor
#  4 Crear *pop_vector* - vector con perturbación de parámetros; multiplicar 
#  por factores de corrección en el intervalo (0.5, 1.5), 100 veces.
#  5 Crear *A*, data.frame con la columna Run que permite identificar la 
#  corrida, y la columna Parameter es *pop_vector* y corresponde a la 
#  perturbación aplicada en la corrida.
#  6 Agregar a cada elemento una tabla leída desde un directorio especificado 
#  por el parámetro, de manera iterativa.
#  7 Creación de la tabla *Y* que contiene las columnas IS, y el valor 
#  perturbado.
#    a Seleccionar X
#    b Convertir a *X* en un data.frame
#    c Convertir en tibble
#    d Seleccionar el criterio -2LL
#    e Renombrar a la variable Parameter en el nombre del parámetro
#    f Seleccionar columnas importanceSampling y la que contiene al parámetro
#................................................................................

extractor <- function(popVal, n = 100, mode='naive', n_tail=10, 
                      modelName = 'M2CPTM_nobs_2_aditv_corr2') {
  X = vector('list', length = n); 
  Y = vector('list', length = n); 
  
  for (i in 1:n) {
    dir_m = file.path(aux_dir, popVal, paste0('A',i), modelName)
    
    X[[i]] = read_csv(file.path(dir_m, 'LogLikelihood', 'logLikelihood.txt'), 
                      col_types = cols())
    
    Y[[i]] = read_csv(file.path(dir_m, 'populationParameters.txt'), 
                      col_types = cols())
  }
  
  X1 <- X %>% 
    map_dfr(~ .x, .id = 'Run') %>%
    pivot_wider(id_cols = 'Run', names_from = 'criteria', values_from = 'importanceSampling')
  
  Y1 <- Y %>% 
    map_dfr(~ .x, .id = 'Run') %>% 
    pivot_wider(id_cols = 'Run', names_from = 'parameter', values_from = 'value')
  
  W = left_join(X1, Y1, by = 'Run') %>% 
    # left_join(Z1, by = 'Run') %>%
    mutate(LL1 = OFV - min(OFV))
  
  if (mode == 'interval') {
    Z = vector('list', length = n);  
    
    for (i in 1:n) {
      dir_m = file.path(aux_dir, popVal, paste0('A',i), modelName)
      
      Z[[i]] = read_csv(file.path(dir_m, 'ChartsData', 'ImportanceSampling',
                                  'convergence.txt'), 
                        col_types = cols())
    }
    
    Z1 <- Z %>%
      map_dfr(~ .x, .id = 'Run') %>%
      group_by(Run) %>% 
      slice_tail(n = n_tail) %>% 
      summarise(
        LL_mn = mean(OFV),
        LL_li = quantile(OFV, 0.05),
        LL_ls = quantile(OFV, 0.95)
      )
    
    W <- left_join(W, Z1, by = 'Run') %>%
      mutate(
        LL_mn = LL_mn - min(OFV),
        LL_li = LL_li - min(OFV),
        LL_ls = LL_ls - min(OFV),
      )
  }
  
  return(W)
}


#-------------------------------------------------------------------------------#
#' Función que toma el valor 3,84 que corresponde a chi^2 con df = 1 y 
#' alpha de 0.05
#'
#' @param data una tabla de datos específica
#' @param x parámetro poblacional variante 
#' @param y 
#' @param v0 
#' @param xmin 
#' @param xmax 
#'
#' @return Retornar los dos puntos donde se corta la recta

xval.func <- function(data, x, y, v0 = 3.84, 
                      xmin = NA_real_, xmax = NA_real_) {
  # Conversión a quosures
  x_quo = rlang::ensym(x);
  y_quo = rlang::ensym(y);
  # Extracción de vectores **x** y **y**
  x_range = pull(data,!!x_quo);
  y_range = pull(data,!!y_quo);
  
  # Selección de valores mínimos o máximo por defecto
  x_min = max(xmin, min(x_range), na.rm = TRUE);
  x_max = min(xmax, max(x_range), na.rm = TRUE);
  
  # Valor de x con mínimo de LL
  m <- slice(data, which.min(!!y_quo)) %>%
    pull(!!x_quo) 
  
  # Optimización de límite de v0 para puntos de 
  # Crear una función aproximada con el valor de perturbación como x y la 
  # verosimilitud corregida como y.
  f1 <- approxfun(x = x_range, y = y_range)
  
  # Extraer los puntos donde se corta el intersecto optimizando las funciones 
  # y definiendo dos intervalos que limitan por izq y der al valor medio.
  A <- vector(length = 2)
  
  A[[1]] <- optimize(
    function(t0) abs(f1(t0) - v0),
    interval = c(x_min, x_max), upper = ceiling(m))$minimum
  
  A[[2]] <- optimize(
    function(t0) abs(f1(t0) - v0),
    interval = c(x_min, x_max), lower = floor(m))$minimum
  
  Minimo  = tibble(!!x_quo := m, !!y_quo := f1(m))
  p_corte = tibble(!!x_quo := A, !!y_quo := f1(A))
  
  Result = list('Minimo' = Minimo, 'Punto_Corte' = p_corte)
  return(Result)
}

#-------------------------------------------------------------------------------#
#' Generar Gráfico Bidimensional de Contornos
#'
#' @param datos Tabla con perfil de verosimilitud
#' @param datos_ref Tabla con valores de referencia (modelo base o final)
#' @param x_param Variable en el eje X
#' @param y_param Variable en el eje Y
#' @param z_indic Variable con el indicador de convergencia
#' @param xlab Etiqueta eje X
#' @param ylab Etiqueta eje Y
#' @param n_bins Número de niveles o contornos
#' @param tipo Tipo de gráfico ggplot o plotly
#'
#' @return
#' @export
#' objeto gráfico de tipo GGplot o Plotly
#'
#' @examples
#' 
#' 
generarGrafico2D <- function(datos, datos_ref, x_param, y_param, z_indic, 
                             xlab='x', ylab='Y', n_bins=5, tipo='ggplot') {
  
  x_param_quo = ensym(x_param)
  y_param_quo = ensym(y_param)
  z_indic_quo = ensym(z_indic)
  
  # Identificación de bins para hacer contornos
  lvl = pull(datos, !!z_indic_quo) %>% 
    list('min' = min(.), 'max' = max(.))
  
  if (tipo=='ggplot') {
    
    # Especificación de gráfico GGplot2
    g <- ggplot(datos, aes(x=!!x_param_quo, y=!!y_param_quo, z=!!z_indic_quo)) + 
      geom_contour_filled(aes(fill=stat(level)), 
                          breaks = seq(lvl$min, lvl$max, length.out = n_bins+1)) +
      # Valor de verosimilitud mínimo global
      geom_point(data=datos %>%  slice_min(LL1), shape=8, color='red') +
      
      # Valor de verosimilitud para estimación de modelo base o final
      geom_point(data=datos_ref %>% add_column(!!z_indic_quo:=0), shape=4, color='green1') +
      scale_fill_viridis_d(name = 'Nivel') +
      guides(fill = guide_colorsteps(barheight = unit(4, "cm"))) +
      xlab(xlab) + ylab(ylab)
    
  } else if (tipo=='plotly') {
    # Generar matriz de mapeo entre las dos dimensiones
    mat = datos[,c(x_param, y_param, z_indic)] %>%
      pivot_wider(id_cols = !!x_param_quo, 
                  names_from = !!y_param_quo, 
                  values_from= !!z_indic_quo) %>% 
      column_to_rownames(var = x_param) %>% 
      as.matrix()
    
    # Configuración de número de contornos
    cont_ls = list(
      showlabels = TRUE, 
      start = lvl$min, end = lvl$max, 
      size = (lvl$max - lvl$min)/(n_bins)
    )
    
    # Creación de plotly
    g <- plot_ly(x=dimnames(mat)[[1]], y=dimnames(mat)[[2]], z = mat, type = "contour", 
                 contours = cont_ls) %>%
      layout(xaxis = list(title = xlab), yaxis = list(title = ylab)) %>%
      config(.Last.value, mathjax = "cdn")
    
  } else {
    stop("Tipo no reconocido")
  }
  return(g)
}