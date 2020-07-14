#-------------------------------------------------------------------------------#
#' Estimación de filtración glomerular (eGFR) mediante CDK-EPI
#'
#' @param Scr concentración de creatinina sérica
#' @param edad edad (años)
#' @param sexo si mujer debe ser 1, si hombre debe ser 0
#' @param raza si raza negra entonces 1
#'
#' @return estimación de GFR en mL/min/1.73m2
#' @export
#'
#' @examples
#' eGFR(Scr = 0.46, edad = 43, sexo = 1, raza = 0)
#' 
eGFR = function(Scr,edad,sexo,raza){
  if (sexo == 1) {k = 0.7; b = 1; a = -0.329 # Mujeres
  } else {k = 0.9; b = 0; a = -0.411} # Hombres
  c = ifelse(raza == 1, 1, 0)
  
  # if (raza == 1){c = 1} else {c=0}
  
  # Tasa de filtración glomerular
  GFR = 141*(min(Scr/k,1))^(a)*(max(Scr/k,1))^-1.209*(0.993^edad)*(1.018^b)*(1.159^c)
  return(GFR)
}

eGFR = Vectorize(eGFR, c('Scr','edad','sexo'))

#-------------------------------------------------------------------------------#
#' Batería de pruebas de normalidad de variables continuas
#'
#' @param data    data frame con las variables colectadas para cada individuo
#' @param vector  vector indicador de posición de variables continuas
#' @param alpha   nivel de significancia de las pruebas
#'
#' @return un tibble con los valores p de las pruebas de normalidad aplicada
#' @export
#'
#' @examples
#' normtest_batery(data = df1, vector = c(12, 14:21, 23, 24), alpha = 0.05) 
#' 
normtest_batery = function(data, vector, alpha) {
  df <-  matrix(nrow = length(unique(vector)), ncol = 7)
  for (j in vector) {
    X = dplyr::pull(data, j) # Selecciona como un vector atómico a una columna
    i = match(j, vector) # Encuentra la posición en el vector
    df[i, 1] = colnames(data[, j])
    df[i, 2] = shapiro.test(X)$p.value          # Shapiro-Wilks
    df[i, 3] = nortest::ad.test(X)$p.value      # Anderson-Darling
    df[i, 4] = nortest::cvm.test(X)$p.value     # Cramer von Mises
    df[i, 5] = nortest::lillie.test(X)$p.value  # Liliefors
    df[i, 6] = nortest::pearson.test(X)$p.value # Pearson
    df[i, 7] = nortest::sf.test(X)$p.value      # Shapiro Francia
  }
  colnames(df) = c('Variable','Shapiro','Anderson_Darling','Cramer_von_Mises',
    'Liliefors','Pearson','Shapiro_Francia')
  
  df1 <- as_tibble(df) %>% 
    mutate(across(.cols = 2:7, ~as.numeric(.x)))
  
  return(df1)
}

#-------------------------------------------------------------------------------#
#' Función de formato condicional (una condición)
#'
#' @param data  tabla de datos 
#' @param param nombre de columna a seleccionar
#' @param crit1 criterio umbral de indicador
#'
#' @return formato condicional adicionado a objeto gt()
#' @export
#'
#' @examples
#' gt() %>% fun_param(Pearson)
#' 
fun_param <- function(data, param, crit1 = 0.05) {
  param_quo <- rlang::ensym(param)
  
  z = data %>%
    tab_style(style = list(cell_fill(color = alpha('#00ff00', 0.5))),
              locations = cells_body(columns = vars(!!param_quo),
                                     rows = (!!param_quo < crit1)))
  return(z)
}

#-------------------------------------------------------------------------------#
#' Función de detección de datos anómalos
#' Esta función permite determinar si un dato es anómalo por estar afuera de la 
#' distribución en un rango *Q1-1.5~IQR* a *Q3+1.5~IQR* (con valores 1.5 veces IQR).
#'
#' @param vec columna que corresponde a una variable continua 
#' @param val valor individual en la variable seleccionada
#'
#' @return vector que indica las variables que están dentro del IQR (TRUE) y 
#' fuera del mismo (FALSE)
#' @export
#'
#' @examples
#' out_det(vec = df1$AGEA, val = df1$AGEA)
#' 
out_det <- function(vec, val) {
  a1 = quantile(x = vec, probs = 0.75)[[1]] + IQR(vec) * 1.5
  a2 = quantile(x = vec, probs = 0.25)[[1]] - IQR(vec) * 1.5
  b = ifelse(val >= a2 & val <= a1, TRUE, FALSE)
  
  return(b)
}
out_det = Vectorize(FUN = out_det, vectorize.args = 'val') # Vectorizar

#-------------------------------------------------------------------------------#
#' Función de formato condicional (verdadero o false)
#'
#' @param data  tabla de datos 
#' @param param nombre de columna a seleccionar
#' @param crit1 criterio umbral de indicador
#'
#' @return formato condicional adicionado a objeto gt()
#' @export
#'
#' @examples
#' gt() %>% fun_param(Pearson)
#' 
fun_param_1 <- function(data, param) {
  param_quo <- rlang::ensym(param)
  
  z = data %>%
    tab_style(style = list(cell_fill(color = alpha('red', 0.5))),
              locations = cells_body(columns = vars(!!param_quo),
                                     rows = (!!param_quo == FALSE)))
  return(z)
}

#-------------------------------------------------------------------------------#
#' Función de **correlación de Pearson**, modificada con índices
#' 
#' 1 Seleccionar los datos por medio de índices
#' 2 Se calcula la correlación entre dos vectores seleccionados desde el
#' data.frame, por sus nombres _x_ y _y_.
#' 3 Retorna esta correlación
#' 
#' @param data data frame con variables continuas
#' @param indices indices de filas
#' @param x variable continua 1 (como string)
#' @param y variable continua 2 (como string) 
#'
#' @return resultados de correlación de Pearson
#' @export
#'
#' @examples
#' corr(df1, 1:14, 'AGEA', 'HCM')
#' 
corr <- function(data, indices, x, y) {
  df <- data[indices,]
  Z = cor(df[, x], df[, y])
  return(Z)
}

#-------------------------------------------------------------------------------#
#' Función de **Bootstrap** con la función de correlación 
#'
#' @param data data frame con los datos
#' @param x variable 1 (número)
#' @param y variable 2 (número)
#' 
#' 
Confint_Boot <- function(data, x, y) {
  B <- boot::boot(data = data, statistic = corr, R = 1000, x = x, y = y )
  C <- boot::boot.ci(B, type = "perc")
  return(C)
}

#-------------------------------------------------------------------------------#
#' Función para variable independiente discreta y dependiente continua
#'
#' @param df data.frame
#' @param xcol variable independiente
#' @param ycol variable dependiente
#' @param h paleta de dos colores
#'
#' @return gráfico ggplot2 de tipo boxplot
#' @export
#'
#' @examples
#' plot2(df1, SEXF, 'WTKG')
#' 
plot2 <- function(df, xcol, ycol, h = c('red', 'blue')) {
  xcol_qu = rlang::ensym(xcol)
  ycol_qu = rlang::sym(ycol) # Para poder leer vector de caracteres
  
  df %>% 
    mutate(!!xcol_qu := as.factor(!!xcol_qu)) %>%
    ggplot(aes(!!xcol_qu, !!ycol_qu, colour = !!xcol_qu)) +
    geom_boxplot(aes(colour=!!xcol_qu,
                     fill=after_scale(alpha(colour, 0.4)))) +
    geom_point() +
    scale_color_manual(values = h) +
    theme(legend.position = c(0.8, 0.8))
}

#-------------------------------------------------------------------------------#
#' Evaluación de parámetos por grupo
#'
#' @param data tabla de datos
#' @param varg variable de agrupamiento
#'
#' @return tabla con resumen de variables seleccionadas por grupo
#' @export
#'
#' @examples
#' comp_func_1(data = df1, varg = SEXF)
#' 
comp_func_1 = function(data, varg){
  varg_q = rlang::ensym(varg)
  
  data %>%
    group_by(!!varg_q) %>%
    summarise(across(
      c('WTKG', 'HCM', 'SCRMGDL', 'ALBGDL', 'CLCRMLMIN', 'PROGDL'),
      list(
        mn = ~ mean(.x, na.rm = TRUE),
        q1 = ~ fivenum(.x)[2],
        q3 = ~ fivenum(.x)[4]
      ))) %>%
    pivot_longer(
      cols = !matches(rlang::quo_name(varg_q)),
      names_to = c('Variable', 'Est'),
      names_sep = '_',
      values_to = 'Val'
    ) %>%
    pivot_wider(names_from = 'Est', values_from = 'Val') %>%
    arrange(Variable, !!varg_q)
}



plot1 <- function(df, xcol, ycol) {
  xcol_qu = rlang::enquo(xcol)
  ycol_qu = rlang::enquo(ycol)
  dplyr::filter(df, EVID == 1 & TAD == 0) %>%
    ggplot(aes(x = !!xcol_qu, y = !!ycol_qu)) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect(colour = 'black'),
      plot.margin = margin(0, 0, 0, 0, unit = 'cm'),
      axis.text = element_blank()
    ) +
    geom_smooth(
      method = 'loess',
      formula = y ~ x,
      se = F,
      lty = 'solid',
      color = 'red2',
      size = 0.4
    ) +
    geom_smooth(
      method = 'lm',
      formula = y ~ x,
      se = F,
      lty = 'solid',
      color = 'blue'
    ) +
    geom_point(shape = 1, size = 1)
}



plotly = function(x, y, X, Y, z) {
  X_qu = rlang::enquo(X)
  Y_qu = rlang::enquo(Y)
  if (z == 1) {
    print(plot1(data, !!X_qu, !!Y_qu), vp = vplayout(x, y))
  }
  if (z == 2) {
    print(plot2(data, !!X_qu, !!Y_qu), vp = vplayout(x, y))
  }
}

