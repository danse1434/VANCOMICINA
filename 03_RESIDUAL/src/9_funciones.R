RES_C <- function(data, filt, x, y, xlab, ylab, ...) {
  ##########################################################################-
  # Volver las variables x y y en expresiones para evaluación tardía
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  
  df1 <- data %>% filter(Resid == filt)
  
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##  Cálculo de percentiles empiricos
  ##  1 Tomar la variable de datos a explorar.
  ##  2 Crear una variable que tome 6 grupos a partir de los datos ordenados 
  ##  con la función dplyr::ntile().
  ##  3 Agrupar por la variable reción creado.
  ##  4 Resumir por la media de TAD, y los percentiles P5%, P50%, y P95% de 
  ##  los datos de residuales.
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  A <- df1 %>%
    mutate(gr = ntile(time, 6)) %>% 
    group_by(gr) %>% 
    summarise(TIME = mean(time), 
              ME = quantile(x = !!y, probs = 0.50), 
              LI = quantile(x = !!y, probs = 0.05), 
              LS = quantile(x = !!y, probs = 0.95))
  ##########################################################################-
  # Crear el gráfico de residuales con las especificaciones mostradas
  g1 <- df1 %>%
    ggplot(mapping = aes(x = !!x, y = !!y)) +
    geom_hline(yintercept = 0) +
    geom_point(col = '#4682B4') +
    coord_cartesian(ylim = c(-2.5, 5.0)) +
    xlab(xlab) + ylab(ylab) +
    stat_smooth(method = 'loess', formula = y ~ x, se = FALSE,
                col = '#EBA213', lty = 'solid', size = 1) +
    # geom_text_repel(data = filter(df1, abs(!!y) > 2), 
                    # aes(label = paste0(ID,' (',time,')')),
                    # size=3.5, ylim=c(4,NA) ) +
    geom_line(data = A, aes(TIME, ME),
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_line(data = A, aes(TIME, LI),
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_line(data = A, aes(TIME, LS),
              col = '#EBA213', lty = 'dashed', size = 1.2)
  return(g1)
}


#-------------------------------------------------------------------------------#
RES_PRE <- function(data, filt, x, y, xlab, ylab) {
  ##########################################################################-
  # Volver las variables x y y en expresiones para evaluación tardóa
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  df1 <- data %>% filter(Resid == filt)
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##  Calculo de percentiles empiricos
  ##  1 Tomar la variable de datos a explorar.
  ##  2 Crear una variable que tome 6 grupos a partir de los datos ordenados 
  ##  con la función dplyr::ntile().
  ##  3 Agrupar por la variable reción creado.
  ##  4 Resumir por la media de TAD, y los percentiles P5%, P50%, y P95% de 
  ##  los datos de residuales.
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  A <- df1 %>%
    mutate(gr = ntile(!!x, 6)) %>% 
    group_by(gr) %>% 
    summarise(PRED = mean(!!x), 
              ME = quantile(x = !!y, probs = 0.50), 
              LI = quantile(x = !!y, probs = 0.05), 
              LS = quantile(x = !!y, probs = 0.95))
  ##########################################################################-
  # Crear el grófico de residuales con las especificaciones mostradas
  df1 %>% 
    ggplot(mapping = aes(x = !!x, y = !!y)) +
    geom_hline(yintercept = 0) +
    geom_point(col = '#4682B4') +
    coord_cartesian(ylim = c(-2.5, 2.5)) +
    xlab(xlab) + ylab(ylab) +
    stat_smooth(method = 'loess', formula = y ~ x, se = FALSE, 
                col = '#EBA213', lty = 'solid', size = 1) +
    geom_line(data = A, aes(PRED, ME), 
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_line(data = A, aes(PRED, LI), 
              col = '#EBA213', lty = 'dashed', size = 1.2) +
    geom_line(data = A, aes(PRED, LS), 
              col = '#EBA213', lty = 'dashed', size = 1.2) %>% 
    return(.)
}
#-------------------------------------------------------------------------------#
# Función que aplica una batería de pruebas de normalidad a los datos
## *data* ingrese el objeto de datos a analizar.
## *vector* ingrese la posición de las columnas donde se encuentran los 
## residuales de interós.
## *alpha* ingrese el valor de probabilidad para los tests. 
normtest_battery <- function(data, var) {
  X          <- pull(data, var)
  df         <- vector('list', 6L)
  df[['SW']] <- shapiro.test(X)$p.value # Shapiro-Wilks
  df[['AD']] <- nortest::ad.test(X)$p.value #Anderson-Darling
  df[['CM']] <- nortest::cvm.test(X)$p.value #Cramer von Mises
  df[['Lf']] <- nortest::lillie.test(X)$p.value #Liliefors
  df[['Pe']] <- nortest::pearson.test(X)$p.value #Pearson
  df[['SF']] <- nortest::sf.test(X)$p.value #Shapiro Francia
  
  return(df)
}

#-------------------------------------------------------------------------------#