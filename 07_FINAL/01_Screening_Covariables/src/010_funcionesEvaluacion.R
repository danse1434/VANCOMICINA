require(rlang)

#-------------------------------------------------------------------------------#
#' Función de correlación
#' 
#' Crear quosures con las variables \code{x} y \code{y}
#' Extraer las variables seleccionadas por x y y como vectores atómicos
#' Crear un carácter con la correlación a 3 dígitos
#'
#' @param df Tabla de datos con variables
#' @param x Variable dependiente (covariable)
#' @param y Variable independiente (desviación eta)
#'
#' @return Correlación entre variables \code{x} y \code{y} dentro de la 
#' tabla especificada
#' @export
#' @examples
#' corr_eq(data, AGEA, eta_Cl_SAEM)
#' 
corr_eq <- function(df, x, y) {
  qx <- enquo(x); qy <- enquo(y)
  
  x <- pull(df, !!qx); y <- pull(df, !!qy)
  
  paste0("r = ", round(cor(x, y), digits = 3)) %>% return(.)
}


#-------------------------------------------------------------------------------#
#' Función de creación de gráficos *et_pf*
#' 
#' Se crea un gráfico con inclusión de valor de correlación entre desviaciones 
#' eta, y covariables continuas.
#' 
#' @param data DataFrame
#' @param x Variable dependiente (covariable)
#' @param y Variable independiente (desviación eta)
#' @param xlab Etiqueta de eje X
#' @param ylab Etiqueta de eje Y 
#' @param col Color de línea y relleno; azul (predeterminado).
#' @param type Tipo de gráfico: 1 dispersión (predeterminado), 2 diagrama 
#' de cajas.
#' @param ymin Valor mínimo del eje Y
#' @param ymax Valor máximo del eje Y
#'
#' @return Gráfico con correlación entre covariable (\code{x}) y desviación 
#' eta (\code{y}), este puede ser dispersión o boxplot. 
#' @export
#' @examples
#' et_pf(AGEA, eta_Cl_SAEM, 'Edad', bquote(eta[Cl]), type = 1)
#' 
et_pf <- function(data, x, y, xlab, ylab, col = "blue", type = 1, 
                  ymin = -0.5, ymax = 0.5) {
  
  xquo <- ensym(x)
  yquo <- ensym(y)
  
  if (type == 1) {
    c <- pull(data,!!xquo)
    xpos <- min(c) + (0.8 * (max(c) - min(c)))
    lab_df <- tribble( ~ x, ~ y, xpos, 0.35)
    
    G1 <- ggplot(data, aes(x = !!xquo, y = !!yquo)) +
      geom_hline(yintercept = 0, lty = 'dashed') +
      geom_point(shape = 22, fill = col, colour = 'black', alpha = 0.5) + 
      stat_smooth(formula = 'y ~ x', method = 'lm', 
                  se = TRUE, colour = col, fill = col, alpha = 0.1) +
      coord_cartesian(ylim = c(ymin, ymax)) +
      theme_bw() + theme(panel.grid = element_line(colour = NA)) +
      ylab(ylab) + xlab(xlab) + 
      geom_text(data = lab_df, aes(x = x, y = y, 
                                   label = corr_eq(data, !!xquo, !!yquo))
      )
    return(G1)
  } else if (type == 2) {
    G2 <- data %>% ggplot(aes(x = factor(!!xquo), y = !!yquo)) +
      geom_hline(yintercept = 0, lty = 'dashed') +
      geom_boxplot(fill = col, colour = 'black', alpha = 0.5) +
      coord_cartesian(ylim = c(ymin, ymax)) +
      theme_bw() + theme(panel.grid = element_line(colour = NA)) +
      ylab(ylab) + xlab(xlab)
    return(G2)
  }
}
#-------------------------------------------------------------------------------#
#' Creación de lista con gráficos para cada variable en el set de datos
#'
#' @param df DataFrame
#' @param x Covariable (variable dependiente)
#' @param col Tema de color de los gráficos
#' @param eta 
#'
#' @return Objeto tipo lista con gráficos para cada covariable de interés
#' @export
#' @examples list_object(eta_Cl_SAEM, 'Cl', col = 'blue')
#' 
list_object <- function(df, eta, x, col) {
  # Función de parada
  stopifnot(is.character(x), is.character(col))
  
  etaquo <- ensym(eta)
  ls     <- list()
  yexpr = bquote(eta[.(x)])
  
  ls[[1]] <- et_pf(df, SEXF, !!etaquo, 'Sexo', yexpr, type = 2, col = col)
  ls[[2]] <- et_pf(df, AGEA, !!etaquo, 'Edad (años)', yexpr, col = col)
  ls[[3]] <- et_pf(df, WTKG, !!etaquo, 'Peso (kg)', yexpr, col = col)
  ls[[4]] <- et_pf(df, HCM, !!etaquo, 'Talla (cm)', yexpr, col = col)
  ls[[5]] <-
    et_pf(df, SCRMGDL, !!etaquo, expression(S[CR] ~ (mg / dL)), yexpr, col = col)
  ls[[6]] <-
    et_pf(df, CLCRMLMIN, !!etaquo, expression(CrCl ~ (mL / min / 1.73 ~ m ^ 2)), 
          yexpr, col = col)
  ls[[7]] <- et_pf(df, PROGDL, !!etaquo, 'Proteínas (g/dL)', yexpr, col = col)
  ls[[8]] <- et_pf(df, ALBGDL, !!etaquo, 'Albúmina (g/dL)', yexpr, col = col)
  
  ls[[9]] <-
    et_pf(df, RAN, !!etaquo, expression(RAN ~ (mm ^ -3)), yexpr, col = col)
  ls[[10]] <- et_pf(df, RAL, !!etaquo, 'RAL (/mm^3)', yexpr, col = col)
  ls[[11]] <- 
    et_pf(df, ANTU, !!etaquo, 'Uso de antibiótico', yexpr, type = 2, col = col)
  
  return(ls)
}