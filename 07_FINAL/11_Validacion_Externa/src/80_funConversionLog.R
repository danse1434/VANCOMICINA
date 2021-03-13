#-------------------------------------------------------------------------------#
#' Title
#'
#' @param media 
#' @param limite_inferior 
#' @param limite_superior 
#' @param n 
#' @param alpha 
#' @param tipo 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
normalSD <- function(media, limite_inferior, limite_superior, n, alpha = 0.05, tipo='max') {
  z <- qnorm(1-alpha/2)
  e1 <- (limite_superior - media)*sqrt(n)/z
  e2 <- (media - limite_inferior)*sqrt(n)/z
  
  if (tipo == 'max') {
    return( max(e1, e2) )
  } else if (tipo == 'comb') {
    return(c(li = e1, ls = e2))
  } else {
    stop('Tipo no reconocido')
  }
}

#-------------------------------------------------------------------------------#
#' Obtener desviación estándar desde intervalo de confianza de 95%
#'
#' Se asume normalidad de la variable
#' 
#' @param media media reportada para variable (valor medio intervalo)
#' @param limite_inferior límite inferior de IC
#' @param limite_superior límite superior de IC 
#' @param n número de muestras para obtención de intervalo
#' @param alpha nivel de significancia (default 0.05)
#' @param tipo tipo de desviación estándar (se sugiere usar el máximo)
#' @param escala escala en donde se debe obtener SD
#'
#' @return
#' @export
#'
#' @examples
#' desvDeInt(3.22, 2.97, 3.48, 50)
#' 
desvDeInt <- function(media, limite_inferior, limite_superior, n, alpha = 0.05, 
                      tipo = 'max', escala = 'norm') {
  z <- qnorm(1-alpha/2)
  
  if (escala == 'norm') {
    e1 <- (limite_superior - media) * sqrt(n) / z
    e2 <- (media - limite_inferior) * sqrt(n) / z
  } else if (escala == 'log') {
    e1 <- (log(limite_superior) - log(media)) * sqrt(n) / z
    e2 <- (log(media) - log(limite_inferior)) * sqrt(n) / z
  } else {
    stop('Escala no reconocida')
  }
  
  if (tipo == 'max') {
    return( max(e1, e2) )
  } else if (tipo == 'comb') {
    return(c(li = e1, ls = e2))
  } else {
    stop('Tipo no reconocido')
  }
  
}

#-------------------------------------------------------------------------------#
#' Aproximar sigma basado en sd en dominio normal
#'
#' @param mu mediana en dominio logarítmico
#' @param var varianza en dominio natural
#' @param interv intervalo a explorar mu
#'
#' @return
#' @export
#'
#' @examples
#' aproximarSigma(1, 0.07500759)
#' 
aproximarSigma <- function(mu, var, interv = c(0, 5)) {
  normForm <- function(mu, s, var) {
    V <- (exp(s ^ 2) - 1) * exp(2 * mu + (s ^ 2))
    # print(c(mu, s, var, V))
    return(abs(V - var))
  }
  
  optimize(normForm, interv, mu = mu, var = var)
}

#-------------------------------------------------------------------------------#
#' Parámetros de dist LogNormal a partir de momentos
#'
#' @param mean media en dominio natural
#' @param var varianza en dominio natural
#'
#' @return 
#' Retorna media y varianza en dominio logarítmico
#' 
#' @export
#'
#' @examples
#' paramMoments(10, 1.4)
#' 
paramMoments <- function(mean, var) {
  mu = log((mean ^ 2) / sqrt(mean ^ 2 + var))
  sigma2 = log(1 + var / (mean^2))
  return(list(mu = mu, sigma2 = sigma2))
}

#-------------------------------------------------------------------------------#
#' Conversión de CV a omega (sd)
#'
#' @param cv coeficiente de variación
#' @param perc cv como porcentaje
#'
#' @return
#' @export
#'
#' @examples
#' cvAOmega(24.3)
#' 
cvAOmega <- function(cv, perc = TRUE) {
  if (perc) {
    cv = cv/100
  }
  
  (cv^2)+1 %>% log() %>% sqrt()
}

#-------------------------------------------------------------------------------#
#' Comparación gráfico en domino normal
#'
#' @param mn_N media dominio natural
#' @param mn_LN media dominio logarítmico
#' @param sd_N sd dominio natural
#' @param sd_LN sd dominio logarítmico
#' @param interv intervalo de eje X
#' @param legpos vector numérico de tamaño 2 o lista con elementos de posición 
#' relativa "pos" (alternativa "inset")
#'
#' @return
#' @export
#'
#' @examples
#' paramMoments(10, 1.4) %>% 
#' {graficoDistr(10, .$mu, 1.4, sqrt(.$sigma2), 
#'              interv = c(0, 30), legpos = list(pos = 'topright'))}
#' 
graficoDistr <- function(mn_N, mn_LN, sd_N, sd_LN, 
                         interv = c(0, 5), legpos = c(0.7, 0.98)) {
  
  x <- seq(interv[1], interv[2], length.out = 1e3)
  y1 <- dnorm(x, mn_N, sd_N)
  y2 <- dlnorm(x, mn_LN, sd_LN)
  
  maxY = max(y1, y2)
  minY = min(y1, y2)
  
  
  # Graficar Plot
  plot(y1 ~ x, type = 'l', col = 'black', lty = 1, 
       xlab = 'x', ylab = 'f(x)', ylim=c(minY, maxY))
  lines(y2 ~ x, type = 'l', col = 'blue', lty = 2)
  
  legSpec <-
    list(
      text = c('Normal', 'Log-Normal'),
      col = c('black', 'blue'),
      lty = c(1:2),
      cex = 0.8
    )
  
  if (is.numeric(legpos)) {
    mapInt <- function(x, A, B, a, b) {
      (x - A) * (b - a) / (B - A) + a
    }
    
    legPositionX <- mapInt(legpos[1], 0, 1, interv[1], interv[2])
    legPositionY <- mapInt(legpos[2], 0, 1, minY, maxY)
    
    legend(legPositionX, legPositionY,
      legend = legSpec$text, col = legSpec$col, lty = legSpec$lty,
      cex = legSpec$cex)
    
  } else if (!is.null(legpos$inset)) {
    legend(legpos$pos, inset = legpos$inset,
      legend = legSpec$text, col = legSpec$col,
      lty = legSpec$lty, cex = legSpec$cex)
  }
  else {
    legend(legpos$pos, legend = legSpec$text,
      col = legSpec$col, lty = legSpec$lty, cex = legSpec$cex)
  }
  
}

#-------------------------------------------------------------------------------#
#' Obtención de media y desviación estándar a partir de {m, q1, q3, n}
#'
#' @param med mediana (segundo cuartil)
#' @param q1 primer cuartil
#' @param q3 tercer cuartil
#' @param n número de muestra
#'
#' @return
#' @export
#'
#' @examples
#' wangScenario3(27.1, 19.3, 36.6, 244)
#' 
#' 
#' wangScenario3(27.1, 19.3, 36.6, 244) %>% 
#' {dnorm(seq(0, 100, length.out = 1e3), .$x, .$s)} %>% 
#'   plot(x=seq(0, 100, length.out = 1e3), type='l')
#' abline(v=19.3, lty=2)
#' abline(v=36.6, lty=2)
#' 
wangScenario3 <- function(med, q1, q3, n){
  x <- (q1 + med + q3) / 3
  s <- (q3 - q1) / (2 * (qnorm((0.75 * n - 0.125) / (n + 0.25))))
  
  return(list(x = x, s = s))
}


