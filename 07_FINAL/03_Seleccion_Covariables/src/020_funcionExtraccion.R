#' Title
#'
#' @param conexion 
#' @param parSpace 
#' @param modelPars 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
conversionTabular <-
  function(conexion,
           parSpace = 22,
           modelPars = c('Cl', 'V1', 'Q', 'V2')) {
    x <- readLines(conexion)
    
    # 1. Extracción de especificaciones de modelo
    x1 <- x[which(str_detect(x, '\\|'))]
    
    x2 <- data.frame('x1' = x1) %>%
      separate(
        col = x1,
        sep = '\\|',
        into = paste0('A', 1:parSpace),
        remove = FALSE
      ) %>%
      select(-x1,-A1) %>%
      mutate(across(everything(), ~ str_replace_all(.x, '\\s{1,}', ' '))) %>%
      mutate(across(everything(), ~ str_replace_all(.x, '(^\\s|\\s$)', ''))) %>%
      as_tibble()
    
    vecCols <- as.character(x2[1, ][1:(dim(x2)[2] - 1)])
    vecCols <- c('parametro', vecCols)
    
    x3 <- x2 %>% filter(A2 %in% modelPars)
    
    colnames(x3) <- vecCols
    
    ll1 <- x[which(str_detect(x, '-2\\s\\*\\sLL'))]
    ll2 <- x[which(str_detect(x, 'BICc'))]
    
    ll_optm <- x[which(str_detect(x, 'Best Model'))] %>%
      str_replace_all('[^\\d\\.]', '') %>%
      as.numeric()
    
    LL <- data.frame(OFV = ll1, BICc = ll2) %>%
      mutate(across(everything(), ~ str_replace_all(.x, '[^\\d\\.]', ''))) %>%
      mutate(across(everything(), ~ as.numeric(.x))) %>%
      rownames_to_column(var = 'iteracion') %>%
      mutate(optimo = ifelse(iteracion == ll_optm, TRUE, FALSE)) %>%
      as_tibble()
    
    #
    x4 <- x3 %>%
      mutate(across(!matches('parametro'), ~ ifelse(.x == 'X', 1, 0))) %>%
      add_column(iteracion = rep(LL$iteracion, each = length(modelPars)),
                 .before = 'parametro') %>%
      left_join(LL, by = 'iteracion')
    
    return(x4)
  }