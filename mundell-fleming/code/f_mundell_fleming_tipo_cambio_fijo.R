# Movilidad perfecta de capitales ----
mf_tc_fijo <- function(data, var_rango_produccion, cambios, parametros_inicial, parametros_final, var_dist_eq, df.equilibrio) {
  
  df <- data
  
  is_0_f <- approxfun(df$produccion, df$is_0, rule = 2)
  lm_0_f <- approxfun(df$produccion, df$lm_0, rule = 2)
  xn_0_f <- approxfun(df$produccion, df$xn_0, rule = 2)
  
  # Puntos de equilibrio
  # Equilibrio inicial
  y0 <- uniroot(function(x) is_0_f(x) - lm_0_f(x), range(df$produccion))$root
  i0 <- lm_0_f(y0)
  xn0 <- xn_0_f(y0)
  df.equilibrio <- data.frame("Escenario" = "Inicial", y = y0, i = i0, xn = xn0,
                              e = parametros_inicial$e, recaudacion = parametros_inicial$t*y0, M = parametros_inicial$M)
  
  cambio_f <- "cambio_funciones_"
  # Indicar si hay una nueva IS
  # if(any(c("c", "t", "m", "b", "C0", "I0", "G0", "XN0", "v", "e", "p_int", "p_dom", "n", "Y_int") %in% cambios)) {
  if(any(c("c", "t", "m", "b", "C0", "I0", "G0", "XN0", "v", "e", "p_int", "p_dom") %in% cambios)) {
    
    # cambio en la IS
    df <- df %>%
      mutate(is_1 = is_f(produccion = var_rango_produccion,
                         alfa = parametros_final$alfa, b = parametros_final$b, C0 = parametros_final$C0,
                         I0 = parametros_final$I0, G0 = parametros_final$G0, XN0 = parametros_final$XN0,
                         # Y_int = parametros_final$Y_int, n = parametros_final$n,
                         v = parametros_final$v, e = parametros_final$e, p_int = parametros_final$p_int, p_dom = parametros_final$p_dom))
    
    is_1_f <- approxfun(df$produccion, df$is_1, rule = 2)
    cambio_f <- paste0(cambio_f, "IS_")
  }
  
  # Indicar si hay una nueva LM
  if(any(c("k", "h", "M", "p_dom") %in% cambios)) {
    
    df <- df %>%
      mutate(lm_1 = lm_f(produccion = var_rango_produccion, k = parametros_final$k, h = parametros_final$h,
                         M = parametros_final$M, p_dom = parametros_final$p_dom))
    
    lm_1_f <- approxfun(df$produccion, df$lm_1, rule = 2)
    cambio_f <- paste0(cambio_f, "LM_")
  }
  
  # Indicar si hay una nueva XN
  # if(any(c("XN0", "v", "e", "p_int", "p_dom", "m", "n", "Y_int") %in% cambios)) {
  if(any(c("XN0", "v", "e", "p_int", "p_dom", "m") %in% cambios)) {
    
    df <- df %>%
      mutate(xn_1 = xn_f(produccion = var_rango_produccion, XN0 = parametros_final$XN0, v = parametros_final$v,
                         # Y_int = parametros_final$Y_int, n = parametros_final$n,
                         e = parametros_final$e, p_int = parametros_final$p_int, p_dom = parametros_final$p_dom, m = parametros_final$m))
    
    xn_1_f <- approxfun(df$produccion, df$xn_1, rule = 2)
    cambio_f <- paste0(cambio_f, "XN")
    
  }
  
  if(grepl("IS|LM|XN", cambio_f)) {
    if(grepl("IS|XN", cambio_f)) {
      # Nivel de renta con la nueva IS para el tipo de interés inicial
      # y2 <- parametros_final %>% mutate(y2 = alfa*(C0+G0+I0+XN0+n*Y_int+v*e*p_int/p_dom-b*i0)) %>% pull(y2)
      y2 <- parametros_final %>% mutate(y2 = alfa*(C0+G0+I0+XN0+v*e*p_int/p_dom-b*i0)) %>% pull(y2)
      # Calcular la oferta monetaria con la que se alcanza el nuevo nivel de renta con el tipo de interés inicial
      M2 <- parametros_final %>% mutate(M2 = p_dom*(k*y2 - h*i0)) %>% pull(M2)
    } else {
      # # Tipo de interés con la nueva LM
      # i2 <- parametros_final %>% mutate(i2 = (k*y2-M/p_dom)/h) %>% pull(i2)
      # Calcular la oferta monetaria con la que se alcanza el tipo de interés inicial
      M2 <- parametros_final %>% mutate(M2 = p_dom*(k*y0 - h*i0)) %>% pull(M2)
      y2 <- y0
    }
    
    df <- df %>%
      mutate(lm_2 = lm_f(produccion = var_rango_produccion, k = parametros_final$k,
                         h = parametros_final$h, M = M2, p_dom = parametros_final$p_dom))
    lm_2_f <- approxfun(df$produccion, df$lm_2, rule = 2)
    
    df.equilibrio <- bind_rows(df.equilibrio %>% filter(Escenario == "Inicial"),
                               data.frame("Escenario" = "Final", y = y2, i = i0, xn = ifelse(exists("xn_1_f"), xn_1_f(y2), xn_0_f(y2)),
                                          e = parametros_final$e, recaudacion = parametros_final$t*y2, M = M2))
    
    df <- df %>%
      filter(produccion > min(c((1-var_dist_eq)*y0, (1+var_dist_eq)*y2)), produccion < max(c((1-var_dist_eq)*y0, (1+var_dist_eq)*y2)))
  } else {
    df <- df %>%
      rename("LM" = "lm_0", "IS" = "is_0")
    df <- df %>%
      filter(produccion > (1-var_dist_eq)*y0, produccion < (1+var_dist_eq)*y0)
  }
  
  list.resultados <- list(df, df.equilibrio)
  
  return(list.resultados)
  
}