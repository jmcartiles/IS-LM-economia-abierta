# IS-LM en economía abierta sin movilidad de capital ----

# Parámetros
f_is_lm_ea_smk <- function(var_parametros, var_rango_produccion = 0:25000, var_dist_eq = 0.2) {
  
  parametros_inicial <- filter(var_parametros, Escenario == "Inicial")
  parametros_final <- filter(var_parametros, Escenario == "Final")
  
  df.cambios <- diff(as.matrix(var_parametros[,-1]))
  cambios <- colnames(df.cambios)[which(df.cambios != 0)]
  
  var_cambios <- 0
  
  # rango_produccion <- 0:25000
  
  # Funciones
  is_f <- function(produccion, c, t, m, b, C0, I0, G0, XN0, v, R) { 1/b * ( (C0+I0+G0+XN0+v*R) - (1-c*(1-t)-m)*produccion ) }
  lm_f <- function(produccion, f1, f2, mp) { 1/f2 * (f1*produccion-mp) }
  sse_f <- function(produccion, XN0, v, R, m) { XN0 + v*R - m*produccion }
  
  df <- data.frame(produccion = var_rango_produccion,
                   is_0 = is_f(produccion = var_rango_produccion,
                               c = parametros_inicial$c, t = parametros_inicial$t, m = parametros_inicial$m, b = parametros_inicial$b,
                               C0 = parametros_inicial$C0, I0 = parametros_inicial$I0, G0 = parametros_inicial$G0, XN0 = parametros_inicial$XN0,
                               v = parametros_inicial$v, R = parametros_inicial$R),
                   lm_0 = lm_f(produccion = var_rango_produccion, f1 = parametros_inicial$f1, f2 = parametros_inicial$f2, mp = parametros_inicial$mp),
                   sse_0 = sse_f(produccion = var_rango_produccion, XN0 = parametros_inicial$XN0, v = parametros_inicial$v, R = parametros_inicial$R, m = parametros_inicial$m))
  
  # Indicar si hay una nueva IS
  if(any(c("c", "t", "m", "b", "C0", "I0", "G0", "XN0", "v", "R") %in% cambios)) {
    
    df <- df %>%
      mutate(is_1 = is_f(produccion = var_rango_produccion,
                         c = parametros_final$c, t = parametros_final$t, m = parametros_final$m, b = parametros_final$b,
                         C0 = parametros_final$C0, I0 = parametros_final$I0, G0 = parametros_final$G0, XN0 = parametros_final$XN0,
                         v = parametros_final$v, R = parametros_final$R))
    
    is_1_f <- approxfun(df$produccion, df$is_1, rule = 2)
    
    var_cambios <- var_cambios + 1
    cambio_funcion <- "IS"
    
  }
  # Indicar si hay una nueva LM
  if(any(c("f1", "f2", "mp") %in% cambios)) {
    
    df <- df %>%
      mutate(lm_1 = lm_f(produccion = var_rango_produccion, f1 = parametros_final$f1, f2 = parametros_final$f2, mp = parametros_final$mp))
    
    lm_1_f <- approxfun(df$produccion, df$lm_1, rule = 2)
    
    var_cambios <- var_cambios + 1
    if(var_cambios == 1) {cambio_funcion <- "LM"}
    if(var_cambios == 2) {cambio_funcion <- "IS_LM"}
    
  }
  # Indicar si hay una nueva XN
  if(any(c("XN0", "v", "R", "m") %in% cambios)) {
    
    df <- df %>%
      mutate(sse_1 = sse_f(produccion = var_rango_produccion, XN0 = parametros_final$XN0, v = parametros_final$v, R = parametros_final$R, m = parametros_final$m))
    
    sse_1_f <- approxfun(df$produccion, df$sse_1, rule = 2)
    
    var_cambios <- var_cambios + 1
    if(var_cambios == 1) {cambio_funcion <- "SSE"}
    
  }
  
  is_0_f <- approxfun(df$produccion, df$is_0, rule = 2)
  lm_0_f <- approxfun(df$produccion, df$lm_0, rule = 2)
  sse_0_f <- approxfun(df$produccion, df$sse_0, rule = 2)
  
  # Puntos de equilibrio
  # Equilibrio inicial
  y0 <- uniroot(function(x) is_0_f(x) - lm_0_f(x), range(df$produccion))$root
  i0 <- lm_0_f(y0)
  sse0 <- sse_0_f(y0)
  df.equilibrio <- data.frame("Escenario" = "Inicial", y = y0, i = i0, sse = sse0)
  # Equilibrio final
  if(var_cambios != 0) {
    if(cambio_funcion == "IS") {
      y1 <- uniroot(function(x) is_1_f(x) - lm_0_f(x), range(df$produccion))$root
      i1 <- lm_0_f(y1)
      df.equilibrio <- bind_rows(df.equilibrio, data.frame("Escenario" = "Final",y = y1, i = i1, sse = ifelse(exists("sse_1_f"), sse_1_f(y1), sse_0_f(y1))))
    }
    if(cambio_funcion == "LM") {
      y1 <- uniroot(function(x) is_0_f(x) - lm_1_f(x), range(df$produccion))$root
      i1 <- lm_1_f(y1)
      df.equilibrio <- bind_rows(df.equilibrio, data.frame("Escenario" = "Final",y = y1, i = i1, sse = ifelse(exists("sse_1_f"), sse_1_f(y1), sse_0_f(y1))))
    }
    if(cambio_funcion == "IS_LM") {
      y1 <- uniroot(function(x) is_1_f(x) - lm_1_f(x), range(df$produccion))$root
      i1 <- lm_1_f(y1)
      df.equilibrio <- bind_rows(df.equilibrio, data.frame("Escenario" = "Final",y = y1, i = i1, sse = ifelse(exists("sse_1_f"), sse_1_f(y1), sse_0_f(y1))))
    }
  }
  
  
  
  # # Gráfico de situación final
  # if(var_cambios == 0) {
  #   rango_produccion_grafico <- (y0*0.8):(y0*1.2)
  # } else {
  #   rango_produccion_grafico <- (y1*0.8):(y1*1.2)
  #   }
  
  
  if(var_cambios == 0) {
    df <- df %>%
      rename("LM" = "lm_0", "IS" = "is_0")
    df <- df %>%
      filter(produccion > (1-var_dist_eq)*y0, produccion < (1+var_dist_eq)*y0)
    } else {
      if(cambio_funcion == "IS") df <- rename(df, "LM" = "lm_0")
      if(cambio_funcion == "LM") df <- rename(df, "IS" = "is_0")
      if(cambio_funcion == "SSE") df <- rename(df, "SSE" = "sse_0")
      df <- df %>%
        filter(produccion > min(c((1-var_dist_eq)*y0, (1+var_dist_eq)*y1)), produccion < max(c((1-var_dist_eq)*y0, (1+var_dist_eq)*y1)))
    }
  
  df.is.lm <- df %>%
    select(-contains("sse")) %>%
    pivot_longer(cols = !produccion, names_to = "funcion", values_to = "tipo_interes") %>%
    mutate(situacion = ifelse(grepl("0", funcion), "Inicial", "Final"),
           funcion = str_replace_all(funcion, c("is_" = "IS ", "lm_" = "LM ")))
  
  df.sse <- df %>%
    select(-matches("is|lm")) %>%
    pivot_longer(cols = !produccion, names_to = "funcion", values_to = "sse") %>%
    mutate(situacion = ifelse(grepl("0", funcion), "Inicial", "Final"),
           funcion = str_replace_all(funcion, c("sse_" = "SSE ")))
  
  # if(var_cambios == 0) {
  #   df.is.lm <- df.is.lm %>%
  #     filter(tipo_interes > i0-10, tipo_interes < i0+10)
  # } else {
  #   df.is.lm <- df.is.lm %>%
  #     filter(tipo_interes > i0-10, tipo_interes < i0+10)
  # }

  grafico_is_lm <- ggplot(data = df.is.lm, aes(x = produccion, y = tipo_interes, color = funcion)) +
    geom_path(size = 1, aes(linetype = situacion)) +
    scale_color_manual(breaks = c("IS", "IS 0", "IS 1", "LM", "LM 0", "LM 1"),
                       values = c("#FF4036", "#FF4036", "#FF4036", "#0073D9", "#0073D9", "#0073D9")) +
    # scale_color_manual(values = c("#FF4036", "#FF4036", "#0073D9", "#0073D9")) +
    geom_segment(aes(x = y0, y = min(tipo_interes), xend = y0, yend = i0), lty = "dotted", color = "black") +
    geom_segment(aes(x = min(produccion), y = i0, xend = y0, yend = i0), lty = "dotted", color = "black") +
    geom_segment(aes(x = df.equilibrio$y[2], y = min(tipo_interes), xend = df.equilibrio$y[2], yend = df.equilibrio$i[2]), lty = "dotted", color = "black") +
    geom_segment(aes(x = min(produccion), y = df.equilibrio$i[2], xend = df.equilibrio$y[2], yend = df.equilibrio$i[2]), lty = "dotted", color = "black") +
    geom_point(data = df.equilibrio[,-1], aes(x = y, y = i), size = 2, color = "black") +
    # geom_point(aes(x = y1, y = i1), size = 2, color = "black") +
    scale_y_continuous(breaks = round(df.equilibrio$i, 2)) +
    scale_x_continuous(breaks = round(df.equilibrio$y, 0)) +
    labs(x = "Producción (Y)",
         y = "Tipo de interés (i)",
         title = "IS-LM en economía abierta sin movilidad de capital") +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    guides(linetype = FALSE)

  grafico_sse <- ggplot(data = df.sse, aes(x = produccion, y = sse, color = funcion)) +
    geom_path(size = 1, aes(linetype = situacion)) +
    scale_color_manual(breaks = c("SSE", "SSE 0", "SSE 1"),
                       values = rep(RColorBrewer::brewer.pal(3, "Set1")[3], 3)) +
    # scale_color_manual(values = c("#FF4036", "#FF4036", "#0073D9", "#0073D9")) +
    geom_segment(aes(x = y0, y = min(sse), xend = y0, yend = df.equilibrio$sse[1]), lty = "dotted", color = "black") +
    geom_segment(aes(x = min(produccion), y = df.equilibrio$sse[1], xend = y0, yend = df.equilibrio$sse[1]), lty = "dotted", color = "black") +
    geom_segment(aes(x = df.equilibrio$y[2], y = min(sse), xend = df.equilibrio$y[2], yend = df.equilibrio$sse[2]), lty = "dotted", color = "black") +
    geom_segment(aes(x = min(produccion), y = df.equilibrio$sse[2], xend = df.equilibrio$y[2], yend = df.equilibrio$sse[2]), lty = "dotted", color = "black") +
    geom_point(data = df.equilibrio[,-1], aes(x = y, y = sse), size = 2, color = "black") +
    scale_y_continuous(breaks = round(df.equilibrio$sse, 0)) +
    scale_x_continuous(breaks = round(df.equilibrio$y, 0)) +
    # geom_point(aes(x = y1, y = i1), size = 2, color = "black") +
    labs(x = "Producción (Y)",
         y = "SSE",
         title = "Saldo del sector exterior") +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    guides(linetype = FALSE)
  if(min(df.sse$sse) <= 0) grafico_sse <- grafico_sse + geom_hline(yintercept=0)
  
  resultados <- list(equilibrio = df.equilibrio, grafico_is_lm = grafico_is_lm, grafico_sse = grafico_sse)
  
  return(resultados)
  
}

# prueba <- f_is_lm_ea_smk(read_xlsx("parametros.xlsx"))
# 
# prueba$equilibrio
# prueba$grafico_is_lm
# prueba$grafico_sse





















