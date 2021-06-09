library(readxl)
library(tidyverse)

df.parametros <- read_xlsx("parametros.xlsx")
var_parametros = df.parametros
var_rango_produccion = 0:25000
var_dist_eq = 0.2
var_color <- c("#FF4036", "#0073D9", "#4DAF4A")
var_movilidad_capital = "mpk"
var_tipo_cambio = "fijo"


parametros_inicial <- filter(var_parametros, Escenario == "Inicial")
parametros_final <- filter(var_parametros, Escenario == "Final")

df.cambios <- diff(as.matrix(var_parametros[,-1]))
cambios <- colnames(df.cambios)[which(df.cambios != 0)]

var_cambios <- 0

# Funciones
is_f <- function(produccion, c, t, m, b, C0, I0, G0, XN0, v, e, p_int, p_dom) { 1/b * ( (C0+I0+G0+XN0+v*e*p_int/p_dom) - (1-c*(1-t)-m)*produccion ) }
lm_f <- function(produccion, f1, f2, M, p_dom) { 1/f2 * (f1*produccion-M/p_dom) }
sse_f <- function(produccion, XN0, v, e, p_int, p_dom, m) { XN0 + v*e*p_int/p_dom - m*produccion }
e_f <- function(var_e, var_cambio_i) {var_e + var_cambio_i}

df <- data.frame(produccion = var_rango_produccion,
                 is_0 = is_f(produccion = var_rango_produccion,
                             c = parametros_inicial$c, t = parametros_inicial$t, m = parametros_inicial$m, b = parametros_inicial$b,
                             C0 = parametros_inicial$C0, I0 = parametros_inicial$I0, G0 = parametros_inicial$G0, XN0 = parametros_inicial$XN0,
                             v = parametros_inicial$v, e = parametros_inicial$e, p_int = parametros_inicial$p_int, p_dom = parametros_inicial$p_dom),
                 lm_0 = lm_f(produccion = var_rango_produccion, f1 = parametros_inicial$f1, f2 = parametros_inicial$f2,
                             M = parametros_inicial$M, p_dom = parametros_inicial$p_dom),
                 sse_0 = sse_f(produccion = var_rango_produccion, XN0 = parametros_inicial$XN0, v = parametros_inicial$v,
                               e = parametros_inicial$e, p_int = parametros_inicial$p_int, p_dom = parametros_inicial$p_dom, m = parametros_inicial$m))

# Indicar si hay una nueva IS
if(any(c("c", "t", "m", "b", "C0", "I0", "G0", "XN0", "v", "e", "p_int", "p_dom") %in% cambios)) {
  
  df <- df %>%
    mutate(is_1 = is_f(produccion = var_rango_produccion,
                       c = parametros_final$c, t = parametros_final$t, m = parametros_final$m, b = parametros_final$b,
                       C0 = parametros_final$C0, I0 = parametros_final$I0, G0 = parametros_final$G0, XN0 = parametros_final$XN0,
                       v = parametros_final$v, e = parametros_final$e, p_int = parametros_final$p_int, p_dom = parametros_final$p_dom))
  
  is_1_f <- approxfun(df$produccion, df$is_1, rule = 2)
  
  var_cambios <- var_cambios + 1
  cambio_funcion <- "IS"
  
}
# Indicar si hay una nueva LM
if(any(c("f1", "f2", "M", "p_dom") %in% cambios)) {
  
  df <- df %>%
    mutate(lm_1 = lm_f(produccion = var_rango_produccion, f1 = parametros_final$f1, f2 = parametros_final$f2,
                       M = parametros_final$M, p_dom = parametros_final$p_dom))
  
  lm_1_f <- approxfun(df$produccion, df$lm_1, rule = 2)
  
  var_cambios <- var_cambios + 1
  if(var_cambios == 1) {cambio_funcion <- "LM"}
  if(var_cambios == 2) {cambio_funcion <- "IS_LM"}
  
}
# Indicar si hay una nueva XN
if(any(c("XN0", "v", "e", "p_int", "p_dom", "m") %in% cambios)) {
  
  df <- df %>%
    mutate(sse_1 = sse_f(produccion = var_rango_produccion, XN0 = parametros_final$XN0, v = parametros_final$v,
                         e = parametros_final$e, p_int = parametros_final$p_int, p_dom = parametros_final$p_dom, m = parametros_final$m))
  
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

# Movilidad perfecta de capital

# Tipo de cambio fijo
# El BC interviene en la economía
# Calcular la nueva oferta monetaria
if((var_movilidad_capital == "mpk" & var_tipo_cambio == "fijo")) {
  
  # Si hay cambios en la IS
  # Nivel de renta con la nueva IS para el tipo de interés inicial
  y2 <- 1/(1-parametros_final$c*(1-parametros_final$t)-parametros_final$m)*
    (parametros_final$C0+parametros_final$I0+parametros_final$G0+parametros_final$XN0+
       parametros_final$v*parametros_final$e*parametros_final$p_int/parametros_final$p_dom -parametros_final$b*i0)
  # Calcular la oferta monetaria con la que se alcanza el nuevo nivel de renta con el tipo de interés inicial
  M2 <- parametros_final$p_dom*(parametros_final$f1*y2 - parametros_final$f2*i0)
  
  df <- df %>%
    mutate(lm_2 = lm_f(produccion = var_rango_produccion, f1 = parametros_final$f1, f2 = parametros_final$f2, M = M2, p_dom = parametros_final$p_dom))
  
  lm_2_f <- approxfun(df$produccion, df$lm_2, rule = 2)
  # var_cambios <- var_cambios + 1
  # if(var_cambios == 1) {cambio_funcion <- "LM"}
  # if(var_cambios == 2) {cambio_funcion <- "IS_LM"}
  df.equilibrio <- bind_rows(df.equilibrio %>% filter(Escenario == "Inicial"), data.frame("Escenario" = "Final", y = y2, i = i0, sse = ifelse(exists("sse_1_f"), sse_1_f(y2), sse_0_f(y2))))
  
}

# Tipo de cambio flexible
# Calcular la nueva oferta monetaria
if((var_movilidad_capital == "mpk" & var_tipo_cambio == "flexible")) {
  
  # Si hay cambios en la IS
  # Las variaciones en el tipo de interés afectan al tipo de cambio
  if(i1 - i0 != 0) {
    
    e1 <- e_f(var_e = parametros_final$e, var_cambio_i = i1 - i0)
    e1 <- (y0*parametros_final$b + (1-parametros_final$c*(1-parametros_final$t)-parametros_final$m)*y0  - 
             (parametros_final$C0+parametros_final$I0+parametros_final$G0+parametros_final$XN0)) * 1/parametros_final$v*parametros_final$p_dom/parametros_final$p_int
      1/b * ( (C0+I0+G0+XN0+v*e*p_int/p_dom) - (1-c*(1-t)-m)*produccion )
    # Variaciones en IS y SSE debido al cambio en el tipo de cambio nominal
    y2 <- 1/(1-parametros_final$c*(1-parametros_final$t)-parametros_final$m)*
      (parametros_final$C0+parametros_final$I0+parametros_final$G0+parametros_final$XN0+parametros_final$v*e1* -parametros_final$b*i0)
    sse2 <- sse_f(produccion = y2, XN0 = parametros_final$XN0, v = parametros_final$v,
                  e = e1, p_int = parametros_final$p_int, p_dom = parametros_final$p_dom, m = parametros_final$m)
    
    df <- df %>%
      mutate(is_2 = is_f(produccion = var_rango_produccion,
                         c = parametros_final$c, t = parametros_final$t, m = parametros_final$m, b = parametros_final$b,
                         C0 = parametros_final$C0, I0 = parametros_final$I0, G0 = parametros_final$G0, XN0 = parametros_final$XN0,
                         v = parametros_final$v, e = e1, p_int = parametros_final$p_int, p_dom = parametros_final$p_dom),
             sse_2 = sse_f(produccion = var_rango_produccion, XN0 = parametros_final$XN0, v = parametros_final$v,
                           e = e1, p_int = parametros_final$p_int, p_dom = parametros_final$p_dom, m = parametros_final$m))
    
    is_2_f <- approxfun(df$produccion, df$is_2, rule = 2)
    sse_2_f <- approxfun(df$produccion, df$sse_2, rule = 2)
    
    }
  





  
}



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
  separate(funcion, into = c("IS_LM", "situacion"), sep = "_", remove = FALSE) %>%
  replace_na(list(situacion = 0)) %>%
  mutate(funcion = toupper(str_replace(funcion, "_", " ")),
         IS_LM = toupper(IS_LM)) %>%
  group_by(IS_LM) %>%
  mutate(situacion = case_when(situacion == max(situacion) ~ "Final",
                               situacion > min(situacion) ~ "Intermedia",
                               TRUE ~ "Inicial"))
  # mutate(situacion = ifelse(grepl("0", funcion), "Inicial", "Final"),
  #        funcion = str_replace_all(funcion, c("is_" = "IS ", "lm_" = "LM ")))

# if(any(grepl("2", df.is.lm$funcion))) {
#   df.is.lm <- df.is.lm %>%
#     mutate(situacion = ifelse(grepl("1")))
# }

df.sse <- df %>%
  select(-matches("is|lm")) %>%
  pivot_longer(cols = !produccion, names_to = "funcion", values_to = "sse") %>%
  separate(funcion, into = c("IS_LM", "situacion"), sep = "_", remove = FALSE) %>%
  replace_na(list(situacion = 0)) %>%
  mutate(funcion = toupper(str_replace(funcion, "_", " ")),
         IS_LM = toupper(IS_LM)) %>%
  group_by(IS_LM) %>%
  mutate(situacion = case_when(situacion == max(situacion) ~ "Final",
                               situacion > min(situacion) ~ "Intermedia",
                               TRUE ~ "Inicial"))
  # mutate(situacion = ifelse(grepl("0", funcion), "Inicial", "Final"),
  #        funcion = str_replace_all(funcion, c("sse_" = "SSE ")))


# # Gráfico de situación final
# if(var_cambios == 0) {
#   rango_produccion_grafico <- (y0*0.8):(y0*1.2)
# } else {
#   rango_produccion_grafico <- (y1*0.8):(y1*1.2)
#   }

grafico_is_lm <- ggplot(data = df.is.lm, aes(x = produccion, y = tipo_interes, color = funcion)) +
  geom_path(size = 1, aes(linetype = situacion)) +
  scale_color_manual(breaks = c("IS", "IS 0", "IS 1", "LM", "LM 0", "LM 1", "LM 2"),
                     values = c(rep(var_color[1], 3), rep(var_color[2], 3))) +
  # scale_color_manual(values = c("#FF4036", "#FF4036", "#0073D9", "#0073D9")) +
  geom_segment(aes(x = y0, y = min(tipo_interes), xend = y0, yend = i0), lty = "dotted", color = "black") +
  geom_segment(aes(x = min(produccion), y = i0, xend = y0, yend = i0), lty = "dotted", color = "black") +
  geom_segment(aes(x = df.equilibrio$y[-1], y = min(tipo_interes), xend = df.equilibrio$y[-1], yend = df.equilibrio$i[-1]), lty = "dotted", color = "black") +
  geom_segment(aes(x = min(produccion), y = df.equilibrio$i[-1], xend = df.equilibrio$y[-1], yend = df.equilibrio$i[-1]), lty = "dotted", color = "black") +
  geom_point(data = df.equilibrio[,-1], aes(x = y, y = i), size = 2, color = "black") +
  # geom_point(aes(x = y1, y = i1), size = 2, color = "black") +
  scale_y_continuous(breaks = round(df.equilibrio$i, 2)) +
  scale_x_continuous(breaks = round(df.equilibrio$y, 0)) +
  labs(x = "Producción (Y)",
       y = "Tipo de interés (i)"#,
       # title = "IS-LM en economía abierta sin movilidad de capital"
       ) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(linetype = FALSE)
grafico_is_lm


grafico_sse <- ggplot(data = df.sse, aes(x = produccion, y = sse, color = funcion)) +
  geom_path(size = 1, aes(linetype = situacion)) +
  scale_color_manual(breaks = c("SSE", "SSE 0", "SSE 1"),
                     values = rep(var_color[3], 3)) +
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
       y = "SSE"#,
       # title = "Saldo del sector exterior"
       ) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(linetype = FALSE)
if(min(df.sse$sse) <= 0) grafico_sse <- grafico_sse + geom_hline(yintercept=0)
grafico_sse












