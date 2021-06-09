# Cargar librería necesarias
library(tidyverse)
library(readxl)

# Parameters
c0 <- 400
c1 <- 0.25
I0 <- 300
d1 <- 0.25
d2 <- 1500
G <- 600
TT <- 400
M_P <- 3000
f1 <- 2
f2 <- 12000


# IS ----
A <- c0 - c1*TT + I0 + G
# i <- A/d2 - (1-c1-d1)/d2 * y

is_f <- function(y) A/d2 - (1-c1-d1)/d2 * y

# LM ----
lm_f <- function(y) 1/f2*(f1*y-M_P)


x <- 0:100000

# Equilibrium production
y <- uniroot(function(x) is_f(x) - lm_f(x), range(x))$root

# Equilibrium interest rate
i <- lm_f(y)

# Gráfico de situación inicial
x0 <- (y*0.9):(y*1.1)

chart <- ggplot() +
  stat_function(data = data.frame(x0), aes(x0, color = "IS"), fun = is_f) +
  stat_function(data = data.frame(x0), aes(x0, color = "LM"), fun = lm_f)
chart

chart <- chart + annotate("point", x = y, y = i, color = "grey30")
chart

chart <- chart + 
  annotate("segment", x = y, xend = y, y = 0, yend = i,
           linetype = "dashed", color = "grey30") +
  annotate("segment", x = min(x0), xend = y, y = i, yend = i,
           linetype = "dashed", color = "grey30")
chart

chart <- chart + 
  # scale_x_continuous(expand = c(0, 0), 
  #                    breaks = y, labels = "p*") +
  # scale_y_continuous(expand = c(0, 0), 
  #                    breaks = i, labels = "i*") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1),
        legend.spacing = unit(0, "cm"), 
        legend.margin = margin(0, 0, 0, 0, "cm")) +
  labs(x = "Producción (Y)", y = "Tipo de interés (i)", 
       color = NULL, fill = NULL)
chart

# Gráfico situación final
M_P_1 <- 3500

# IS ----
A <- c0 - c1*TT + I0 + G
# i <- A/d2 - (1-c1-d1)/d2 * y

is_f <- function(y) A/d2 - (1-c1-d1)/d2 * y

# LM ----
lm_f_1 <- function(y) 1/f2*(f1*y-M_P_1)

x <- 0:100000

# Equilibrium production
y <- uniroot(function(x) is_f(x) - lm_f_1(x), range(x))$root

# Equilibrium interest rate
i <- lm_f_1(y)

# Gráfico de situación final
x1 <- (y*0.9):(y*1.1)

chart <- chart +
  stat_function(data = data.frame(x1), aes(x1, color = "LM 1"), fun = lm_f_1)

chart <- chart + 
  annotate("segment", x = y, xend = y, y = 0, yend = i,
           linetype = "dashed", color = "grey30") +
  annotate("segment", x = min(x0), xend = y, y = i, yend = i,
           linetype = "dashed", color = "grey30")


chart

chart <- chart + annotate("point", x = y, y = i, color = "grey30")
chart





# IS-LM en economía abierta sin movilidad de capitales ----

# Parámetros
parametros <- read_excel("parametros.xlsx")
parametros_inicial <- filter(parametros, Escenario == "inicial")
parametros_final <- filter(parametros, Escenario == "final")

df.cambios <- diff(as.matrix(parametros[,-1]))
cambios <- colnames(df.cambios)[which(df.cambios > 0)]

var_cambios <- 0

if(is_empty(cambios)) {
  
  # situación inicial
  var_cambios <- 0
  
} else {
  
  
  
  
  
}


rango_produccion <- 0:25000

# Funciones
is_f <- function(produccion, c, t, m, b, C0, I0, G0, XN0, v, R) { 1/b * ( (C0+I0+G0+XN0+v*R) - (1-c*(1-t)-m)*produccion ) }
lm_f <- function(produccion, f1, f2, mp) { 1/f2 * (f1*produccion-mp) }

df <- data.frame(produccion = rango_produccion,
                 is_0 = is_f(produccion = rango_produccion,
                                             c = parametros_inicial$c, t = parametros_inicial$t, m = parametros_inicial$m, b = parametros_inicial$b,
                                             C0 = parametros_inicial$C0, I0 = parametros_inicial$I0, G0 = parametros_inicial$G0, XN0 = parametros_inicial$XN0,
                                             v = parametros_inicial$v, R = parametros_inicial$R),
                 lm_0 = lm_f(produccion = rango_produccion, f1 = parametros_inicial$f1, f2 = parametros_inicial$f2, mp = parametros_inicial$mp))

# Indicar si hay una nueva IS
if(any(c("c", "t", "m", "b", "C0", "I0", "G0", "XN0", "v", "R") %in% cambios)) {
  
  df <- df %>%
    mutate(is_1 = is_f(produccion = rango_produccion,
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
    mutate(lm_1 = lm_f(produccion = rango_produccion, f1 = parametros_final$f1, f2 = parametros_final$f2, mp = parametros_final$mp))
  
  lm_1_f <- approxfun(df$produccion, df$lm_1, rule = 2)
  
  var_cambios <- var_cambios + 1
  if(var_cambios == 1) {cambio_funcion <- "LM"}
  if(var_cambios == 2) {cambio_funcion <- "IS_LM"}
  
}

is_0_f <- approxfun(df$produccion, df$is_0, rule = 2)
lm_0_f <- approxfun(df$produccion, df$lm_0, rule = 2)

# Puntos de equilibrio
# Equilibrio inicial
y0 <- uniroot(function(x) is_0_f(x) - lm_0_f(x), range(df$produccion))$root
i0 <- lm_0_f(y0)
df.equilibrio <- data.frame(y = y0, i = i0)
# Equilibrio final
if(var_cambios != 0) {
  if(cambio_funcion == "IS") {
    y1 <- uniroot(function(x) is_1_f(x) - lm_0_f(x), range(df$produccion))$root
    i1 <- lm_0_f(y1)
    df.equilibrio <- bind_rows(df.equilibrio, data.frame(y = y1, i = i1))
    }
  if(cambio_funcion == "LM") {
    y1 <- uniroot(function(x) is_0_f(x) - lm_1_f(x), range(df$produccion))$root
    i1 <- lm_1_f(y1)
    df.equilibrio <- bind_rows(df.equilibrio, data.frame(y = y1, i = i1))
    }
  if(cambio_funcion == "IS_LM") {
    y1 <- uniroot(function(x) is_1_f(x) - lm_1_f(x), range(df$produccion))$root
    i1 <- lm_1_f(y1)
    df.equilibrio <- bind_rows(df.equilibrio, data.frame(y = y1, i = i1))
    }
}



# Gráfico de situación final
rango_produccion_grafico <- (y1*0.5):(y1*1.5)

df <- df %>%
  filter(produccion < y1*1.2, produccion > y1*0.8) %>%
  pivot_longer(cols = !produccion, names_to = "funcion", values_to = "tipo_interes") %>%
  mutate(situacion = ifelse(grepl("0", funcion), "Inicial", "Final"),
         funcion = str_replace(funcion, c("is_", "lm_"), c("IS ", "LM ")))

ggplot(data = df, aes(x = produccion, y = tipo_interes, color = funcion)) +
  geom_path(size = 1, aes(linetype = situacion)) +
  scale_color_manual(values = c("#FF4036", "#FF4036", "#0073D9", "#0073D9")) +
  # geom_segment(aes(x = y0, y = min(tipo_interes), xend = y0, yend = i0), lty = "dotted") +
  # geom_segment(aes(x = min(produccion), y = i0, xend = y0, yend = i0), lty = "dotted") +
  # geom_segment(aes(x = y1, y = min(tipo_interes), xend = y1, yend = i1), lty = "dotted") +
  # geom_segment(aes(x = min(produccion), y = i1, xend = y1, yend = i1), lty = "dotted") +
  geom_point(data = df.equilibrio, aes(x = y, y = i), size = 2, color = "black") +
  # geom_point(aes(x = y1, y = i1), size = 2, color = "black") +
  labs(x = "Producción (Y)",
       y = "Tipo de interés (i)",
       title = "IS-LM en economía abierta sin movilidad de capitales") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(linetype = FALSE)


















y0 <- uniroot(function(x) is_0_f(x) - lm_0_f(x), range(df$produccion))$root
y1 <- uniroot(function(x) is_1_f(x) - lm_0_f(x), range(df$produccion))$root

# Tipo de interés de equilibrio
i0 <- lm_0_f(y0)
i1 <- lm_0_f(y1)

# Gráfico de situación final
rango_produccion_grafico <- (y1*0.5):(y1*1.5)

df <- df %>%
  filter(produccion < y1*1.2, produccion > y1*0.8)

ggplot() +
  geom_path(data = df, color = "#FF4036", size = 1, aes(x = produccion, y = is_0), linetype = "dashed") +
  geom_path(data = df, color = "#FF4036", size = 1, aes(x = produccion, y = is_1)) +
  geom_path(data = df, color = "#0073D9", size = 1, aes(x = produccion, y = lm_0)) +
  geom_segment(aes(x = y0, y = min(df$is_0, df$is_1, df$lm_0), xend = y0, yend = i0), lty = "dotted") +
  geom_segment(aes(x = min(df$produccion), y = i0, xend = y0, yend = i0), lty = "dotted") +
  geom_segment(aes(x = y1, y = min(df$is_0, df$is_1, df$lm_0), xend = y1, yend = i1), lty = "dotted") +
  geom_segment(aes(x = min(df$produccion), y = i1, xend = y1, yend = i1), lty = "dotted") +
  geom_point(aes(x = y0, y = i0), size = 2) +
  geom_point(aes(x = y1, y = i1), size = 2) +
  scale_x_continuous(expand = c(0, 0), breaks = c(y0, y1),
                     labels = round(c(y0, y1), 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(i0, i1),
                     labels = round(c(i0, i1), 2)) +
  labs(x = "Producción (Y)",
       y = "Tipo de interés (i)",
       title = "IS-LM en economía abierta sin movilidad de capitales") +
  theme_classic()


ggplot(data = df) +
  geom_path(data = df, size = 1, aes(x = produccion, y = is_0, color = "IS 0"), linetype = "dashed") +
  geom_path(data = df, size = 1, aes(x = produccion, y = is_1, color = "IS 1")) +
  geom_path(data = df, size = 1, aes(x = produccion, y = lm_0, color = "LM 0")) +
  scale_color_manual(values = c("#FF4036", "#FF4036", "#0073D9")) +
  geom_segment(aes(x = y0, y = min(df$is_0, df$is_1, df$lm_0), xend = y0, yend = i0), lty = "dotted") +
  geom_segment(aes(x = min(df$produccion), y = i0, xend = y0, yend = i0), lty = "dotted") +
  geom_segment(aes(x = y1, y = min(df$is_0, df$is_1, df$lm_0), xend = y1, yend = i1), lty = "dotted") +
  geom_segment(aes(x = min(df$produccion), y = i1, xend = y1, yend = i1), lty = "dotted") +
  geom_point(aes(x = y0, y = i0), size = 2) +
  geom_point(aes(x = y1, y = i1), size = 2) +
  labs(x = "Producción (Y)",
       y = "Tipo de interés (i)",
       title = "IS-LM en economía abierta sin movilidad de capitales") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

















