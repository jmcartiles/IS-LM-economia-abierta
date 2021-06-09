

df.parametros <- read_xlsx("parametros.xlsx")
var_parametros = df.parametros
var_rango_produccion = 0:25000
var_dist_eq = 0.2
var_color <- c("#FF4036", "#0073D9", "#4DAF4A")
var_movilidad_capital = "mpk"
var_tipo_cambio = "fijo"



f_is_lm_ea <- function(var_parametros, var_rango_produccion, var_dist_eq, var_color, var_movilidad_capital, var_tipo_cambio) {
  
  parametros_inicial <- filter(var_parametros, Escenario == "Inicial")
  parametros_final <- filter(var_parametros, Escenario == "Final")
  
  df.cambios <- diff(as.matrix(var_parametros[,-1]))
  cambios <- colnames(df.cambios)[which(df.cambios != 0)]
  
  var_cambios <- 0
  
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
  
  
  
}