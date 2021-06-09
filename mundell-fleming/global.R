
library(shiny)
library(DT)
library(tidyverse)
library(plotly)

lapply(list.files("code/"), function(x) source(paste0("code/", x), encoding = "UTF-8"))




# configuracion_escenario_ui <- function(id) {
#   
#   ns <- NS(id)
#   tagList(
#     column(4,
#            numericInput(inputId = "c", label = "Propensión marginal a consumir", value = 0.25, min = 0, max = 1),
#            numericInput(inputId = "t", label = "Tipo impositivo", value = 0.2, min = 0, max = 1),
#            numericInput(inputId = "m", label = "Propensión marginal a importar", value = 0.1, min = 0, max = 1),
#            numericInput(inputId = "b", label = "Sensibilidad de inversión al tipo de interés", value = 1500),
#            numericInput(inputId = "C0", label = "Consumo autónomo", value = 400)),
#     column(4,
#            numericInput(inputId = "I0", label = "Inversión autónoma", value = 310),
#            numericInput(inputId = "G0", label = "Gasto público", value = 600),
#            numericInput(inputId = "XN0", label = "Exportaciones autónomas", value = 450),
#            numericInput(inputId = "v", label = "Sensibilidad de la BC al tipo de cambio real", value = 1000),
#            numericInput(inputId = "e", label = "Tipo de cambio", value = 1)
#     ),
#     column(4,
#            numericInput(inputId = "p_int", label = "Precio internacional", value = 1),
#            numericInput(inputId = "p_dom", label = "Precio nacional", value = 1),
#            numericInput(inputId = "k", label = "Sensibilidad de la demanda de dinero al nivel de renta", value = 10),
#            numericInput(inputId = "h", label = "Sensibilidad de la demanda de dinero al tipo de interés", value = 12000),
#            numericInput(inputId = "M", label = "Oferta nominal de dinero", value = 3000)
#     )
#   )
#   
# }
# 
# configuracion_escenario_server <- function(id) {
#   
#   moduleServer(
#     id,
#     function(input, output, session) {
#       reactive({
#         data.frame(
#           c = input$c
#         )
#         })
#     })
#   
# }