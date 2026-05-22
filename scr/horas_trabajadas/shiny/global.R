library(arrow)
library(data.table)
library(dplyr)
library(DT)
# library(gganimate)
library(ggplot2)
library(plotly)
library(scales)
library(shiny)
library(readxl)

# Paleta de colores
colorpal1 <- "#526DB0"
colorpal2 <- "#F5C201"
colorpal3 <- "#A0B8C8"
colorpal4 <- "#7A7A7A"
colorpal5 <- "grey75"
colorpal6 <- "#DC5924"
colorpal7 <- "#7B90C3"

# Dataframes
data <- reactive({
  arrow::read_parquet("./datos/datos.parquet")
})

actividades <- reactive({
  read_excel("./datos/actividad.xlsx")
}) 

ccaas <- reactive({
  read_excel("./datos/ccaa.xlsx")
}) 

ciclos <- reactive({
  read_excel("./datos/ciclo.xlsx")
}) 

edades <- reactive({
  read_excel("./datos/edad.xlsx")
})

jornadas <- reactive({
  read_excel("./datos/jornada.xlsx")
}) 

sexos <- reactive({
  read_excel("./datos/sexo.xlsx")
})

variables <- reactive({
  read_excel("./datos/variables.xlsx")
})

# Cargar ficheros auxiliares
source("./elementos.R")
source("./funcionesauxdf.R")
source("./plots.R")
source("./temaplots.R")