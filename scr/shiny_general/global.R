library(cowplot)
library(dplyr)
library(ggplot2)
library(readxl)
library(rsconnect)
library(plotly)
library(shiny)

# Función para convertir períodos a formato numérico
convertir_periodo_a_numero <- function(periodo) {
  # Extraer el año y el trimestre del período
  anio <- as.numeric(substr(periodo, 1, 4))
  trimestre <- as.numeric(substr(periodo, 6, 6))
  
  # Convertir a formato numérico único (e.g., 2002T1 -> 200201)
  return(anio * 10 + trimestre)
}

# Reactive expressions para leer dataframes
ciclo_ano_t <- reactive({
  read_excel("./data/ciclo_ano_t.xlsx")
}) 

colores_ccaa <- reactive({
  read_excel("./data/ccaa_colores.xlsx")
}) 

hogares <- reactive({
  read_excel("./data/hogares_con_proporciones.xlsx", sheet = "Hogares")
})

horas <- reactive({
  read_excel("./data/horas.xlsx", sheet = "Horas")
})


empleo <- reactive({
  read.csv("./data/empleo.csv", sep = ";", dec = ",") %>%
    rename(COMARCA = Comunidades.y.Ciudades.Autónomas) %>%
    mutate(
      Periodo = as.factor(Periodo),  # Convertir 'Periodo' en factor
      COMARCA = sub("^[^ ]+ ", "", COMARCA),
      COMARCA = ifelse(COMARCA == "Nacional", "España", COMARCA),
      COMARCA = sub("Castilla - La Mancha", "Castilla-La Mancha", COMARCA),
      Total = as.numeric(gsub(",", ".", Total)),
      aaaatt = convertir_periodo_a_numero(as.character(Periodo))
    )
  })

actividad <- reactive({
  read.csv("./data/actividad.csv", sep = ";", dec = ",") %>%
    rename(COMARCA = Comunidades.y.Ciudades.Autónomas) %>%
    mutate(
      Periodo = as.factor(Periodo),  # Convertir 'Periodo' en factor
      COMARCA = sub("^[^ ]+ ", "", COMARCA),
      COMARCA = ifelse(COMARCA == "Nacional", "España", COMARCA),
      COMARCA = sub("Castilla - La Mancha", "Castilla-La Mancha", COMARCA),
      Total = as.numeric(gsub(",", ".", Total)),
      aaaatt = convertir_periodo_a_numero(as.character(Periodo))
    )
})

paro <- reactive({
  read.csv("./data/paro.csv", sep = ";", dec = ",") %>%
    rename(COMARCA = Comunidades.y.Ciudades.Autónomas) %>%
    mutate(
      Periodo = as.factor(Periodo),  # Convertir 'Periodo' en factor
      COMARCA = sub("^[^ ]+ ", "", COMARCA),
      COMARCA = ifelse(COMARCA == "Nacional", "España", COMARCA),
      COMARCA = sub("Castilla - La Mancha", "Castilla-La Mancha", COMARCA),
      Total = as.numeric(gsub(",", ".", Total)),
      aaaatt = convertir_periodo_a_numero(as.character(Periodo))
    )
})

# Cargar ficheros auxiliares
source("./funcionesaux.R")
source("./graficos/plot_plotly_hogares.R")
source("./graficos/plot_plotly_horas.R")
source("./graficos/plot_plotly_tasas.R")