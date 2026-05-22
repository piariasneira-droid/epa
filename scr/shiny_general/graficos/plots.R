# Carga de librerías
library(dplyr)
library(plotly)
library(readxl)
library(scales)

# Inicialización de variables
#dir <- "C:/R_scripts/explotacion_microdatos/epa/shiny"
dir <- "D:/R_scipts/explotacion_microdatos/epa/shiny"
#dir <- "//Eco/eco/PPVER132/GRP/ECONOMIA/ESTUDIOS/Coyuntura/R_scipts/explotacion_microdatos/epa/shiny"
dir_graficos <- file.path(dir, "graficos")
path_colores <- file.path(dir, "data", "ccaa_colores.xlsx")
path_horas <- file.path(dir, "data", "horas.xlsx")
path_hogares <- file.path(dir, "data", "hogares_con_proporciones.xlsx")
path_actividad <- file.path(dir, "data", "actividad.csv")
path_empleo <- file.path(dir, "data", "empleo.csv")
path_paro <- file.path(dir, "data", "paro.csv")

# Cargar dataframes
ccaa_colores_df <- read_excel(path_colores)
horas <- read_excel(path_horas)
hogares <- read_excel(path_hogares)
actividad <- read.csv(path_actividad, sep = ";", dec = ",")

# Cargar funciones plotly
source(file.path(dir_graficos, "plot_plotly_hogares.R"))
source(file.path(dir_graficos, "plot_plotly_horas.R"))
source(file.path(dir_graficos, "plot_plotly_tasas.R"))

# Crea un named vector de colores usando la columna CCAA como nombres
colores <- setNames(ccaa_colores_df$COLOR, ccaa_colores_df$COMARCA)

plot_act_ocu <- crear_grafico_hogares(hogares, "P_Act_ocu", 
                              "Hogares con todos sus miembros activos ocupados",
                              "Proporción sobre el total de hogares", 'Proporción %', colores)

plot_act_par <- crear_grafico_hogares(hogares, "P_Act_par", 
                                      "Hogares con todos sus miembros activos parados",
                                      "Proporción sobre el total de hogares", 'Proporción %', colores)

plot_inact <- crear_grafico_hogares(hogares, "P_Inact", 
                                      "Hogares con todos sus miembros activos inactivos",
                                      "Proporción sobre el total de hogares", 'Proporción %', colores)

plot_uni_ocu <- crear_grafico_hogares(hogares, "P_Uni_ocu", 
                                      "Hogares unipersonales con su persona de referencia ocupada",
                                      "Proporción sobre el total de hogares unpipersonales", 'Proporción %', colores)

plot_uni_par <- crear_grafico_hogares(hogares, "P_Uni_par", 
                                      "Hogares unipersonales con su persona de referencia parada",
                                      "Proporción sobre el total de hogares unipersonales", 'Proporción %', colores)

plot_uni_inact <- crear_grafico_hogares(hogares, "P_Uni_ina", 
                                      "Hogares unipersonales con su persona de referencia inactiva",
                                      "Proporción sobre el total de hogares unipersonales", 'Proporción %', colores)

plot_n_hogares <- crear_grafico_hogares_n_y_tv(hogares, "Hogares", "Tasa_var_hogares", 
                                               "Número de hogares y tasa de variación anualizada", colores)

plot_n_hogares_uni <- crear_grafico_hogares_n_y_tv(hogares, "Hogares_uni", "Tasa_var_hogares_uni", 
                                                   "Hogares con todos sus miembros inactivos", colores)

plot_horas_trabajadas <- crear_grafico_horas_totales(horas, "HORAS_TOT", "MM_4T_HORAS_TOT",
                                                      "Horas trabajadas", colores)

plot_horas_semanales <- crear_grafico_horas_media(horas, "HORAS_MEDIA", "Horas semanales trabajadas", colores)


plot_compa <- crear_grafico_comp(horas, "TV_ANUAL_HORAS_TOT", "Tasas de variación", colores)

plot_tasas <- crear_grafico_tasas(actividad, "tit", "sub", "tity", colores)
