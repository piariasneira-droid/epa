# Cargar librerías necesarias
library(dplyr)
library(magrittr)
library(tidyr)
library(openxlsx)
library(data.table)
library(readr)

# Inicializamos las variables directorio
#dir <- "C:/R_scripts/explotacion_microdatos/epa"
dir <- "D:/R_scripts/explotacion_microdatos/epa"
#dir <- "//Eco/eco/PPVER132/GRP/ECONOMIA/ESTUDIOS/Coyuntura/R_scipts/explotacion_microdatos/epa"
dir_microdatos_h_20 <- file.path(dir, "microdatos", "csvs_hasta_20")
dir_microdatos_21_23 <- file.path(dir, "microdatos", "csvs_21_23")
#dir_microdatos_d_24 <- file.path(dir, "microdatos", "csvs_desde_24")

# Cargamos procesarmiento lectura
source(file.path(dir, "scripts_lectura", "procesamiento_lectura.R" ))

# Leemos datos desde el 05 hasta el 20
epa_h_20 <- lectura_microdatos_hasta_20(dir_microdatos_h_20)
epa_21_23 <- lectura_microdatos_21_23(dir_microdatos_21_23)
#epa_d_24 <- lectura_microdatos_desde_24(dir_microdatos_d_24)

# Renombrar las columnas EDAD5 y HORPLUS a EDAD1 y HOREPLUS en epa_05_20
epa_h_20 <- epa_h_20 %>%
  rename(
    EDAD1 = EDAD5,
    HOREPLU = HORPLU
  ) %>%
  mutate(FACTOR = FACTOREL)

# Transferir valores de HORPLU a HOREPLU
epa_21_23$HOREPLU <- ifelse(epa_21_23$HORPLU != "", epa_21_23$HORPLU, epa_21_23$HOREPLU)
epa_21_23$HORPLU <- NULL

epa_21_23 <- epa_21_23 %>%
  mutate(FACTOR = FACB2021)

# Sacamos ls csv juntos
epa_05_23 <- bind_rows(epa_h_20, epa_21_23)
fwrite(epa_05_23, file = file.path(dir, "microdatos", "microdatos_juntos.csv"), na = "")