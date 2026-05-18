# Libraries ----
source("./carga_librerias.R")

# Trimestre de carga de microdatos
trimestre_carga <- "2026T1"

# Carga funciones para añadir trimestres
source("./scr/read_data/anadir_csv_trimestre.R")
source("./scr/read_data/read_data_funproc.R")

# Actualizamos fichero de microdatos
append_quarterly_data(trimestre_carga)