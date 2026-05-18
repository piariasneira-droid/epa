# Environ ----
source("./carga_librerias.R")
trimestre_carga <- "2026T1"

# Load function
source("./scr/read_data/anadir_csv_trimestre.R")
source("./scr/read_data/read_data_funproc.R")

# Update microdata ----
append_quarterly_data(trimestre_carga)

