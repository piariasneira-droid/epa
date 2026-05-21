# Environ ----
source("./carga_librerias.R")
trimestre_carga <- "2026T1"

# Cargar funciones ----
source("./scr/read_data_partitioned/read_data_funproc_partitioned_schema.R")
source("./scr/recuentos/procesamiento_recuentos.R")
source("./scr/columnas_epa.R")
columnas_seleccionadas[columnas_seleccionadas == "FACTOR"] <- "FACTOREL"
tipos_columnas$FACTOR <- NULL
tipos_columnas$FACTOREL <- "numeric"

# Actualización hogares, horas y rznotb
add_quarterly_hours(quarter_to_load = trimestre_carga,
                         seg_cols = c(), 
                         selected_columns = columnas_seleccionadas, 
                         column_types = tipos_columnas)

add_quarterly_households(quarter_to_load = trimestre_carga,
                         seg_cols = c(),
                         selected_columns = columnas_seleccionadas,
                         column_types = tipos_columnas)

add_quarterly_rznotb(quarter_to_load = trimestre_carga,
                         seg_cols = c(), 
                         selected_columns = columnas_seleccionadas, 
                         column_types = tipos_columnas)

# Actualización microdatos ----
append_quarterly_partition(trimestre_carga)
update_epa_microdata_flat(trimestre_carga)