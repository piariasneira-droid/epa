lectura_anadir_t <- function(dir, trimestre_carga) {
  # Cargar las librerías necesarias
  library(data.table)
  library(dplyr)
  
  # Inicializar rutas y variables
  dir_csv <- file.path(dir, "microdatos", "microdatos_juntos.csv")
  dir_microdatos_trimestre <- file.path(dir, "microdatos", "csvs_desde_24")
  nombre_archivo_carga <- paste0("EPA_", trimestre_carga, ".tab")
  
  # Cargar los datos
  epa_tri <- fread(file.path(dir_microdatos_trimestre, nombre_archivo_carga), sep = "\t", colClasses = "character")
  epa_old <- fread(dir_csv, sep = ",", colClasses = "character")
  
  # Añadir columna FACTOR
  epa_tri$FACTOR <- epa_tri$FACTOREL
  
  # Calcular el último trimestre cargado
  epa_old_max <- max(as.integer(epa_old$CICLO), na.rm = TRUE)
  epa_tri_max <- max(as.integer(epa_tri$CICLO), na.rm = TRUE)
  
  # Comprobar si el valor máximo de CICLO en epa_tri es mayor que en epa_old
  if (epa_tri_max > epa_old_max) {
    # Crear nuevo CSV
    epa <- bind_rows(epa_old, epa_tri)
    fwrite(epa, file = dir_csv, sep = ",")
    message("Datos añadidos al archivo CSV.")
  } else {
    message("El archivo .tab ya está añadido al archivo CSV. No se añadieron datos.")
  }
  
  # Borrar variables para liberar memoria
  rm(dir, dir_csv, dir_microdatos_trimestre, epa_old, epa_old_max, epa_tri, epa_tri_max, nombre_archivo_carga, trimestre_carga)
}