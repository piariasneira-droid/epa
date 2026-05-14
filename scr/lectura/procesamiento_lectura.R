# Cargar librerias
library(readr)
library(dplyr)

# Lectura microdatos hasta el año 20
lectura_microdatos_hasta_20 <- function(directory) {
  # Inicialización de las variables
  epa <- data.frame()
  
  # Listar todos los ficheros .csv en el directorio y subdirectorios
  csv_files <- list.files(path = directory, pattern = "^EPA_\\d{4}T\\d\\.csv$", full.names = TRUE, recursive = TRUE)
  
  # Procesar archivos .csv y combinarlos en epa
  for (file in csv_files) {
    df <- read_delim(file, delim = "\t", col_names = TRUE, col_types = cols(.default = "c"))
    epa <- bind_rows(epa, df)
  }
  
  # Eliminar variables temporales de forma segura
  if (exists("csv_files")) rm(csv_files)
  if (exists("df")) rm(df)
  if (exists("file")) rm(file)
  
  # Devolver el dataframe resultante
  return(epa)
}

# Función lectura microdatos años 21-23
lectura_microdatos_21_23 <- function(directory) {
  # Inicialización de las variables
  epa_original <- data.frame()
  epa_anexos <- data.frame()
  
  # Listar todos los ficheros .csv en el directorio y subdirectorios
  csv_files <- list.files(path = directory, pattern = "^EPA_\\d{4}T\\d\\.csv$", full.names = TRUE, recursive = TRUE)
  tab_files <- list.files(path = directory, pattern = "^EPAAnexo_\\d{4}T\\d\\.tab$", full.names = TRUE, recursive = TRUE)
  
  # Procesar archivos .csv y combinarlos en epa_original
  for (file in csv_files) {
    df <- read_delim(file, delim = "\t", col_names = TRUE, col_types = cols(.default = "c"))
    epa_original <- bind_rows(epa_original, df)
  }
  
  # Procesar archivos .tab y combinarlos en epa_anexos
  for (file in tab_files) {
    df <- read_delim(file, delim = "\t", col_names = TRUE, col_types = cols(.default = "c"))
    epa_anexos <- bind_rows(epa_anexos, df)
  }
  
  # Realizar el left join de epa_original con epa_anexos usando las columnas de identificación
  df <- epa_original %>%
    left_join(epa_anexos %>% select(CICLO, CCAA, NVIVI, NIVEL, NPERS, FACB2021), 
              by = c("CICLO", "CCAA", "NVIVI", "NIVEL", "NPERS"))
  
  # Eliminar variables temporales de forma segura
  if (exists("csv_files")) rm(csv_files)
  if (exists("epa_anexos")) rm(epa_anexos)
  if (exists("epa_original")) rm(epa_original)
  if (exists("file")) rm(file)
  if (exists("tab_files")) rm(tab_files)
  
  # Devolver el dataframe resultante
  return(df)
}

# Lectura microdatos hasta el año 24
lectura_microdatos_desde_24 <- function(directory) {
  # Inicialización de las variables
  epa <- data.frame()
  
  # Listar todos los ficheros .csv en el directorio y subdirectorios
  tab_files <- list.files(path = directory, pattern = "^EPA_\\d{4}T\\d\\.tab$", full.names = TRUE, recursive = TRUE)
  
  # Procesar archivos .csv y combinarlos en epa
  for (file in tab_files) {
    df <- read_delim(file, delim = "\t", col_names = TRUE)
    epa <- bind_rows(epa, df)
  }
  
  # Eliminar variables temporales de forma segura
  if (exists("tab_files")) rm(tab_files)
  if (exists("df")) rm(df)
  if (exists("file")) rm(file)
  
  # Devolver el dataframe resultante
  return(epa)
}

comparar_dataframes <- function(df1, df2) {
  # Obtener nombres de columnas de cada dataframe
  col_names_df1 <- colnames(df1)
  col_names_df2 <- colnames(df2)
  
  # Columnas coincidentes
  coincidentes <- intersect(col_names_df1, col_names_df2)
  
  # Columnas del primer dataframe que no están en el segundo
  no_en_df2 <- setdiff(col_names_df1, col_names_df2)
  
  # Columnas del segundo dataframe que no están en el primero
  no_en_df1 <- setdiff(col_names_df2, col_names_df1)
  
  # Devolver las tres listas como resultado
  resultado <- list(
    coincidentes = coincidentes,
    no_en_df2 = no_en_df2,
    no_en_df1 = no_en_df1
  )
  
  return(resultado)
}