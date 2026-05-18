# Columnas seleccionadas y sus tipos
columnas_seleccionadas <- c("CICLO", "CCAA", "NVIVI", "NPERS", "EDAD1",
        "RELPP1", "NAC1", "SEXO1", "OCUP1", "ACT1", "SITU", "PARCO1", "AOI", 
        "HORASE", "FACTOR", "DUCON1", "ITBU", "RZNOTB")

# Definir los tipos de las columnas
tipos_columnas <- list(
  CICLO = "integer",
  CCAA = "integer",
  NVIVI = "integer",
  NPERS = "integer",
  EDAD1 = "integer",
  RELPP1 = "integer",
  NAC1 = "integer",
  SEXO1 = "integer",
  OCUP1 = "integer",
  ACT1 = "integer",
  SITU = "integer",
  PARCO1 = "integer",
  AOI = "integer",
  HORASE = "character",
  FACTOR = "numeric",
  DUCON1 = "integer",
  ITBU = "integer",
  RZNOTB = "integer"
)