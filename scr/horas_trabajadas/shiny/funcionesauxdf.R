# Funciones auxiliares
asignar_comarca <- function(df) {
  df <- df %>%
    mutate(COMARCA = case_when(
      CCAA == 1 ~ "Andalucía",
      CCAA == 2 ~ "Aragón",
      CCAA == 3 ~ "Asturias, Principado de",
      CCAA == 4 ~ "Balears, Illes",
      CCAA == 5 ~ "Canarias",
      CCAA == 6 ~ "Cantabria",
      CCAA == 7 ~ "Castilla y León",
      CCAA == 8 ~ "Castilla-La Mancha",
      CCAA == 9 ~ "Cataluña",
      CCAA == 10 ~ "Comunitat Valenciana",
      CCAA == 11 ~ "Extremadura",
      CCAA == 12 ~ "Galicia",
      CCAA == 13 ~ "Madrid, Comunidad de",
      CCAA == 14 ~ "Murcia, Región de",
      CCAA == 15 ~ "Navarra, Comunidad Foral de",
      CCAA == 16 ~ "País Vasco",
      CCAA == 17 ~ "Rioja, La",
      CCAA == 51 ~ "Ceuta",
      CCAA == 52 ~ "Melilla",
      CCAA == 99 ~ "España",
      TRUE ~ "Desconocido"
    ))
  return(df)
}

asignar_ano_trimestre <- function(df) {
  df <- df %>%
    mutate(
      ANO = 2021 + (CICLO - 194) %/% 4, 
      TRIMESTRE = (CICLO - 194) %% 4 + 1, 
      ANO_T = paste0(ANO, "-T", TRIMESTRE)
    )
  return(df)
}

asignar_sexo <- function(df) {
  df <- df %>%
    mutate(SEXO = case_when(
      SEXO1 == 1 ~ "Hombres",
      SEXO1 == 6 ~ "Mujeres",
      SEXO1 == 99 ~ "Ambos sexos",
      TRUE ~ NA_character_
    ))
  return(df)
}

asignar_actividad <- function(df) {
  df %>%
    mutate(ACTIVIDAD = case_when(
      ACT1 == 0 ~ "Agricultura, ganadería, silvicultura y pesca (CNAE-09: 01, 02, 03; CNAE-93: 01, 02, 05)",
      ACT1 == 1 ~ "Industria de la alimentación, textil, cuero, madera y papel (CNAE-09: 10-18; CNAE-93: 15-22)",
      ACT1 == 2 ~ "Industrias extractivas, refino de petróleo, industria química, farmacéutica, industria del caucho, suministro de energía, gestión de residuos. Metalurgia (CNAE-09: 05-09, 19-25, 35, 36-39; CNAE-93: 10-14, 23-28, 40, 41)",
      ACT1 == 3 ~ "Construcción de maquinaria, equipo eléctrico y material de transporte. Instalación y reparación industrial (CNAE-09: 26-33; CNAE-93: 29-37)",
      ACT1 == 4 ~ "Construcción (CNAE-09: 41-43; CNAE-93: 45)",
      ACT1 == 5 ~ "Comercio al por mayor y al por menor, reparación de automóviles, hostelería (CNAE-09: 45-47, 55, 56; CNAE-93: 50-52, 55)",
      ACT1 == 6 ~ "Transporte y almacenamiento. Información y comunicaciones (CNAE-09: 49-53, 58-63; CNAE-93: 60-64)",
      ACT1 == 7 ~ "Intermediación financiera, seguros, actividades inmobiliarias, servicios profesionales, científicos, administrativos y otros (CNAE-09: 64-66, 68, 69-75, 77-82; CNAE-93: 65-67, 70-74)",
      ACT1 == 8 ~ "Administración Pública, educación y actividades sanitarias (CNAE-09: 84, 85, 86-88; CNAE-93: 75, 80, 85)",
      ACT1 == 9 ~ "Otros servicios (CNAE-09: 90-93, 94-96, 97, 99; CNAE-93: 90-93, 95, 99)",
      ACT1 == 99 ~ "Total",
      TRUE ~ "Desconocido"  # Por si hay algún código que no está en la lista
    ))
}

asignar_label <- function(df) {
  df %>%
    mutate(LABEL = case_when(
      COMARCA == "Andalucía" ~ "AND",
      COMARCA == "Aragón" ~ "ARA",
      COMARCA == "Asturias, Principado de" ~ "AST",
      COMARCA == "Balears, Illes" ~ "BAL",
      COMARCA == "Canarias" ~ "CAN",
      COMARCA == "Cantabria" ~ "CANT",
      COMARCA == "Castilla y León" ~ "CYL",
      COMARCA == "Castilla-La Mancha" ~ "CLM",
      COMARCA == "Cataluña" ~ "CAT",
      COMARCA == "Comunitat Valenciana" ~ "VAL",
      COMARCA == "Extremadura" ~ "EXT",
      COMARCA == "Galicia" ~ "GAL",
      COMARCA == "Madrid, Comunidad de" ~ "CM",
      COMARCA == "Murcia, Región de" ~ "MUR",
      COMARCA == "Navarra, Comunidad Foral de" ~ "NAV",
      COMARCA == "País Vasco" ~ "PV",
      COMARCA == "Rioja, La" ~ "RIO",
      COMARCA == "España" ~ "ESP",
      COMARCA == "Ceuta" ~ "CEU",
      COMARCA == "Melilla" ~ "MEL",
      TRUE ~ NA_character_            
    ))
}

asignar_labeling <- function(df) {
  df %>%
    mutate(LABEL_ENG = case_when(
      COMARCA == "Andalucía" ~ "AND",
      COMARCA == "Aragón" ~ "ARA",
      COMARCA == "Asturias, Principado de" ~ "AST",
      COMARCA == "Balears, Illes" ~ "BAL",
      COMARCA == "Canarias" ~ "CAN",
      COMARCA == "Cantabria" ~ "CANT",
      COMARCA == "Castilla y León" ~ "CL",
      COMARCA == "Castilla-La Mancha" ~ "CLM",
      COMARCA == "Cataluña" ~ "CAT",
      COMARCA == "Comunitat Valenciana" ~ "VAL",
      COMARCA == "Extremadura" ~ "EXT",
      COMARCA == "Galicia" ~ "GAL",
      COMARCA == "Madrid, Comunidad de" ~ "CoM",
      COMARCA == "Murcia, Región de" ~ "MUR",
      COMARCA == "Navarra, Comunidad Foral de" ~ "NAV",
      COMARCA == "País Vasco" ~ "BC",
      COMARCA == "Rioja, La" ~ "RIO",
      COMARCA == "España" ~ "SP",
      COMARCA == "Ceuta" ~ "CEU",
      COMARCA == "Melilla" ~ "MEL",
      TRUE ~ NA_character_            
    ))
}

asignar_actividad_label <- function(df) {
  df %>%
    mutate(ACT_LAB = case_when(
      ACT1 == 0 ~ "AGR+GAN",
      ACT1 == 1 ~ "ALI+AAGG",
      ACT1 == 2 ~ "FAR+ENE",
      ACT1 == 3 ~ "EQUIPO",
      ACT1 == 4 ~ "CONSTRU",
      ACT1 == 5 ~ "COMERCIO",
      ACT1 == 6 ~ "TIC+TRP",
      ACT1 == 7 ~ "FIN+SEG",
      ACT1 == 8 ~ "SA+ED+AP",
      ACT1 == 9 ~ "OTR_SER",
      ACT1 == 99 ~ "TOT",
      TRUE ~ "DESCONOCIDO"  # Cambia los valores no mapeados a "DESCONOCIDO"
    ))
}

asignar_edad <- function(df) {
  df %>%
    mutate(EDAD = case_when(
      EDAD1 == 00 ~ "0 a 4 años",
      EDAD1 == 05 ~ "5 a 9 años",
      EDAD1 == 10 ~ "10 a 15 años",
      EDAD1 == 16 ~ "16 a 19 años",
      EDAD1 == 20 ~ "20 a 24 años",
      EDAD1 == 25 ~ "25 a 29 años",
      EDAD1 == 30 ~ "30 a 34 años",
      EDAD1 == 35 ~ "35 a 39 años",
      EDAD1 == 40 ~ "40 a 44 años",
      EDAD1 == 45 ~ "45 a 49 años",
      EDAD1 == 50 ~ "50 a 54 años",
      EDAD1 == 55 ~ "55 a 59 años",
      EDAD1 == 60 ~ "60 a 64 años",
      EDAD1 == 65 ~ "65 o más años",
      EDAD1 == 99 ~ "Total",
      TRUE ~ "DESCONOCIDO"  # Etiqueta para edades no mapeadas
    ))
}

asignar_jornada <- function(df) {
  df %>%
    mutate(JORNADA = case_when(
      PARCO1 == 1 ~ "Completa",
      PARCO1 == 6 ~ "Parcial",
      PARCO1 == 99 ~ "Ambas jornadas",
      TRUE ~ "DESCONOCIDO"  # Etiqueta para jornadas no mapeadas
    ))
}

asignar_sector <- function(df) {
  df %>%
    mutate(SECTOR = case_when(
      ACT1 == 0 ~ "AGRICULTURA",
      ACT1 == 1 ~ "INDUSTRIA",
      ACT1 == 2 ~ "INDUSTRIA",
      ACT1 == 3 ~ "INDUSTRIA",
      ACT1 == 4 ~ "CONSTRUCCIÓN",
      ACT1 == 5 ~ "SERVICIOS",
      ACT1 == 6 ~ "SERVICIOS",
      ACT1 == 7 ~ "SERVICIOS",
      ACT1 == 8 ~ "SERVICIOS",
      ACT1 == 9 ~ "SERVICIOS",
      ACT1 == 99 ~ "TOTAL",
      TRUE ~ "DESCONOCIDO"  # Por si hay algún código que no está en la lista
    ))
}

asignar_sj_bis <- function(df) {
  df %>%
    mutate(
      SJ_LAB = case_when(
        SEXO == "Hombres" & JORNADA == "Completa" ~ "HC",
        SEXO == "Hombres" & JORNADA == "Parcial" ~ "HP",
        SEXO == "Mujeres" & JORNADA == "Completa" ~ "MC",
        SEXO == "Mujeres" & JORNADA == "Parcial" ~ "MP",
        TRUE ~ NA_character_  # Por si hay alguna combinación que no coincide
      ))
}
