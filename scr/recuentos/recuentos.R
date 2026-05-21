# recuentos.R
# Workspace ----

source("./carga_librerias.R")
source("./scr/recuentos/procesamiento_recuentos.R")
source("./scr/columnas_epa.R")

# Configuration ----
# data_path  <- "./data/microdatos/epa_microdata.parquet"
data_path <- "./data/microdatos/epa_microdata_partitioned.parquet"
output_dir <- "./documentos/horas_hogares"
seg_cols   <- c()

# Data loading ----
epa <- load_microdata(data_path,
                      cols  = columnas_seleccionadas,
                      types = tipos_columnas)

# Counts ----
aoi        <- count_aoi(epa, FACTOR, segment_cols = seg_cols)
aoi_g      <- group_aoi(aoi)
households <- count_households(epa, FACTOR, segment_cols = seg_cols)
hours      <- count_hours(epa, FACTOR, segment_cols = seg_cols)
rznotb     <- count_rznotb(epa, FACTOR, segment_cols = seg_cols)

# Subtotals ----
## AOI ----
aoi_tot        <- compute_subtotals(aoi,        c(seg_cols, "CCAA"), "TOTAL")
data.table::setorder(aoi_tot, CICLO, CCAA)

aoi_g_tot      <- compute_subtotals(aoi_g,      c(seg_cols, "CCAA"), "TOTAL")
data.table::setorder(aoi_g_tot, CICLO, CCAA)

## Households ----
households_tot <- compute_subtotals(
  households, 
  c(seg_cols, "CCAA"), 
  c("Act_ocu", "Act_par", "Inac", "Uni_ocu", "Uni_par", "Uni_ina", "Hogares_uni", "Hogares")
)
data.table::setorder(households_tot, CICLO, CCAA)

## RZNOTB ----
rznotb_tot <- compute_subtotals(rznotb, c(seg_cols, "CCAA"), c("TOTAL"))
data.table::setorder(rznotb_tot, CICLO, CCAA)

## Hours ----
hours_tot <- compute_subtotals(
  hours[, !"HORAS_MED"],
  marginal_cols = c("CCAA"),
  value_cols    = c("TOTAL", "OCUPADOS_TRABAJANDO")
)
hours_tot[, HORAS_MED := TOTAL / OCUPADOS_TRABAJANDO]
data.table::setorder(hours_tot, CICLO, CCAA)

# Excel output ----
## Hours ----
write_excel_formatted(
  dt = hours_tot,
  sheet_name = "horas",
  path = "./documentos/horas_hogares/horas.xlsx",
  col_int = c("CICLO", "CCAA"), col_mil = c("TOTAL", "OCUPADOS_TRABAJANDO"),
  col_dec = c("HORAS_MED"), col_per = c(), col_char = c(), col_int2 =c())

## Households ----
write_excel_formatted(
  dt = households_tot,
  sheet_name = "hogares",
  path = "./documentos/horas_hogares/hogares.xlsx",
  col_int = c("CICLO", "CCAA"), col_dec = c(), 
  col_per = c(), col_char = c(), col_int2 =c(),
  col_mil = c("Act_ocu", "Act_par", "Inac", "Uni_ocu", "Uni_par", "Uni_ina", "Hogares_uni", "Hogares"))
  
## RZNOTB ----
write_excel_formatted(
  dt = rznotb_tot,
  sheet_name = "rznotb",
  path = "./documentos/horas_hogares/rznotb.xlsx",
  col_int = c("CICLO", "CCAA", "AOI", "RZNOTB"), col_mil = c("TOTAL"),
  col_dec = c(), col_per = c(), col_char = c(), col_int2 =c())