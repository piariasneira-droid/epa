# recuentos.R
# Workspace ----

source("./scr/carga_librerias.R")
source("./scr/recuentos/procesamiento_recuentos.R")
source("./scr/columnas_epa.R")

# Configuration ----
data_path <- "./data/microprocessed"
path_fileho <- "./documentos/horas_hogares/horas.xlsx"
path_filehh <- "./documentos/horas_hogares/hogares.xlsx"
path_filerz <- "./documentos/horas_hogares/rznotb.xlsx"
seg_cols   <- c()

# Data loading ----
epa <- load_microdata(data_path,
                      cols  = columnas_seleccionadas,
                      types = tipos_columnas)

# Counts ----
households <- count_households(epa, FACTOR, segment_cols = seg_cols)
hours      <- count_hours(epa, FACTOR, segment_cols = seg_cols)
rznotb     <- count_rznotb(epa[HORASE == "0000"], FACTOR, segment_cols = seg_cols)

# Subtotals ----
## Households ----
households_tot <- compute_subtotals(
  households, 
  c(seg_cols, "CCAA"), 
  c("Act_ocu", "Act_par", "Inac", "Uni_ocu", "Uni_par", "Uni_ina", "Hogares_uni", "Hogares")
)
data.table::setorder(households_tot, CICLO, CCAA)

## RZNOTB ----
rznotb_tot <- compute_subtotals(rznotb, c(seg_cols, "CCAA"), c("TOTAL"))
rznotb_tot <- rznotb_tot[, .(TOTAL = sum(TOTAL)), by = c("CICLO", "CCAA", "RZNOTB", seg_cols)]
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
  path = path_fileho,
  col_int = c("CICLO", "CCAA"), col_mil = c("TOTAL", "OCUPADOS_TRABAJANDO"),
  col_dec = c("HORAS_MED"), col_per = c(), col_char = c(), col_int2 =c())

## Households ----
write_excel_formatted(
  dt = households_tot,
  sheet_name = "hogares",
  path = path_filehh,
  col_int = c("CICLO", "CCAA"), col_dec = c(), 
  col_per = c(), col_char = c(), col_int2 =c(),
  col_mil = c("Act_ocu", "Act_par", "Inac", "Uni_ocu", "Uni_par", "Uni_ina", "Hogares_uni", "Hogares"))
  
## RZNOTB ----
write_excel_formatted(
  dt = rznotb_tot,
  sheet_name = "rznotb",
  path = path_filerz,
  col_int = c("CICLO", "CCAA", "RZNOTB"), col_mil = c("TOTAL"),
  col_dec = c(), col_per = c(), col_char = c(), col_int2 =c())