# horas_trabajadas.R
# Workspace ----

source("./scr/carga_librerias.R")
source("./scr/recuentos/procesamiento_recuentos.R")
source("./scr/columnas_epa.R")
source("./scr/horas_trabajadas/horas_procfun.R")

# Configuration ----
data_path <- "./data/microprocessed"
out_folder <- "./scr/horas_trabajadas/shiny/datos"
path_file <- "./scr/horas_trabajadas/shiny/datos/datos.parquet"
seg_cols   <- c("EDAD1", "SEXO1", "ACT1", "PARCO1")

# Data loading ----
epa <- load_microdata(data_path,
                      cols  = columnas_seleccionadas,
                      types = tipos_columnas)

# AOI ----
aoi <- count_aoi(epa, FACTOR, segment_cols = seg_cols)

id_cols_aoi <- setdiff(names(aoi), c("AOI", "TOTAL"))
formula_dcast_aoi <- as.formula(paste(paste(id_cols_aoi, collapse = " + "), "~ AOI"))
aoi_wide <- process_aoi_wide(
  df = aoi, 
  id_cols = id_cols_aoi, 
  formula_dcast = formula_dcast_aoi)

aoi_wide <- compute_subtotals(aoi_wide,
  c("CCAA", seg_cols),
  c(
    "AOI_COD_3", "AOI_COD_4", "AOI_COD_5", "AOI_COD_6",
    "AOI_COD_7", "AOI_COD_8", "AOI_COD_9", "AOI_COD_NA",
    "OCUPADOS", "PARADOS", "ACTIVOS", "INACTIVOS", "MENORES", "PET", "POB"
  )
)

# HOURS ----
hours <- count_hours(epa, FACTOR, segment_cols = seg_cols)
hours_tot <- compute_subtotals(
  hours[, !"HORAS_MED"],
  marginal_cols = c("CCAA", seg_cols),
  value_cols    = c("TOTAL", "OCUPADOS_TRABAJANDO")
)
hours_tot[, HORAS_MED := TOTAL / OCUPADOS_TRABAJANDO]
data.table::setorder(hours_tot, CICLO, CCAA)

# RZNOTB ----
rznotb <- count_rznotb(epa[HORASE == "0000"], FACTOR, segment_cols = seg_cols)

rznotb_tot <- compute_subtotals(rznotb, c(seg_cols, "CCAA"), c("TOTAL"))
rznotb_tot <- rznotb_tot[, .(TOTAL = sum(TOTAL)), by = c("CICLO", "CCAA", "RZNOTB", seg_cols)]
data.table::setorder(rznotb_tot, CICLO, CCAA)

id_cols_rz <- setdiff(names(rznotb_tot), c("RZNOTB", "TOTAL"))
formula_dcast_rz <- as.formula(paste(paste(id_cols_rz, collapse = " + "), "~ RZNOTB"))
rznotb_wide <- process_rznotb_wide(
  df = rznotb_tot,
  id_cols = id_cols_rz,
  formula_dcast = formula_dcast_rz)

# JOIN ----
join_keys <- c("CICLO", "CCAA", seg_cols)
df_dt <- merge(hours_tot, rznotb_wide, by = join_keys, all.x = TRUE)
cols_to_fill <- setdiff(names(df_dt), join_keys)
for (col in cols_to_fill) {
  set(df_dt, i = which(is.na(df_dt[[col]])), j = col, value = 0)
}

# SAVE ----
dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(
  x = df_dt,
  sink = path_file)



