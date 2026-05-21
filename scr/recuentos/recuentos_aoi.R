# recuentos.R
# Workspace ----

source("./carga_librerias.R")
source("./scr/recuentos/procesamiento_recuentos.R")
source("./scr/columnas_epa.R")

# Configuration ----
data_path  <- "./data/microdatos/epa_microdata.parquet"
data_path <- "./data/microprocessed"
path_file <- "./documentos/aoi_tasas.xlsx"
seg_cols   <- c()

# Data loading ----
epa <- load_microdata(data_path,
                      cols  = columnas_seleccionadas,
                      types = tipos_columnas)

# AOI ----
aoi        <- count_aoi(epa, FACTOR, segment_cols = seg_cols)
aoi_g      <- group_aoi(aoi)

aoi_tot    <- compute_subtotals(aoi,        c(seg_cols, "CCAA"), "TOTAL")
data.table::setorder(aoi_tot, CICLO, CCAA)

aoi_g_tot  <- compute_subtotals(aoi_g,      c(seg_cols, "CCAA"), "TOTAL")
data.table::setorder(aoi_g_tot, CICLO, CCAA)

## Calculate rates ----
aoi_g_wide <- reshape_pob_wide(aoi_g_tot, segment_cols = seg_cols)
aoi_rates <- aoi_g_wide %>%
  calculate_labor_rates() %>%
  calculate_annual_variation(segment_cols = seg_cols) %>%
  rename_annual_variation()

## Excel outputs ----
write_excel_formatted(
  dt         = aoi_rates,
  sheet_name = "AOI",
  path       = path_file,
  col_int    = c("CICLO", "CCAA"),
  col_mil    = c("Empleados", "Parados", "Inactivos", "Menores", "Activos", "PED", "Poblacion",
                 "Empleados_dif", "Parados_dif", "Inactivos_dif", "Menores_dif",
                 "Activos_dif", "PED_dif", "Poblacion_dif"),
  col_dec    = c("Tactividad", "Templeabilidad", "Tparo",
                 "Tactividad_dif", "Templeabilidad_dif", "Tparo_dif",
                 "Tactividad_tv", "Templeabilidad_tv", "Tparo_tv",
                 "Empleados_tv", "Parados_tv", "Inactivos_tv", "Menores_tv",
                 "Activos_tv", "PED_tv", "Poblacion_tv"),
  col_per    = c(),
  col_char   = c(),
  col_int2   = c()
)