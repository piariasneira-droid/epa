# EPA microdata — read and consolidate
# Libraries ----
source("./carga_librerias.R")

# Paths — microdata folders ----
dir_microdata_h_20  <- "./data/microdatos/csvs_hasta_20"
dir_microdata_21_23 <- "./data/microdatos/csvs_21_23"
dir_microdata_d_24  <- "./data/microdatos/csvs_desde_24"

# Load helper functions ----
source("./scr/read_data/read_data_funproc.R")

# Read microdata by period ----
epa_h_20  <- read_microdata_up_to_2020(dir_microdata_h_20, save_parquet= FALSE)
epa_21_23 <- read_microdata_2021_2023(dir_microdata_21_23, save_parquet= FALSE)
epa_d_24  <- read_microdata_from_2024(dir_microdata_d_24)

# Harmonise columns — up to 2020 dataset ----
setnames(epa_h_20, 
         old = c("EDAD5", "HORPLU"), 
         new = c("EDAD1", "HOREPLU"), 
         skip_absent = TRUE)

epa_h_20[, FACTOR := FACTOREL]

# Harmonise columns — 2021-2023 dataset ----
epa_21_23[, HOREPLU := fifelse(HORPLU != "", HORPLU, HOREPLU)]
epa_21_23[, FACTOR := FACB2021]
epa_21_23[, HORPLU := NULL]

# Stack all periods and export ----
cat(">> Stacking all periods...\n")
epa_full <- rbindlist(list(epa_h_20, epa_21_23, epa_d_24), use.names = TRUE, fill = TRUE)
cat(">> Total rows:", nrow(epa_full), "\n")

# Parquet output 
out_path <- "./data/microdatos/epa_microdata.parquet"
write_parquet(epa_full, sink = out_path)

cat(">> Parquet saved:", out_path, "\n")