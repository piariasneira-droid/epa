# EPA microdata — partitioned schema ingestion
# Load workspace ----
source("./scr/carga_librerias.R")
source("./scr/read_data_partitioned/read_data_funproc_partitioned_schema.R")

# Paths ----
dir_microdata_h_20  <- "./data/microdatos/csvs_hasta_20"
dir_microdata_21_23 <- "./data/microdatos/csvs_21_23"
dir_microdata_d_24  <- "./data/microdatos/csvs_desde_24"

dir_out <- "./data/microprocessed"

# Write partitions ----
write_partitions_up_to_2020(
  directory = dir_microdata_h_20,
  base_dir  = dir_out
)

write_partitions_2021_2023(
  directory = dir_microdata_21_23,
  base_dir  = dir_out
)

write_partitions_from_2024(
  directory = dir_microdata_d_24,
  base_dir  = dir_out
)

cat(">> All partitions written to:", dir_out, "\n")