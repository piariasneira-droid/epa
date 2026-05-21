source("./carga_librerias.R")
source("./scr/read_data_partitioned/read_data_funproc_partitioned_schema.R")

data_dir <- "./data/microprocessed"
out_dir  <-"./data/microdatos/epa_microdata_partitioned.parquet"

#combine_epa_partitions(input_dir   = data_dir, output_file = out_dir)

cat(">> Opening dataset...\n")
start_time <- Sys.time()

ds <- arrow::open_dataset(data_dir, partitioning = "ciclo")

cat(">> Collecting data...\n")
dt_full <- ds |> dplyr::collect()

cat(">> Writing parquet...\n")
arrow::write_parquet(dt_full, sink = out_dir)

elapsed <- Sys.time() - start_time

cat("\n✅ DONE!\n")
cat("   Rows:", format(nrow(dt_full), big.mark = ","), "\n")
cat("   Cols:", ncol(dt_full), "\n")
cat("   Time:", format(elapsed), "\n")