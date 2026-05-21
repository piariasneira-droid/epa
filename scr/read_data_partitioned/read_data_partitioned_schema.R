# EPA microdata — partitioned schema ingestion
# Output: ./data/microprocessed/ciclo=<YYYYTq>/part.parquet (one file per quarter)

# Libraries ----
source("./carga_librerias.R")

# Paths — microdata source folders ----
dir_microdata_h_20  <- "./data/microdatos/csvs_hasta_20"
dir_microdata_21_23 <- "./data/microdatos/csvs_21_23"
dir_microdata_d_24  <- "./data/microdatos/csvs_desde_24"

# Output root — all partitions land here ----
dir_out <- "./data/microprocessed"

# Load helper functions ----
source("./scr/read_data_partitioned/read_data_funproc_partitioned_schema.R")

# ── Write partitions ────────────────────────────────────────────────────────
# Each function loops over its source files independently.
# Already-written partitions are skipped automatically (overwrite = FALSE).
# Re-run safely at any time; only missing quarters will be processed.

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

# ── Result layout ────────────────────────────────────────────────────────────
# ./data/microprocessed/
#   ciclo=2005T1/part.parquet
#   ciclo=2005T2/part.parquet
#   ...
#   ciclo=2024T4/part.parquet
#
# ~80 partitions total (2005-2024), one per quarterly wave.

# ── Appending a new quarter (recurring task) ─────────────────────────────────
# When a new quarterly release arrives, run only this — no historical data touched:
#
#   append_quarterly_partition("2025T1")

# ── Reading the full series (lazy, memory-efficient) ─────────────────────────
# Use open_epa_dataset() to get an Arrow Dataset — nothing is loaded until
# you filter/collect:
#
#   ds <- open_epa_dataset(dir_out)
#
#   # Single quarter into memory
#   epa_2023T1 <- ds |>
#     filter(ciclo == "2023T1") |>
#     collect() |>
#     as.data.table()
#
#   # Full series into memory (same as the old flat parquet approach)
#   epa_full <- ds |> collect() |> as.data.table()
#
#   # Targeted query — only employed persons, last 4 quarters
#   recent <- ds |>
#     filter(ciclo %in% c("2024T1", "2024T2", "2024T3", "2024T4"),
#            SITU == "1") |>
#     select(CICLO, CCAA, FACTOR, EDAD1) |>
#     collect() |>
#     as.data.table()

cat(">> All partitions written to:", dir_out, "\n")