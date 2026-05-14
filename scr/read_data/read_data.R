# EPA microdata — read and consolidate

# Libraries ----
library(dplyr)
library(readr)
library(arrow)
library(purrr)

# Paths — microdata folders ----
dir_microdata_h_20  <- "./data/microdatos/csvs_hasta_20"
dir_microdata_21_23 <- "./data/microdatos/csvs_21_23"
dir_microdata_d_24  <- "./data/microdatos/csvs_desde_24"

# Load helper functions ----
source("./scr/read_data/read_data_funproc.R")

# Read microdata by period ----
epa_h_20  <- read_microdata_up_to_2020(dir_microdata_h_20, save_parquet= TRUE)
epa_21_23 <- read_microdata_2021_2023(dir_microdata_21_23, save_parquet= TRUE)
epa_d_24  <- read_microdata_from_2024(dir_microdata_d_24)

# Harmonise columns — up to 2020 dataset ----
epa_h_20 <- epa_h_20 %>%
  rename(
    EDAD1   = EDAD5,
    HOREPLU = HORPLU
  ) %>%
  mutate(FACTOR = FACTOREL)

# Harmonise columns — 2021-2023 dataset ----
epa_21_23 <- epa_21_23 %>%
  mutate(
    HOREPLU = ifelse(HORPLU != "", HORPLU, HOREPLU),
    FACTOR  = FACB2021
  ) %>%
  select(-HORPLU)


# Stack all periods and export ----
cat(">> Stacking all periods...\n")
epa_full <- bind_rows(epa_h_20, epa_21_23, epa_d_24)
cat(">> Total rows:", nrow(epa_full), "\n")

# Parquet output 
out_path <- "./data/microdatos/epa_microdata.parquet"
write_parquet(epa_full, sink = out_path)

cat(">> Parquet saved:", out_path, "\n")