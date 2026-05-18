# Processing functions for EPA microdata reading

# Read microdata up to 2020 ----

#' Reads and combines EPA microdata files up to year 2020.
#'
#' Scans the given directory recursively for files matching the pattern
#' EPA_YYYYTq.csv and binds them into a single data frame. All columns
#' are read as character to preserve leading zeros and avoid type conflicts
#' across quarters.
#'
#' @param directory Path to the folder containing the CSV files.
#' @param save_parquet If TRUE, saves the result as a .parquet file next to
#'   the CSVs (default FALSE).
#' @return A data frame with all records stacked row-wise.

read_microdata_up_to_2020 <- function(directory, save_parquet = FALSE) {
  csv_files <- list.files(
    path       = directory,
    pattern    = "^EPA_\\d{4}T\\d\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  cat(">> [up to 2020] Found", length(csv_files), "CSV files\n")
  
  epa_list <- lapply(csv_files, function(f) {
    cat("   Reading:", basename(f), "\n")
    fread(f, sep = "\t", colClasses = "character", header = TRUE, nThread = 4)
  })
  
  epa <- rbindlist(epa_list, use.names = TRUE, fill = TRUE)
  cat(">> [up to 2020] Done —", nrow(epa), "rows loaded\n\n")
  
  if (save_parquet) {
    out_path <- file.path(directory, "epa_up_to_2020.parquet")
    write_parquet(epa, out_path)
    cat(">> Parquet saved:", out_path, "\n")
  }
  
  return(epa)
}

# Read microdata 2021-2023 ----
#' Reads and combines EPA microdata files for years 2021 to 2023.
#'
#' Two file types are processed:
#'   - EPA_YYYYTq.csv      : main microdata files.
#'   - EPAAnexo_YYYYTq.tab : annex files containing the FACB2021 weighting
#'     factor and additional identifiers.
#' The annex is left-joined onto the main table using the household and
#' person identifiers (CICLO, CCAA, NVIVI, NIVEL, NPERS).
#'
#' @param directory Path to the folder containing the CSV and TAB files.
#' @param save_parquet If TRUE, saves the result as a .parquet file next to
#'   the source files (default FALSE).
#' @return A data frame with main records enriched with the annex columns.

read_microdata_2021_2023 <- function(directory, save_parquet = FALSE) {
  csv_files <- list.files(
    path       = directory,
    pattern    = "^EPA_\\d{4}T\\d\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  cat(">> [2021-2023] Found", length(csv_files), "CSV files\n")
  
  epa_main_list <- lapply(csv_files, function(f) {
    cat("   Reading:", basename(f), "\n")
    fread(f, sep = "\t", colClasses = "character", header = TRUE, nThread = 4)
  })
  
  epa_main <- rbindlist(epa_main_list, use.names = TRUE, fill = TRUE)
  tab_files <- list.files(
    path       = directory,
    pattern    = "^EPAAnexo_\\d{4}T\\d\\.tab$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  cat(">> [2021-2023] Found", length(tab_files), "annex TAB files\n")
  epa_annex_list <- lapply(tab_files, function(f) {
    cat("   Reading:", basename(f), "\n")
    fread(f, sep = "\t", colClasses = "character", header = TRUE, nThread = 4)
  })
  epa_annex <- rbindlist(epa_annex_list, use.names = TRUE, fill = TRUE)
  
  # Fast Join using data.table keys
  join_keys <- c("CICLO", "CCAA", "NVIVI", "NIVEL", "NPERS")
  setkeyv(epa_main, join_keys)
  setkeyv(epa_annex, join_keys)
  
  # Left join syntax in data.table: x[y] syntax or merge()
  cols_to_keep <- c(join_keys, "FACB2021")
  epa <- merge(epa_main, epa_annex[, ..cols_to_keep], by = join_keys, all.x = TRUE)
  
  cat(">> [2021-2023] Done —", nrow(epa), "rows loaded\n\n")
  
  if (save_parquet) {
    out_path <- file.path(directory, "epa_2021_2023.parquet")
    write_parquet(epa, out_path)
    cat(">> Parquet saved:", out_path, "\n")
  }
  
  return(epa)
}

# Read microdata from 2024 onwards ----
#' Reads and combines EPA microdata files from year 2024 onwards.
#'
#' Scans the given directory recursively for files matching the pattern
#' EPA_YYYYTq.tab and binds them into a single data frame.
#'
#' @param directory Path to the folder containing the TAB files.
#' @param save_parquet If TRUE, saves the result as a .parquet file next to
#'   the source files (default FALSE).
#' @return A data frame with all records stacked row-wise.

read_microdata_from_2024 <- function(directory, save_parquet = FALSE) {
  
  tab_files <- list.files(
    path       = directory,
    pattern    = "^EPA_\\d{4}T\\d\\.tab$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  cat(">> [from 2024] Found", length(tab_files), "TAB files\n")
  
  epa_list <- lapply(tab_files, function(f) {
    cat("   Reading:", basename(f), "\n")
    fread(f, sep = "\t", colClasses = "character", header = TRUE, nThread = 4)
  })
  epa <- rbindlist(epa_list, use.names = TRUE, fill = TRUE)
  
  cat(">> [from 2024] Done —", nrow(epa), "rows loaded\n\n")
  
  if (save_parquet) {
    out_path <- file.path(directory, "epa_from_2024.parquet")
    write_parquet(epa, out_path)
    cat(">> Parquet saved:", out_path, "\n")
  }
  
  return(epa)
}

# Compare data frame columns ----

#' Compares the column names of two data frames.
#'
#' Returns three sets: columns shared by both, columns only in the first,
#' and columns only in the second.
#'
#' @param df1 First data frame.
#' @param df2 Second data frame.
#' @return A named list with elements \code{shared}, \code{only_in_df1},
#'   and \code{only_in_df2}.

compare_dataframe_columns <- function(df1, df2) {
  
  cols_df1 <- colnames(df1)
  cols_df2 <- colnames(df2)
  
  result <- list(
    shared      = intersect(cols_df1, cols_df2),
    only_in_df1 = setdiff(cols_df1, cols_df2),
    only_in_df2 = setdiff(cols_df2, cols_df1)
  )
  
  return(result)
}