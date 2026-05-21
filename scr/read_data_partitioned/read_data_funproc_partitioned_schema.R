# processing functions for EPA microdata — partitioned schema (one parquet per CICLO)
# Output layout: ./data/microprocessed/ciclo=<CICLO>/part.parquet
# CICLO now comes from the data column (integer 1-230), saved as string

# Helpers ----

#' Derives the CICLO string from the CICLO column in the data.
#' Reads first row to get the quarter value without loading entire file.
#' @param filepath Full path to a CSV or TAB file.
#' @return Character string, e.g. "168".
.ciclo_from_column <- function(filepath) {
  # Read only first few rows to get CICLO value
  dt_sample <- fread(filepath, sep = "\t", colClasses = "character", 
                     nrows = 1, header = TRUE, nThread = 4)
  
  if (!("CICLO" %in% names(dt_sample))) {
    stop("Column CICLO not found in file: ", filepath)
  }
  
  ciclo_val <- dt_sample[1, CICLO]
  
  if (is.na(ciclo_val) || ciclo_val == "") {
    stop("CICLO value is empty or NA in file: ", filepath)
  }
  
  as.character(ciclo_val)
}

#' Builds the partition output path for a given CICLO value.
#' @param base_dir Root output directory (e.g. "./data/microprocessed").
#' @param ciclo    CICLO string, e.g. "168".
#' @return Full path to the parquet file that should be written.
.partition_path <- function(base_dir, ciclo) {
  dir <- file.path(base_dir, paste0("ciclo=", ciclo))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "part.parquet")
}

#' Checks whether a partition already exists.
#' @param base_dir Root output directory.
#' @param ciclo    CICLO string.
#' @return Logical.
.partition_exists <- function(base_dir, ciclo) {
  file.exists(.partition_path(base_dir, ciclo))
}


# Write microdata up to 2020 — partitioned ----

write_partitions_up_to_2020 <- function(directory,
                                        base_dir  = "./data/microprocessed",
                                        overwrite = FALSE) {
  csv_files <- list.files(
    path       = directory,
    pattern    = "^EPA_\\d{4}T\\d\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  cat(">> [up to 2020] Found", length(csv_files), "CSV files\n")
  
  written <- character(0)
  
  for (f in csv_files) {
    # Extract CICLO from the data column (not filename)
    ciclo <- .ciclo_from_column(f)
    out   <- .partition_path(base_dir, ciclo)
    
    if (!overwrite && .partition_exists(base_dir, ciclo)) {
      cat("   [skip] ciclo=", ciclo, " — partition already exists\n", sep = "")
      next
    }
    
    cat("   Reading:", basename(f), "(ciclo=", ciclo, ")\n")
    # Read ALL columns as character to preserve data integrity
    dt <- fread(f, sep = "\t", colClasses = "character", header = TRUE, nThread = 4)
    
    # Harmonise column names
    setnames(dt,
             old  = c("EDAD5", "HORPLU"),
             new  = c("EDAD1", "HOREPLU"),
             skip_absent = TRUE)
    
    # Harmonise factor column
    dt[, FACTOR := FACTOREL]
    
    write_parquet(dt, sink = out)
    cat("   >> Written:", out, "\n")
    written <- c(written, out)
  }
  
  cat(">> [up to 2020] Done —", length(written), "partition(s) written\n\n")
  invisible(written)
}


# Write microdata 2021-2023 — partitioned ----

write_partitions_2021_2023 <- function(directory,
                                       base_dir  = "./data/microprocessed",
                                       overwrite = FALSE) {
  csv_files <- list.files(
    path       = directory,
    pattern    = "^EPA_\\d{4}T\\d\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  cat(">> [2021-2023] Found", length(csv_files), "CSV files\n")
  
  join_keys    <- c("CICLO", "CCAA", "NVIVI", "NIVEL", "NPERS")
  cols_to_keep <- c(join_keys, "FACB2021")
  written      <- character(0)
  
  for (f in csv_files) {
    ciclo <- .ciclo_from_column(f)
    out   <- .partition_path(base_dir, ciclo)
    
    if (!overwrite && .partition_exists(base_dir, ciclo)) {
      cat("   [skip] ciclo=", ciclo, " — partition already exists\n", sep = "")
      next
    }
    
    year_q_from_filename <- regmatches(basename(f), regexpr("\\d{4}T\\d", basename(f)))
    annex_name <- paste0("EPAAnexo_", year_q_from_filename, ".tab")
    annex_path <- file.path(directory, annex_name)
    
    cat("   Reading:", basename(f), "(ciclo=", ciclo, ")\n")
    dt_main <- fread(f, sep = "\t", colClasses = "character", header = TRUE, nThread = 4)
    
    if (file.exists(annex_path)) {
      cat("   Reading annex:", annex_name, "\n")
      dt_annex <- fread(annex_path, sep = "\t", colClasses = "character",
                        header = TRUE, nThread = 4)
      
      setkeyv(dt_main,  join_keys)
      setkeyv(dt_annex, join_keys)
      dt <- merge(dt_main, dt_annex[, ..cols_to_keep], by = join_keys, all.x = TRUE)
    } else {
      warning("Annex file not found for ciclo=", ciclo, ": ", annex_path)
      dt <- dt_main
    }
    
    # ✅ FIXED: Verificar existencia de columnas antes de usarlas
    if ("HORPLU" %in% names(dt) && "HOREPLU" %in% names(dt)) {
      # Ambas existen: coalesce
      dt[, HOREPLU := fifelse(!is.na(HORPLU) & HORPLU != "", HORPLU, HOREPLU)]
    } else if ("HORPLU" %in% names(dt)) {
      # Solo HORPLU existe
      dt[, HOREPLU := HORPLU]
    }
    # Si solo HOREPLU existe, no hacer nada
    
    # Eliminar HORPLU si existe
    if ("HORPLU" %in% names(dt)) {
      dt[, HORPLU := NULL]
    }
    
    # FACB2021 a FACTOR
    if ("FACB2021" %in% names(dt)) {
      dt[, FACTOR := FACB2021]
      dt[, FACB2021 := NULL]
    }
    
    write_parquet(dt, sink = out)
    cat("   >> Written:", out, "\n")
    written <- c(written, out)
  }
  
  cat(">> [2021-2023] Done —", length(written), "partition(s) written\n\n")
  invisible(written)
}

# Write microdata from 2024 — partitioned ----

write_partitions_from_2024 <- function(directory,
                                       base_dir  = "./data/microprocessed",
                                       overwrite = FALSE) {
  tab_files <- list.files(
    path       = directory,
    pattern    = "^EPA_\\d{4}T\\d\\.tab$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  cat(">> [from 2024] Found", length(tab_files), "TAB files\n")
  
  written <- character(0)
  
  for (f in tab_files) {
    # Extract CICLO from the data column
    ciclo <- .ciclo_from_column(f)
    out   <- .partition_path(base_dir, ciclo)
    
    if (!overwrite && .partition_exists(base_dir, ciclo)) {
      cat("   [skip] ciclo=", ciclo, " — partition already exists\n", sep = "")
      next
    }
    
    cat("   Reading:", basename(f), "(ciclo=", ciclo, ")\n")
    # Read ALL columns as character
    dt <- fread(f, sep = "\t", colClasses = "character", header = TRUE, nThread = 4)
    
    # Harmonise factor column
    dt[, FACTOR := FACTOREL]
    
    write_parquet(dt, sink = out)
    cat("   >> Written:", out, "\n")
    written <- c(written, out)
  }
  
  cat(">> [from 2024] Done —", length(written), "partition(s) written\n\n")
  invisible(written)
}


# Append a new quarterly file — partitioned ----

append_quarterly_partition <- function(quarter_to_load,
                                       src_dir   = "./data/microdatos/csvs_desde_24",
                                       base_dir  = "./data/microprocessed",
                                       overwrite = FALSE) {
  load_file <- file.path(src_dir, paste0("EPA_", quarter_to_load, ".tab"))
  
  if (!file.exists(load_file)) {
    stop("Source file not found: ", load_file)
  }
  
  # Extract CICLO from the data column
  ciclo <- .ciclo_from_column(load_file)
  out   <- .partition_path(base_dir, ciclo)
  
  if (!overwrite && .partition_exists(base_dir, ciclo)) {
    message("Partition ciclo=", ciclo, " already exists. Use overwrite = TRUE to replace.")
    return(invisible(NULL))
  }
  
  cat(">> Reading:", basename(load_file), "\n")
  # Read ALL columns as character
  dt <- fread(load_file, sep = "\t", colClasses = "character", nThread = 4)
  dt[, FACTOR := FACTOREL]
  
  write_parquet(dt, sink = out)
  message(">> Partition written: ", out)
  invisible(out)
}


# Open the full partitioned dataset (lazy, no memory load) ----

open_epa_dataset <- function(base_dir = "./data/microprocessed") {
  arrow::open_dataset(base_dir, partitioning = "ciclo", format = "parquet")
}


# Compare data frame columns (unchanged helper) ----

compare_dataframe_columns <- function(df1, df2) {
  cols_df1 <- colnames(df1)
  cols_df2 <- colnames(df2)
  
  list(
    shared      = intersect(cols_df1, cols_df2),
    only_in_df1 = setdiff(cols_df1, cols_df2),
    only_in_df2 = setdiff(cols_df2, cols_df1)
  )
}

# Join parquets ----
combine_epa_duckdb <- function(
    input_dir   = "./data/microprocessed",
    output_file = "./data/microprocessed/epa_microdata.parquet"
) {
  
  cat("╔════════════════════════════════════════╗\n")
  cat("║  COMBINING EPA PARTITIONS WITH DUCKDB  ║\n")
  cat("╚════════════════════════════════════════╝\n\n")
  
  if (!dir.exists(input_dir)) {
    stop("Input directory not found: ", input_dir)
  }
  
  partitions <- list.dirs(input_dir, recursive = FALSE, full.names = FALSE)
  cat(">> Found", length(partitions), "partitions\n")
  cat(">> Input directory:", input_dir, "\n\n")
  
  cat(">> Starting DuckDB combination...\n")
  start_time <- Sys.time()
  
  # Conectar a DuckDB
  con <- duckdb::dbConnect(duckdb::duckdb())
  
  # ✅ FIXED: Usar union_by_name=True para schemas diferentes
  query <- sprintf(
    "SELECT * FROM read_parquet('%s/ciclo=*/part.parquet', union_by_name=true)",
    input_dir
  )
  
  cat(">> Executing DuckDB query (with schema union)...\n")
  dt_full <- DBI::dbGetQuery(con, query)
  
  if (nrow(dt_full) == 0) {
    warning("No data read from partitions!")
    DBI::dbDisconnect(con)
    return(invisible(NULL))
  }
  
  # Escribir resultado
  cat(">> Writing combined parquet...\n")
  arrow::write_parquet(dt_full, sink = output_file)
  
  DBI::dbDisconnect(con)
  
  elapsed <- Sys.time() - start_time
  file_size_gb <- file.size(output_file) / 1024^3
  
  cat("\n╔════════════════════════════════════════╗\n")
  cat("║  ✅ COMBINATION COMPLETED SUCCESSFULLY  ║\n")
  cat("╚════════════════════════════════════════╝\n\n")
  cat("📊 Results:\n")
  cat("   Output file: ", output_file, "\n")
  cat("   Total rows: ", format(nrow(dt_full), big.mark = ","), "\n")
  cat("   Total cols: ", ncol(dt_full), "\n")
  cat("   File size:  ", format(file_size_gb, digits = 2), " GB\n")
  cat("   Time taken: ", format(elapsed), "\n")
  cat("   Speed:      ", format(file_size_gb / as.numeric(elapsed), digits = 2), 
      " GB/min\n\n")
  
  invisible(output_file)
}

# update_epa_microdata_flat.R
# Function: update_epa_microdata_flat
#
# Purpose:
#   Called AFTER append_quarterly_partition() has written the new ciclo
#   partition into ./data/microprocessed/.
#   Reads the existing flat parquet (epa_microdata_partitioned.parquet),
#   appends the new ciclo's data, and overwrites the flat file in place.
#
# Usage (in actualizacion_trimestral_particionado.R):
#   append_quarterly_partition(trimestre_carga)
#   update_epa_microdata_flat(trimestre_carga)
#
# Arguments:
#   quarter_to_load  Character. Quarter label to append, e.g. "2026T1".
#                    Used only to locate the matching partition folder and
#                    for logging — the CICLO integer is read from the data.
#   flat_file        Full path to the flat parquet that accumulates all data.
#                    Default: "./data/microdatos/epa_microdata_partitioned.parquet"
#   base_dir         Root of the Hive-partitioned tree.
#                    Default: "./data/microprocessed"
#   overwrite        If FALSE (default) and the ciclo is already present in
#                    the flat file, the function exits without writing.
#                    Set TRUE to force re-addition of that ciclo.


update_epa_microdata_flat <- function(
    quarter_to_load,
    flat_file = "./data/microdatos/epa_microdata_partitioned.parquet",
    base_dir  = "./data/microprocessed",
    overwrite = FALSE
) {
  
  cat("╔══════════════════════════════════════════╗\n")
  cat("║  UPDATE EPA MICRODATA FLAT PARQUET       ║\n")
  cat("╚══════════════════════════════════════════╝\n\n")
  
  # Locate the new partition written by append_quarterly_partition()
  partition_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  new_part_file  <- NULL
  new_ciclo      <- NULL
  
  for (d in partition_dirs) {
    part_parquet <- file.path(d, "part.parquet")
    if (!file.exists(part_parquet)) next
    
    # Read one row to check CICLO (fast)
    sample <- arrow::read_parquet(
      part_parquet,
      col_select = dplyr::any_of(c("CICLO", "ciclo"))
    )
    
    ciclo_val <- if ("CICLO" %in% names(sample)) {
      as.character(sample$CICLO[1])
    } else if ("ciclo" %in% names(sample)) {
      as.character(sample$ciclo[1])
    } else {
      NA_character_
    }
    
    # Match the partition folder name to quarter_to_load
    folder_name <- basename(d)
    part_mtime  <- file.mtime(part_parquet)
    
    if (is.null(new_part_file) || part_mtime > file.mtime(new_part_file)) {
      new_part_file <- part_parquet
      new_ciclo     <- ciclo_val
    }
  }
  
  # Safer: resolve via quarter_to_load directly through the src file
  # (mirrors append_quarterly_partition logic)
  src_tab <- file.path("./data/microdatos/csvs_desde_24",
                       paste0("EPA_", quarter_to_load, ".tab"))
  
  if (file.exists(src_tab)) {
    # Re-derive ciclo from original tab source (most reliable)
    dt_sample <- data.table::fread(
      src_tab, sep = "\t", colClasses = "character",
      nrows = 1, header = TRUE, nThread = 4
    )
    new_ciclo <- as.character(dt_sample[1, CICLO])
    cat(">> Ciclo derived from source TAB:", new_ciclo, "\n")
  } else {
    cat(">> Source TAB not found; ciclo derived from newest partition:", new_ciclo, "\n")
  }
  
  if (is.null(new_ciclo) || is.na(new_ciclo)) {
    stop("Could not determine CICLO for quarter: ", quarter_to_load)
  }
  
  # Confirm the partition parquet for this ciclo exists
  ciclo_dir      <- file.path(base_dir, paste0("ciclo=", new_ciclo))
  ciclo_parquet  <- file.path(ciclo_dir, "part.parquet")
  
  if (!file.exists(ciclo_parquet)) {
    stop(
      "Partition parquet not found for ciclo=", new_ciclo,
      ".\nRun append_quarterly_partition(\"", quarter_to_load, "\") first.\n",
      "Expected path: ", ciclo_parquet
    )
  }
  
  cat(">> New partition file  :", ciclo_parquet, "\n")
  cat(">> Flat parquet target :", flat_file, "\n\n")
  
  # Guard: check if ciclo already in flat file
  
  if (file.exists(flat_file) && !overwrite) {
    cat(">> Scanning existing flat file for ciclo =", new_ciclo, "...\n")
    
    existing_ciclos <- tryCatch({
      tmp <- arrow::read_parquet(flat_file, col_select = dplyr::any_of(c("CICLO", "ciclo")))
      col <- if ("CICLO" %in% names(tmp)) "CICLO" else "ciclo"
      unique(as.character(tmp[[col]]))
    }, error = function(e) {
      warning("Could not read existing flat file to check ciclos: ", conditionMessage(e))
      character(0)
    })
    
    if (new_ciclo %in% existing_ciclos) {
      message(
        "Ciclo ", new_ciclo, " already present in flat file. ",
        "Use overwrite = TRUE to force re-addition."
      )
      return(invisible(NULL))
    }
    cat("   >> Ciclo", new_ciclo, "not yet present — proceeding.\n\n")
  }
  
  # Read data
  
  start_time <- Sys.time()
  
  # Read new quarter partition
  cat(">> Reading new partition (ciclo =", new_ciclo, ")...\n")
  dt_new <- data.table::as.data.table(arrow::read_parquet(ciclo_parquet))
  cat("   Rows:", format(nrow(dt_new), big.mark = ","),
      " | Cols:", ncol(dt_new), "\n")
  
  # Read existing flat parquet (if it exists)
  if (file.exists(flat_file)) {
    cat(">> Reading existing flat parquet...\n")
    dt_existing <- data.table::as.data.table(arrow::read_parquet(flat_file))
    cat("   Rows:", format(nrow(dt_existing), big.mark = ","),
        " | Cols:", ncol(dt_existing), "\n\n")
    
    # Align schemas (union_by_name style)
    cols_existing <- names(dt_existing)
    cols_new      <- names(dt_new)
    
    only_in_new      <- setdiff(cols_new, cols_existing)
    only_in_existing <- setdiff(cols_existing, cols_new)
    
    if (length(only_in_new) > 0) {
      cat(">> New columns not in existing flat file (filled with NA):\n   ",
          paste(only_in_new, collapse = ", "), "\n")
      dt_existing[, (only_in_new) := NA_character_]
    }
    
    if (length(only_in_existing) > 0) {
      cat(">> Existing columns not in new partition (filled with NA):\n   ",
          paste(only_in_existing, collapse = ", "), "\n")
      dt_new[, (only_in_existing) := NA_character_]
    }
    
    # Bind & write 
    cat(">> Binding rows...\n")
    dt_combined <- data.table::rbindlist(
      list(dt_existing, dt_new),
      use.names = TRUE,
      fill      = TRUE
    )
  } else {
    cat(">> Flat file does not exist yet — creating from new partition only.\n\n")
    dt_combined <- dt_new
  }
  
  cat(">> Combined dataset:",
      format(nrow(dt_combined), big.mark = ","), "rows |",
      ncol(dt_combined), "cols\n")
  
  # Ensure output directory exists
  out_dir <- dirname(flat_file)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  cat(">> Writing updated flat parquet...\n")
  arrow::write_parquet(dt_combined, sink = flat_file)
  
  elapsed       <- Sys.time() - start_time
  file_size_mb  <- file.size(flat_file) / 1024^2
  
  cat("\n╔══════════════════════════════════════════╗\n")
  cat("║  ✅ FLAT PARQUET UPDATED SUCCESSFULLY    ║\n")
  cat("╚══════════════════════════════════════════╝\n\n")
  cat("📊 Results:\n")
  cat("   Output file :", flat_file, "\n")
  cat("   Total rows  :", format(nrow(dt_combined), big.mark = ","), "\n")
  cat("   Total cols  :", ncol(dt_combined), "\n")
  cat("   File size   :", round(file_size_mb, 1), "MB\n")
  cat("   Time taken  :", format(elapsed), "\n\n")
  
  invisible(flat_file)
}


