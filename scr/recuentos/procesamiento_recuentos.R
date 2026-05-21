open_epa_dataset <- function(base_dir = "./data/microprocessed") {
  arrow::open_dataset(
    base_dir, 
    partitioning = arrow::schema(CICLO = arrow::string()), 
    format = "parquet"
  )
}

#' Load EPA microdata from a parquet file or partitioned directory into a data.table
#'
#' Arrow is used only for efficient columnar reading; the result is immediately
#' materialised as a data.table so all downstream functions stay in data.table.
#'
#' @param path         Path to the parquet file or partitioned directory
#' @param cols         Character vector of columns to load (NULL = all)
#' @param types        Named list of target types per column (NULL = keep as-is)
#' @return             A data.table
load_microdata <- function(path, cols = NULL, types = NULL) {
  # 1. Handle both a single flat file or a partitioned directory
  if (dir.exists(path)) {
    ds <- open_epa_dataset(path)
  } else {
    ds <- arrow::open_dataset(path)
  }
  
  # 2. Select columns cleanly using dplyr verbs
  # CRITICAL: We use any_of or ensure cols are present in the dataset schema
  if (!is.null(cols)) {
    # If CICLO is a partition variable, it MUST be included in the select statement
    ds <- dplyr::select(ds, dplyr::all_of(cols))
  }
  
  # 3. Bring into R directly as a data.table
  # Collecting first yields an Arrow Table, then we convert to data.table
  dt <- data.table::as.data.table(dplyr::collect(ds))
  
  # 4. Efficiently enforce column types with memory-safe reference assignment
  if (!is.null(types)) {
    # We use intersect to only loop through columns that actually exist in dt
    cols_to_convert <- intersect(names(dt), names(types))
    
    for (col in cols_to_convert) {
      target <- types[[col]]
      
      # Safely handle conversion without losing attributes
      val <- dt[[col]]
      converted_val <- switch(
        target,
        character = as.character(val),
        numeric   = as.numeric(val),
        double    = as.numeric(val),
        integer   = as.integer(val),
        factor    = as.factor(val),
        val # Default fallback
      )
      
      data.table::set(dt, j = col, value = converted_val)
    }
  }
  
  return(dt)
}

# Subtotals / marginal totals ----

#' Compute marginal subtotals for every combination of grouping columns
#'
#' For each subset of `marginal_cols`, the remaining columns are used as keys
#' and `value_cols` are summed. Marginalised columns receive the sentinel 99.
#'
#' @param dt            A data.table
#' @param marginal_cols Columns to marginalise over (all combinations computed)
#' @param value_cols    Numeric columns to sum
#' @return              data.table with original rows plus all subtotal rows
compute_subtotals <- function(dt, marginal_cols, value_cols) {
  dt <- data.table::as.data.table(dt)   # accept data.frames too
  non_value_cols <- setdiff(names(dt), value_cols)
  results <- list(dt)
  
  for (i in seq_along(marginal_cols)) {
    combns <- combn(marginal_cols, i, simplify = FALSE)
    
    for (combo in combns) {
      group_cols <- setdiff(non_value_cols, combo)
      
      subtotal <- dt[,
                     lapply(.SD, sum, na.rm = TRUE),
                     by       = group_cols,
                     .SDcols  = value_cols
      ]
      
      # Mark marginalised columns with sentinel 99
      for (col in combo) {
        data.table::set(subtotal, j = col, value = 99L)
      }
      
      results[[length(results) + 1L]] <- subtotal
    }
  }
  
  data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
}


# Lag calculation ----

#' Append lagged values of numeric columns aligned on CICLO
#'
#' A self-join on (group_cols + CICLO - lag_n) avoids sorting the full table.
#'
#' @param dt         data.table containing a CICLO column
#' @param group_cols Columns (besides CICLO) that uniquely identify a series
#' @param value_cols Numeric columns whose lagged values are appended
#' @param lag_n      Integer lag (number of CICLO periods to look back)
#' @return           dt with extra columns named `<col>_LAG_<lag_n>`
compute_lag <- function(dt, group_cols, value_cols, lag_n) {
  dt <- data.table::as.data.table(dt)
  
  lag_suffix  <- paste0("_LAG_", lag_n)
  lag_names   <- paste0(value_cols, lag_suffix)
  join_cols   <- c(group_cols, "CICLO")
  
  # Build a slim reference table shifted by lag_n
  ref <- dt[, .SD, .SDcols = c(join_cols, value_cols)]
  data.table::setnames(ref, value_cols, lag_names)
  ref[, CICLO := CICLO + lag_n]   # shift forward so the join aligns correctly
  
  data.table::setkeyv(dt,  join_cols)
  data.table::setkeyv(ref, join_cols)
  
  ref[dt]   # right-join: all rows of dt, lagged values where available
}


# AOI grouping ----

#' Recode detailed AOI codes into broad labour-force categories
#'
#' POB codes returned:
#'   1  = Employed       (AOI 3-4)
#'   2  = Unemployed     (AOI 5-6)
#'   3  = Inactive       (AOI 7-9)
#'   4  = Minors         (AOI NA)
#'  11  = Active         (AOI 3-6)
#'  21  = Working-age    (AOI 3-9)
#'  99  = Total pop      (excludes AOI == 99)
#'
#' @param dt  data.table containing AOI and TOTAL columns
#' @return    data.table in long format with POB column replacing AOI
group_aoi <- function(dt) {
  dt <- data.table::as.data.table(dt)
  base_groups <- setdiff(names(dt), c("TOTAL", "AOI"))
  
  make_group <- function(row_filter, pob_code) {
    sub <- dt[row_filter][,
                          .(TOTAL = sum(TOTAL, na.rm = TRUE)),
                          by = base_groups
    ]
    sub[, POB := pob_code]
    sub
  }
  
  result <- data.table::rbindlist(list(
    make_group(dt$AOI %in% c(3L, 4L),                      1L),   # Employed
    make_group(dt$AOI %in% c(5L, 6L),                      2L),   # Unemployed
    make_group(dt$AOI %in% c(7L, 8L, 9L),                  3L),   # Inactive
    make_group(is.na(dt$AOI),                               4L),   # Minors
    make_group(dt$AOI %in% c(3L, 4L, 5L, 6L),             11L),   # Active
    make_group(dt$AOI %in% 3L:9L,                          21L),   # Working-age
    make_group(!dt$AOI %in% 99L | is.na(dt$AOI),           99L)    # Total pop
  ), use.names = TRUE, fill = TRUE)
  
  data.table::setorderv(result, c("CICLO", "CCAA", "POB"))
  result
}


# AOI counts ----

#' Count weighted AOI observations, optionally by segmentation columns
#'
#' @param dt           data.table with microdata
#' @param weight_col   Unquoted name of the weighting column (e.g. FACTOR)
#' @param segment_cols Character vector of extra grouping columns (may be empty)
#' @return             data.table grouped by CICLO, CCAA, AOI [+ segment_cols]
count_aoi <- function(dt, weight_col, segment_cols = character(0)) {
  weight_col_str <- deparse(substitute(weight_col))
  group_vars     <- c("CICLO", "CCAA", "AOI", segment_cols)
  cols_needed    <- c(group_vars, weight_col_str)
  
  result <- dt[,
               cols_needed,
               with = FALSE
  ][,
    .(TOTAL = sum(get(weight_col_str), na.rm = TRUE)),
    by = group_vars
  ]
  
  data.table::setorderv(result, group_vars)
  result
}

#' Reshape and rename population data (AOI) to wide format
#'
#' @param dt A data.table containing columns POB, TOTAL, CICLO, and CCAA.
#' @param segment_cols A character vector with additional segmentation columns (e.g., seg_cols).
#'
#' @return A data.table in wide format with POB categories as columns.
reshape_pob_wide <- function(dt, segment_cols = c()) {
  dt_copy <- data.table::copy(dt)
  
  dt_copy[, POB := data.table::fcase(
    POB %in% c(1, "1"),   "Empleados",
    POB %in% c(2, "2"),   "Parados",
    POB %in% c(3, "3"),   "Inactivos",
    POB %in% c(4, "4"),   "Menores",
    POB %in% c(11, "11"), "Activos",
    POB %in% c(21, "21"), "PED",
    POB %in% c(99, "99"), "Poblacion",
    default = as.character(POB)
  )]
  
  # Construct dynamic LHS formula for dcast
  formula_lhs  <- paste(c("CICLO", segment_cols, "CCAA"), collapse = " + ")
  formula_wide <- as.formula(paste(formula_lhs, "~ POB"))
  
  dt_wide <- data.table::dcast(dt_copy, formula_wide, value.var = "TOTAL")
  
  # Define and safely apply column ordering
  col_order <- c("CICLO", segment_cols, "CCAA", 
                 "Empleados", "Parados", "Inactivos", "Menores", "Activos", "PED", "Población")
  
  data.table::setcolorder(dt_wide, intersect(col_order, names(dt_wide)))
  
  # Order rows explicitly by key variables
  data.table::setorder(dt_wide, CICLO, CCAA)
  
  return(dt_wide)
}

#' Calculate Labor Market Rates (Activity, Employment, Unemployment)
#'
#' @param dt A data.table in wide format containing columns Empleados, Parados, Activos, and PED.
#'
#' @return The same data.table with Tactividad, Templeabilidad, and Tparo columns added by reference.
calculate_labor_rates <- function(dt) {
  # Modify by reference using data.table's := operator for maximum efficiency
  dt[, `:=`(
    # Activity Rate = (Activos / Population Aged 16+) * 100
    Tactividad      = (Activos / PED) * 100,
    
    # Employment Rate = (Empleados / Population Aged 16+) * 100
    Templeabilidad  = (Empleados / PED) * 100,
    
    # Unemployment Rate = (Parados / Activos) * 100
    Tparo           = (Parados / Activos) * 100
  )]
  
  return(dt)
}

#' Calculate Annual Variations (YoY) for Labor Market Metrics
#'
#' @param dt A data.table containing wide population metrics and rates.
#' @param segment_cols A character vector with additional segmentation columns (e.g., seg_cols).
#'
#' @return A new data.table with absolute differences (_dif) and percentage variations (_pct) appended.
calculate_annual_variation <- function(dt, segment_cols = c()) {
  dt_current <- data.table::copy(dt)
  
  target_cols <- c("Empleados", "Parados", "Inactivos", "Menores", "Activos",
                   "PED", "Poblacion", "Tactividad", "Templeabilidad", "Tparo")
  target_cols <- intersect(target_cols, names(dt_current))
  
  # Past table: shift CICLO forward by 4 so it aligns with current
  dt_past <- data.table::copy(dt_current)[, .SD, .SDcols = c("CICLO", segment_cols, "CCAA", target_cols)]
  dt_past[, CICLO := CICLO + 4L]   
  past_cols <- paste0("past_", target_cols)
  data.table::setnames(dt_past, old = target_cols, new = past_cols)
  
  # Join 
  join_keys <- c("CICLO", segment_cols, "CCAA")
  dt_joined <- dt_past[dt_current, on = join_keys]
  
  dif_cols <- paste0(target_cols, "_dif")
  pct_cols <- paste0(target_cols, "_pct")
  
  dt_joined[, (dif_cols) := Map(`-`, mget(target_cols), mget(past_cols))]
  dt_joined[, (pct_cols) := Map(function(d, p) (d / p) * 100, mget(dif_cols), mget(past_cols))]
  
  dt_joined[, (past_cols) := NULL]
  data.table::setorder(dt_joined, CICLO, CCAA)
  
  return(dt_joined)
}
rename_annual_variation <- function(dt) {
  dt <- data.table::copy(dt)
  
  # Rename _pct to _tv
  pct_cols <- grep("_pct$", names(dt), value = TRUE)
  tv_cols  <- gsub("_pct$", "_tv", pct_cols)
  data.table::setnames(dt, pct_cols, tv_cols)
  
  return(dt)
}

# Household counts ----
#' Internal helper: assign a household role label to each person
.classify_household_roles <- function(dt) {
  dt[, HOUSEHOLD_ROLE := data.table::fcase(
    RELPP1 == 1L & AOI %in% c(3L, 4L),     "PRocu",  # Reference, employed
    RELPP1 == 1L & AOI %in% c(5L, 6L),     "PRpar",  # Reference, unemployed
    RELPP1 == 1L & AOI %in% c(7L, 8L, 9L), "PRina",  # Reference, inactive
    RELPP1 != 1L & AOI %in% c(3L, 4L),     "REocu",  # Other, employed
    RELPP1 != 1L & AOI %in% c(5L, 6L),     "REpar",  # Other, unemployed
    RELPP1 != 1L & AOI %in% c(7L, 8L, 9L), "REina",  # Other, inactive
    RELPP1 != 1L & is.na(AOI),              "REmen"   # Other, minor
  )]
  dt
}

#' Internal helper: pivot roles wide per household and derive household types
.aggregate_household_types <- function(dt, weight_col_str, segment_cols) {
  hh_group <- c("CICLO", "CCAA", "NVIVI", "HOUSEHOLD_ROLE", segment_cols)
  
  # Weighted sum per household × role
  hh <- dt[,
           .(weighted_count = sum(get(weight_col_str), na.rm = TRUE)),
           by = hh_group
  ]
  
  # Pivot wide: one column per role
  all_roles <- c("PRocu", "PRpar", "PRina", "REocu", "REpar", "REina", "REmen")
  wide <- data.table::dcast(
    hh,
    formula   = as.formula(paste(
      paste(setdiff(hh_group, "HOUSEHOLD_ROLE"), collapse = " + "),
      "~ HOUSEHOLD_ROLE"
    )),
    value.var = "weighted_count",
    fill      = 0
  )
  
  # Ensure all role columns exist (some may be absent in this data slice)
  for (role in all_roles) {
    if (!role %in% names(wide)) wide[, (role) := 0]
  }
  
  # Derive household type aggregates
  wide[, `:=`(
    Act_ocu = data.table::fifelse(
      (PRocu != 0 | REocu != 0) & PRpar == 0 & REpar == 0,
      PRocu + PRpar + PRina, NA_real_),
    Act_par = data.table::fifelse(
      (PRpar != 0 | REpar != 0) & PRocu == 0 & REocu == 0,
      PRocu + PRpar + PRina, NA_real_),
    Inac    = data.table::fifelse(
      PRocu == 0 & PRpar == 0 & REocu == 0 & REpar == 0,
      PRina, NA_real_),
    Uni_ocu = data.table::fifelse(
      REocu == 0 & REpar == 0 & REina == 0 & REmen == 0,
      PRocu, NA_real_),
    Uni_par = data.table::fifelse(
      REocu == 0 & REpar == 0 & REina == 0 & REmen == 0,
      PRpar, NA_real_),
    Uni_ina = data.table::fifelse(
      REocu == 0 & REpar == 0 & REina == 0 & REmen == 0,
      PRina, NA_real_),
    Hogares = PRocu + PRpar + PRina
  )]
  
  wide
}

#' Count weighted households by type, optionally by segmentation columns
#'
#' TIPO_HOGAR codes:
#'   1 = Act_ocu, 2 = Act_par, 3 = Inac, 5 = Uni_ocu, 6 = Uni_par,
#'   7 = Uni_ina, 8 = Hogares_uni, 9 = Hogares
#'
#' @param dt           data.table with microdata
#' @param weight_col   Unquoted name of the weighting column
#' @param segment_cols Character vector of extra grouping columns (may be empty)
#' @return             data.table in long format with TIPO_HOGAR and TOTAL
count_households <- function(dt, weight_col, segment_cols = character(0)) {
  weight_col_str <- deparse(substitute(weight_col))
  needed_cols    <- c("CICLO", "CCAA", "NVIVI", "RELPP1", "AOI",
                      segment_cols, weight_col_str)
  summary_groups <- c("CICLO", "CCAA", segment_cols)
  type_cols      <- c("Act_ocu", "Act_par", "Inac",
                      "Uni_ocu", "Uni_par", "Uni_ina", "Hogares")
  
  wide <- dt[, needed_cols, with = FALSE] |>
    .classify_household_roles() |>
    .aggregate_household_types(weight_col_str, segment_cols)
  
  # Sum type aggregates per CICLO × CCAA [× segment_cols]
  summary <- wide[,
                  c(lapply(.SD, sum, na.rm = TRUE),
                    list(Hogares_uni = sum(Uni_ocu, na.rm = TRUE) +
                           sum(Uni_par, na.rm = TRUE) +
                           sum(Uni_ina, na.rm = TRUE))),
                  by      = summary_groups,
                  .SDcols = type_cols
  ]
  
  # Reorder columns to match the Excel file layout exactly
  correct_order <- c(summary_groups, 
                     "Act_ocu", "Act_par", "Inac", 
                     "Uni_ocu", "Uni_par", "Uni_ina", 
                     "Hogares_uni", "Hogares")
  
  data.table::setcolorder(summary, correct_order)
  
  # Sort rows by grouping variables (e.g., CICLO, CCAA)
  data.table::setorderv(summary, summary_groups)
  
  return(summary)
}


# Working-hours counts ----

#' Count weighted working hours for employed persons (AOI 3-4)
#'
#' HORASE is a 4-character string: first two digits = hours, last two = minutes.
#' The sentinel value "9999" means not applicable / missing.
#'
#' @param dt           data.table with microdata
#' @param weight_col   Unquoted name of the weighting column
#' @param segment_cols Character vector of extra grouping columns (may be empty)
#' @return             data.table with TOTAL (weighted hours), HORAS_MED, OCUPADOS_TRABAJANDO
count_hours <- function(dt, weight_col, segment_cols = character(0)) {
  weight_col_str <- deparse(substitute(weight_col))
  needed_cols    <- c("CICLO", "CCAA", "AOI", "HORASE", segment_cols, weight_col_str)
  group_vars     <- c("CICLO", "CCAA", segment_cols)
  
  # Work on a filtered copy to avoid modifying the original by reference
  sub <- dt[AOI %in% c(3L, 4L), needed_cols, with = FALSE] |> data.table::copy()
  
  sub[, HORASE := formatC(as.integer(HORASE), width = 4, flag = "0")]
  sub[, hours_raw   := data.table::fifelse(HORASE != "9999",
                                           as.numeric(substr(HORASE, 1, 2)), NA_real_)]
  sub[, minutes_raw := data.table::fifelse(HORASE != "9999",
                                           as.numeric(substr(HORASE, 3, 4)), NA_real_)]
  sub[, HOURS_TOTAL := data.table::fifelse(
    hours_raw + minutes_raw / 60 == 0, NA_real_,
    hours_raw + minutes_raw / 60
  )]
  
  w <- as.name(weight_col_str)
  
  result <- sub[, .(
    TOTAL               = sum(HOURS_TOTAL * get(weight_col_str), na.rm = TRUE),
    HORAS_MED           = sum(HOURS_TOTAL * get(weight_col_str), na.rm = TRUE) /
      sum(data.table::fifelse(is.na(HOURS_TOTAL), 0,
                              get(weight_col_str)), na.rm = TRUE),
    OCUPADOS_TRABAJANDO = sum(data.table::fifelse(is.na(HOURS_TOTAL), 0,
                                                  get(weight_col_str)), na.rm = TRUE)
  ), by = group_vars]
  
  data.table::setorderv(result, group_vars)
  result
}


# Reason-not-working counts ----

#' Count weighted observations by reason for not working (RZNOTB)
#'
#' @param dt           data.table with microdata
#' @param weight_col   Unquoted name of the weighting column
#' @param segment_cols Character vector of extra grouping columns (may be empty)
#' @return             data.table grouped by CICLO, CCAA, AOI, RZNOTB [+ segment_cols]
count_rznotb <- function(dt, weight_col, segment_cols = character(0)) {
  weight_col_str <- deparse(substitute(weight_col))
  group_vars     <- c("CICLO", "CCAA", "AOI", "RZNOTB", segment_cols)
  needed_cols    <- c(group_vars, weight_col_str)
  
  result <- dt[, needed_cols, with = FALSE][,
                                            .(TOTAL = sum(get(weight_col_str), na.rm = TRUE)),
                                            by = group_vars
  ]
  
  data.table::setorderv(result, group_vars)
  result
}


# Excel output ----

#' Write a data.table to a formatted Excel workbook
#'
#' @param dt         data.table (or data.frame) to write
#' @param sheet_name Name of the worksheet
#' @param path       Output file path (.xlsx)
#' @param col_int    Column names to format as integers (0)
#' @param col_int2   Column names to format as integers with thousands (#,##0)
#' @param col_dec    Column names to format with one decimal (#,##0.0)
#' @param col_mil    Column names to format as thousands (#,##0.0,)
#' @param col_per    Column names to format as percentages (0.0%)
#' @param col_char   Column names to format as text (@)
write_excel_formatted <- function(dt, sheet_name, path,
                                  col_int  = character(0),
                                  col_int2 = character(0),
                                  col_dec  = character(0),
                                  col_mil  = character(0),
                                  col_per  = character(0),
                                  col_char = character(0)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, dt)
  
  styles <- list(
    int  = openxlsx::createStyle(numFmt = "0"),
    int2 = openxlsx::createStyle(numFmt = "#,##0"),
    dec  = openxlsx::createStyle(numFmt = "#,##0.0"),
    mil  = openxlsx::createStyle(numFmt = "#,##0.0,"),
    per  = openxlsx::createStyle(numFmt = "0.0%"),
    char = openxlsx::createStyle(numFmt = "@")
  )
  
  apply_style <- function(col_names, style) {
    for (col in col_names) {
      idx <- which(names(dt) == col)
      if (length(idx) > 0L) {
        openxlsx::addStyle(wb, sheet_name, style = style,
                           cols = idx, rows = seq(2L, nrow(dt) + 1L),
                           gridExpand = TRUE)
      }
    }
  }
  
  apply_style(col_int,  styles$int)
  apply_style(col_int2, styles$int2)
  apply_style(col_dec,  styles$dec)
  apply_style(col_mil,  styles$mil)
  apply_style(col_per,  styles$per)
  apply_style(col_char, styles$char)
  
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  openxlsx::addFilter(wb, sheet_name, cols = seq_len(ncol(dt)), rows = 1L)
  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
  
  invisible(path)
}

# Quaterly update ----
add_quarterly_households <- function(quarter_to_load, seg_cols, 
                                     selected_columns = NULL, 
                                     column_types = NULL) {
  
  # Path management
  load_file_name <- paste0("EPA_", quarter_to_load, ".tab")
  microdata_dir <- "./data/microdatos/csvs_desde_24"
  load_file_path <- file.path(microdata_dir, load_file_name)
  hh_excel_path <- "./documentos/horas_hogares/hogares.xlsx"
  
  # Load new quarter data
  quarter_epa <- fread(load_file_path, select = selected_columns, sep = "\t",
                       colClasses = unlist(column_types))
  
  quarter_epa <- quarter_epa %>%
    mutate(FACTOR = FACTOREL) %>%
    select(-FACTOREL)
  
  # Process quarter data 
  quarter_hh <- count_households(quarter_epa, FACTOR, segment_cols = seg_cols)
  
  # FIX: Fixed typo 'quarte_hh' to 'quarter_hh'
  quarter_hh_tot <- compute_subtotals(
    quarter_hh,  
    c(seg_cols, "CCAA"),  
    c("Act_ocu", "Act_par", "Inac", "Uni_ocu", "Uni_par", "Uni_ina", "Hogares_uni", "Hogares")
  )
  
  data.table::setorder(quarter_hh_tot, CICLO, CCAA)
  
  # Read previous quarters 
  hh_up_to_t <- read_excel(hh_excel_path)
  
  # Check if the data is already loaded 
  current_cycle <- max(quarter_hh_tot$CICLO, na.rm = TRUE)
  existing_cycle <- max(as.numeric(hh_up_to_t$CICLO), na.rm = TRUE)
  
  if (current_cycle == existing_cycle) {
    message("The file data is already loaded.")
    return(NULL)
  }
  
  # Append the new quarter to the historical dataframe 
  updated_hh <- bind_rows(hh_up_to_t, quarter_hh_tot) %>%
    arrange(CICLO, CCAA)
  
  # Write the resulting dataframe to the Excel file 
  write_excel_formatted(
    dt = updated_hh,
    sheet_name = "hogares",
    path = hh_excel_path,
    col_int = c("CICLO", "CCAA"), col_dec = c(), 
    col_per = c(), col_char = c(), col_int2 = c(),
    col_mil = c("Act_ocu", "Act_par", "Inac", "Uni_ocu", "Uni_par", "Uni_ina", "Hogares_uni", "Hogares")
  )
}

add_quarterly_hours <- function(quarter_to_load, seg_cols, 
                                selected_columns = NULL, 
                                column_types = NULL) {
  
  # Path management
  load_file_name <- paste0("EPA_", quarter_to_load, ".tab")
  microdata_dir <- "./data/microdatos/csvs_desde_24"
  load_file_path <- file.path(microdata_dir, load_file_name)
  hours_excel_path <- "./documentos/horas_hogares/horas.xlsx"
  
  # Load new quarter data
  quarter_epa <- fread(load_file_path, select = selected_columns, sep = "\t",
                       colClasses = unlist(column_types))
  
  quarter_epa <- quarter_epa %>%
    mutate(FACTOR = FACTOREL) %>%
    select(-FACTOREL)
  
  # Process quarter data 
  quarter_hours <- count_hours(quarter_epa, FACTOR, segment_cols = seg_cols)
  
  quarter_hours_tot <- compute_subtotals(
    quarter_hours[, !"HORAS_MED"],
    marginal_cols = c("CCAA"),
    value_cols    = c("TOTAL", "OCUPADOS_TRABAJANDO")
  )
  
  quarter_hours_tot[, HORAS_MED := TOTAL / OCUPADOS_TRABAJANDO]
  
  # Read previous quarters 
  hours_up_to_t <- read_excel(hours_excel_path)
  
  # Check if the data is already loaded 
  current_cycle <- max(quarter_hours_tot$CICLO, na.rm = TRUE)
  existing_cycle <- max(hours_up_to_t$CICLO, na.rm = TRUE)
  
  if (current_cycle == existing_cycle) {
    message("The file data is already loaded.")
    return(NULL)
  }
  
  # Append the new quarter to the historical dataframe 
  updated_hours <- bind_rows(hours_up_to_t, quarter_hours_tot) %>%
    arrange(CICLO, CCAA)
  
  # Write the resulting dataframe to the Excel file 
  write_excel_formatted(
    dt = updated_hours,
    sheet_name = "horas",
    path = hours_excel_path,
    col_int = c("CICLO", "CCAA"), 
    col_mil = c("TOTAL", "OCUPADOS_TRABAJANDO"),
    col_dec = c("HORAS_MED"), 
    col_per = c(), col_char = c(), col_int2 = c()
  )
}

add_quarterly_rznotb <- function(quarter_to_load, seg_cols, 
                                 selected_columns = NULL, 
                                 column_types = NULL) {
  
  # Path management
  load_file_name <- paste0("EPA_", quarter_to_load, ".tab")
  microdata_dir <- "./data/microdatos/csvs_desde_24"
  load_file_path <- file.path(microdata_dir, load_file_name)
  rznotb_excel_path <- "./documentos/horas_hogares/rznotb.xlsx"
  
  # Load new quarter data
  quarter_epa <- fread(load_file_path, select = selected_columns, sep = "\t",
                       colClasses = unlist(column_types))
  
  quarter_epa <- quarter_epa %>%
    mutate(FACTOR = FACTOREL) %>%
    select(-FACTOREL)
  
  # Process quarter data 
  quarter_rznotb <- count_rznotb(quarter_epa, FACTOR, segment_cols = seg_cols)
  quarter_rznotb_tot <- compute_subtotals(quarter_rznotb, c(seg_cols, "CCAA"), c("TOTAL"))
  
  # Read previous quarters 
  rznotb_up_to_t <- read_excel(rznotb_excel_path)
  
  # Check if the data is already loaded 
  current_cycle <- max(quarter_rznotb_tot$CICLO, na.rm = TRUE)
  existing_cycle <- max(rznotb_up_to_t$CICLO, na.rm = TRUE)
  
  if (current_cycle == existing_cycle) {
    message("The file data is already loaded.")
    return(NULL)
  }
  
  # Append the new quarter to the historical dataframe 
  updated_rznotb <- bind_rows(rznotb_up_to_t, as.data.frame(quarter_rznotb_tot)) %>%
    arrange(CICLO, CCAA)
  
  # Write the resulting dataframe to the Excel file 
  write_excel_formatted(
    dt = updated_rznotb, 
    sheet_name = "rznotb",
    path = rznotb_excel_path,
    col_int = c("CICLO", "CCAA"), 
    col_mil = c("TOTAL", "OCUPADOS_TRABAJANDO"),
    col_dec = c("HORAS_MED"), 
    col_per = c(), col_char = c(), col_int2 = c()
  )
}

add_quarterly_aoi <- function(quarter_to_load, seg_cols,
                              selected_columns = NULL,
                              column_types = NULL) {
  
  # Path management
  load_file_name  <- paste0("EPA_", quarter_to_load, ".tab")
  microdata_dir   <- "./data/microdatos/csvs_desde_24"
  load_file_path  <- file.path(microdata_dir, load_file_name)
  aoi_excel_path  <- "./documentos/aoi_tasas.xlsx"
  
  # Load new quarter microdata
  quarter_epa <- data.table::fread(load_file_path, select = selected_columns,
                                   sep = "\t", colClasses = unlist(column_types))
  quarter_epa <- quarter_epa %>%
    dplyr::mutate(FACTOR = FACTOREL) %>%
    dplyr::select(-FACTOREL)
  
  # Process: counts -> subtotals -> wide -> rates (NO variation yet)
  quarter_aoi <- count_aoi(quarter_epa, FACTOR, segment_cols = seg_cols)
  
  quarter_aoi_tot <- compute_subtotals(
    quarter_aoi,
    marginal_cols = c(seg_cols, "CCAA"),
    value_cols    = c("TOTAL")
  )
  
  quarter_aoi_g   <- group_aoi(quarter_aoi_tot)
  quarter_aoi_wide <- reshape_pob_wide(quarter_aoi_g, segment_cols = seg_cols)
  quarter_rates   <- calculate_labor_rates(quarter_aoi_wide)
  
  # Check if already loaded
  current_cycle  <- max(quarter_rates$CICLO, na.rm = TRUE)
  
  # Read historical file
  aoi_up_to_t <- as.data.table(readxl::read_excel(aoi_excel_path))
  existing_cycle <- max(as.numeric(aoi_up_to_t$CICLO), na.rm = TRUE)
  
  if (current_cycle == existing_cycle) {
    message("AOI data for this quarter is already loaded.")
    return(invisible(NULL))
  }
  
  # Append new quarter (rates only, no variation cols)
  rate_cols   <- c("CICLO", seg_cols, "CCAA",
                   "Empleados", "Parados", "Inactivos", "Menores",
                   "Activos", "PED", "Poblacion",
                   "Tactividad", "Templeabilidad", "Tparo")
  updated_aoi <- data.table::rbindlist(
    list(aoi_up_to_t[, intersect(rate_cols, names(aoi_up_to_t)), with = FALSE],
         quarter_rates[, intersect(rate_cols, names(quarter_rates)), with = FALSE]),
    use.names = TRUE, fill = TRUE
  )
  data.table::setorder(updated_aoi, CICLO, CCAA)
  
  # Recompute annual variation across the full updated series
  updated_aoi <- calculate_annual_variation(updated_aoi, segment_cols = seg_cols)
  
  # Write to Excel
  write_excel_formatted(
    dt         = updated_aoi,
    sheet_name = "aoi_rates",
    path       = aoi_excel_path,
    col_int    = c("CICLO", "CCAA"),
    col_mil    = c("Empleados", "Parados", "Inactivos", "Menores", "Activos", "PED", "Poblacion",
                   "Empleados_dif", "Parados_dif", "Inactivos_dif", "Menores_dif",
                   "Activos_dif", "PED_dif", "Poblacion_dif"),
    col_dec    = c("Tactividad", "Templeabilidad", "Tparo",
                   "Tactividad_dif", "Templeabilidad_dif", "Tparo_dif",
                   "Tactividad_tv", "Templeabilidad_tv", "Tparo_tv",
                   "Empleados_tv", "Parados_tv", "Inactivos_tv", "Menores_tv",
                   "Activos_tv", "PED_tv", "Poblacion_tv"),
    col_per    = character(0),
    col_char   = character(0),
    col_int2   = character(0)
  )
  
  message("AOI rates updated through CICLO ", current_cycle)
  invisible(aoi_excel_path)
}