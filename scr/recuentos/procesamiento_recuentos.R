# procesamiento_recuentos.R
# Data loading ----

#' Load EPA microdata from a parquet file into a data.table
#'
#' Arrow is used only for efficient columnar reading; the result is immediately
#' materialised as a data.table so all downstream functions stay in data.table.
#'
#' @param path         Path to the parquet file
#' @param cols         Character vector of columns to load (NULL = all)
#' @param types        Named list of target types per column (NULL = keep as-is)
#' @return             A data.table
load_microdata <- function(path, cols = NULL, types = NULL) {
  ds <- arrow::open_dataset(path)
  
  if (!is.null(cols)) {
    ds <- ds |> dplyr::select(dplyr::all_of(cols))
  }
  
  dt <- data.table::as.data.table(dplyr::collect(ds))
  
  # Enforce column types if provided
  if (!is.null(types)) {
    for (col in intersect(names(dt), names(types))) {
      target <- types[[col]]
      data.table::set(dt, j = col, value = switch(
        target,
        character = as.character(dt[[col]]),
        numeric   = ,
        double    = as.numeric(dt[[col]]),
        integer   = as.integer(dt[[col]]),
        factor    = as.factor(dt[[col]]),
        dt[[col]]   # pass-through for unknown types
      ))
    }
  }
  
  dt
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
  quarter_rznotb <- count_hours(quarter_epa, FACTOR, segment_cols = seg_cols)
  
  quarter_rznotb_tot <- compute_subtotals(
    quarter_rznotb[, !"HORAS_MED"],
    marginal_cols = c("CCAA"),
    value_cols    = c("TOTAL", "OCUPADOS_TRABAJANDO")
  )
  
  quarter_rznotb_tot[, HORAS_MED := TOTAL / OCUPADOS_TRABAJANDO]
  
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
