append_quarterly_data <- function(quarter_to_load) {
  # Initialize paths and variables
  csv_dir <- "./data/microdatos/epa_microdata.csv"
  quarterly_microdata_dir <- "./data/microdatos/csvs_desde_2024"
  load_file_name <- paste0("EPA_", quarter_to_load, ".tab")
  
  # Load data using data.table (optimized memory allocation)
  epa_quarterly <- fread(file.path(quarterly_microdata_dir, load_file_name), sep = "\t", colClasses = "character")
  epa_old <- fread(csv_dir, sep = ",", colClasses = "character")
  
  # Add FACTOR column using data.table's in-place assignment (prevents copying memory)
  epa_quarterly[, FACTOR := FACTOREL]
  
  # Calculate the latest loaded quarter
  epa_old_max <- max(as.integer(epa_old$CICLO), na.rm = TRUE)
  epa_quarterly_max <- max(as.integer(epa_quarterly$CICLO), na.rm = TRUE)
  
  # Check if the maximum CICLO value in epa_quarterly is greater than in epa_old
  if (epa_quarterly_max > epa_old_max) {
    # Combine datasets using data.table's efficient rbindlist instead of bind_rows
    epa_combined <- rbindlist(list(epa_old, epa_quarterly), use.names = TRUE, fill = TRUE)
    
    # Write optimized CSV
    fwrite(epa_combined, file = csv_dir, sep = ",")
    message("Data successfully added to the CSV file.")
  } else {
    message("The .tab file has already been added to the CSV file. No data was appended.")
  }
  
  # Clear variables from the environment to free up memory
  rm(csv_dir, quarterly_microdata_dir, epa_old, epa_old_max, epa_quarterly, epa_quarterly_max, load_file_name, quarter_to_load)
}