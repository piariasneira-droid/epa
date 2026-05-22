process_aoi_wide <- function(df, id_cols, formula_dcast) {
  # Wider table
  aoi_wide <- dcast(
    df, 
    formula_dcast, 
    value.var = "TOTAL", 
    fill = 0
  )
  
  # Reanming cols
  cols_to_rename <- grep("^[0-9]+$|^NA$", names(aoi_wide), value = TRUE)
  new_names <- paste0("AOI_COD_", cols_to_rename)
  
  setnames(aoi_wide, old = cols_to_rename, new = new_names)
  
  if ("AOI_COD_NA" %in% names(aoi_wide)) {
  } else if ("AOI_COD_V1" %in% names(aoi_wide) && is.na(levels(aoi$AOI)[1])) { 
    setnames(aoi_wide, "AOI_COD_NA", "AOI_COD_NA", skip_absent = TRUE)
  }
  
  # New columns
  aoi_wide[, `:=`(
    OCUPADOS  = AOI_COD_3 + AOI_COD_4,
    PARADOS   = AOI_COD_5 + AOI_COD_6,
    INACTIVOS = AOI_COD_7 + AOI_COD_8 + AOI_COD_9,
    MENORES   = AOI_COD_NA
  )][, `:=`(
    ACTIVOS   = OCUPADOS + PARADOS
  )][, `:=`(
    PET       = ACTIVOS + INACTIVOS
  )][, `:=`(
    POB       = MENORES + PET
  )]
  
  return(aoi_wide)
}

process_rznotb_wide <- function(df, id_cols, formula_dcast) {
  rznotb_wide <- dcast(
    df, 
    formula_dcast, 
    value.var = "TOTAL", 
    fill = 0
  )
  
  cols_to_rename <- grep("^[0-9]+$|^NA$", names(rznotb_wide), value = TRUE)
  new_names <- paste0("RZNOTB_COD_", cols_to_rename)
  
  setnames(rznotb_wide, old = cols_to_rename, new = new_names)
  
  if ("RZNOTB_COD_NA" %in% names(rznotb_wide)) {
    # Already has the correct name
  } else if ("RZNOTB_COD_V1" %in% names(rznotb_wide)) { 
    # Handle alternative data.table naming conventions for NA values dynamically:
    setnames(rznotb_wide, "RZNOTB_COD_V1", "RZNOTB_COD_NA", skip_absent = TRUE)
  }
  
  rznotb_wide[, `:=`(
    NOTB_VACACIONES      = RZNOTB_COD_1,
    NOTB_NAC_HIJO        = RZNOTB_COD_2,
    NOTB_ENFERMEDAD      = RZNOTB_COD_4,
    NOTB_PARO            = RZNOTB_COD_10,
    NOTB_ERTE_ERE        = RZNOTB_COD_11,
    NOTB_HUELGA          = RZNOTB_COD_12,
    NOTB_NO_SABE         = RZNOTB_COD_0,
    NOTB_NO_CLASIFICABLE = RZNOTB_COD_NA,
    NOTB_OTR             = RZNOTB_COD_3 + RZNOTB_COD_5 + RZNOTB_COD_6 + RZNOTB_COD_8 +
      RZNOTB_COD_9 + RZNOTB_COD_13 + RZNOTB_COD_14 + RZNOTB_COD_15
  )][, `:=`(
    NOTB_TOTAL           = NOTB_VACACIONES + NOTB_NAC_HIJO + NOTB_ENFERMEDAD + NOTB_PARO + 
      NOTB_ERTE_ERE + NOTB_HUELGA + NOTB_NO_SABE + NOTB_NO_CLASIFICABLE + NOTB_OTR
  )]
  
  return(rznotb_wide)
}