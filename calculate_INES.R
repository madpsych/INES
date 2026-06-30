################################################################################
# INES: Index of Number of Events and Severity
# --------------------------------------------
# This function computes the INES score from a clinical dataset.
#
# REQUIRED INPUT:
# A data.frame where each row is an individual and the following
# columns are already coded as specified:
#
#   - ID                  : unique sample identifier (character or numeric)
#
#   - rapid_cycl_count    : 0 = no rapid cycling, 3 = rapid cycling
#
#   - psychosis           : 0 = no psychosis, 3 = psychosis
#
#   - n_hosp_cat          : number of hospitalizations
#                           coded as 0 = none, 1 = 1–2, 2 = 3–5, 3 = >5
#
#   - n_sui_cat           : number of suicide attempts
#                           coded as 0 = none, 1 = 1–2, 2 = 3–5, 3 = >5
#
#   - n_manic_cat         : number of manic episodes
#                           coded as 0 = none, 1 = 1–2, 2 = 3–5, 3 = >5
#
#   - n_hipomanic_cat     : number of hypomanic episodes
#                           coded as 0 = none, 1 = 1–2, 2 = 3–5, 3 = >5
#
#   - n_depressive_cat    : number of depressive episodes
#                           coded as 0 = none, 1 = 1–2, 2 = 3–5, 3 = >5
#
#   - n_mixed_cat         : number of mixed episodes
#                           coded as 0 = none, 1 = 1–2, 2 = 3–5, 3 = >5
#
#   - disease_years       : duration of illness in years (numeric > 1).
#                           Individuals with disease_years ≤ 1 are excluded.
#
# OUTPUT:
# A data.frame with:
#   - ID
#   - INES score
################################################################################

compute_INES <- function(df) {
  
  required_cols <- c(
    "ID", "rapid_cycl_count", "n_hosp_cat", "n_sui_cat", "n_manic_cat",
    "n_depressive_cat", "n_mixed_cat", "psychosis", "n_hipomanic_cat",
    "disease_years"
  )
  
  #Check columns
  missing <- setdiff(required_cols, colnames(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  #Convert numeric columns
  numeric_cols <- setdiff(required_cols, "ID")
  for (col in numeric_cols) {
    converted <- suppressWarnings(as.numeric(df[[col]]))
    if (any(is.na(converted) & !is.na(df[[col]]))) {
      warning(sprintf("Column '%s' contains non-numeric values and was converted to NA.", col))
    }
    df[[col]] <- converted
  }
  
  #Filter individuals with disease_years <= 1
  if (any(df$disease_years <= 1, na.rm = TRUE)) {
    warning("Individuals with disease_years <= 1 were excluded from INES computation.")
  }
  df <- df[df$disease_years > 1, ]
  
  # Compute INES
  sum_events <- 
    df$rapid_cycl_count +
    df$n_hosp_cat +
    df$n_sui_cat +
    df$n_manic_cat +
    df$n_depressive_cat +
    df$n_mixed_cat +
    df$psychosis +
    df$n_hipomanic_cat
  
  INES <- log2(0.5 * (sum_events / log2(df$disease_years)) + 0.5 * sum_events + 1) / log2(25)

  output_df <- data.frame(ID = df$ID, INES = INES)
  
  return(output_df)
}

#Apply INEs function
result <- compute_INES(df_input)
