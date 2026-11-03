# INES: Index of Number of Events and Severity
# Input: one row per individual; event variables must already be coded.
# Uses only base R.

compute_INES <- function(df_input) {
  required <- c("ID", "psychosis", "n_hosp_cat", "n_sui_cat",
                "n_manic_cat", "n_hipomanic_cat", "n_depressive_cat",
                "disease_years")

  if (!is.data.frame(df_input)) stop("df_input must be a data.frame.")
  missing_cols <- setdiff(required, names(df_input))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  dat <- df_input
  scores <- setdiff(required, "ID")
  # Convert factors via their labels, not their internal integer codes.
  for (nm in scores) {
    x <- dat[[nm]]
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      suppressWarnings(converted <- as.numeric(x))
      if (any(!is.na(x) & is.na(converted))) {
        stop("Column '", nm, "' contains nonnumeric values.")
      }
      x <- converted
    }
    if (!is.numeric(x)) stop("Column '", nm, "' must be numeric or a numeric-coded factor.")
    if (any(!is.finite(x) & !is.na(x))) stop("Column '", nm, "' contains infinite values.")
    dat[[nm]] <- x
  }

  event_cols <- c("n_hosp_cat", "n_sui_cat", "n_manic_cat",
                  "n_hipomanic_cat", "n_depressive_cat")
  for (nm in event_cols) {
    if (any(!is.na(dat[[nm]]) & !(dat[[nm]] %in% 0:3))) {
      stop("Column '", nm, "' must contain only 0, 1, 2 or 3.")
    }
  }
  if (any(!is.na(dat$psychosis) & !(dat$psychosis %in% c(0, 3)))) {
    stop("'psychosis' must be coded 0 (no) or 3 (yes).")
  }
  if (anyNA(dat[required])) {
    stop("Missing values in required columns; impute or remove them before scoring.")
  }
  if (anyDuplicated(dat$ID)) stop("'ID' must uniquely identify individuals.")

  # This follows the original exclusion rule (>1 year), but see warning below.
  excluded <- dat$disease_years <= 1
  if (any(excluded)) {
    message(sum(excluded), " individual(s) excluded: disease_years <= 1.")
    dat <- dat[!excluded, , drop = FALSE]
  }
  if (any(dat$disease_years < 2)) {
    warning("For disease_years between 1 and 2, the formula may yield INES > 10. ",
            "Restrict to disease_years >= 2 for a guaranteed 0-10 range.")
  }

  dat$Sum_INES <- rowSums(dat[c("psychosis", event_cols)])
  dat$INES <- 10 * log2((dat$Sum_INES / 2) *
                          (1 / log2(dat$disease_years) + 1) + 1) / log2(19)
  rownames(dat) <- NULL
  dat
}
