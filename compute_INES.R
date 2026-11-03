# INES — Index of Number of Events and Severity
# Revised six-item index: psychosis + five lifetime event counts.
# Base R only. Input counts are NOT pre-categorized.
#
# FALSE (default): fixed cutoffs from the revised MadManic cohort.
# TRUE: calculate per-event tertiles among positive counts in THIS input.
# In both modes, zero counts -> grade 0.
#
# IMPORTANT: This function scores complete data; it does not impute missing
# observations. Impute externally before scoring if appropriate.
#
# Output: list(data = data.frame, cutoffs = data.frame, mode = string)
# disease_years < 2 excluded by default to ensure 0 <= INES <= 10.

compute_INES <- function(df_input, use.own.bins = FALSE,
                         columns = c(
                           ID = "ID",
                           psychosis = "psychosis",
                           hospitalizations = "Number_hosp",
                           suicide_attempts = "Number_suicide",
                           manic = "Number_manic",
                           hypomanic = "Number_hipomanic",
                           depressive = "Number_depressive",
                           disease_years = "Disease years"
                         ),
                         min.disease.years = 2,
                         missing.policy = c("error", "propagate")) {

  missing.policy <- match.arg(missing.policy)
  if (!is.data.frame(df_input)) stop("'df_input' must be a data.frame.")
  if (!is.logical(use.own.bins) || length(use.own.bins) != 1L ||
      is.na(use.own.bins)) stop("'use.own.bins' must be TRUE or FALSE.")
  expected <- c("ID", "psychosis", "hospitalizations", "suicide_attempts",
                "manic", "hypomanic", "depressive", "disease_years")
  if (!is.character(columns) || !setequal(names(columns), expected) ||
      anyDuplicated(names(columns)) || anyDuplicated(unname(columns)) ||
      anyNA(columns) || any(!nzchar(columns))) {
    stop("'columns' must map each of the eight required roles to a unique column.")
  }
  columns <- columns[expected]
  absent <- setdiff(unname(columns), names(df_input))
  if (length(absent)) stop("Missing columns: ", paste(absent, collapse = ", "))
  if (length(min.disease.years) != 1L || !is.finite(min.disease.years) ||
      min.disease.years < 2) {
    stop("'min.disease.years' must be >= 2 for scores bounded between 0 and 10.")
  }

  dat <- df_input
  numeric_input <- function(x, label) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      suppressWarnings(y <- as.numeric(x))
      if (any(!is.na(x) & (trimws(x) == "" | is.na(y)))) {
        stop("Column '", label, "' contains nonnumeric values.")
      }
      x <- y
    }
    if (!is.numeric(x) || any(!is.finite(x[!is.na(x)]))) {
      stop("Column '", label, "' must be finite numeric values or NA.")
    }
    x
  }
  event_roles <- c("hospitalizations", "suicide_attempts",
                   "manic", "hypomanic", "depressive")
  for (role in c(event_roles, "disease_years")) {
    label <- columns[[role]]
    dat[[label]] <- numeric_input(dat[[label]], label)
  }
  for (role in event_roles) {
    label <- columns[[role]]
    x <- dat[[label]]
    if (any(!is.na(x) & (x < 0 | x != floor(x)))) {
      stop("Column '", label, "' must contain nonnegative integer counts.")
    }
  }

  pcol <- columns[["psychosis"]]
  psych <- dat[[pcol]]
  if (is.factor(psych)) psych <- as.character(psych)
  if (is.logical(psych)) psych <- as.integer(psych)
  psych <- numeric_input(psych, pcol)
  if (any(!is.na(psych) & !(psych %in% c(0, 1, 3)))) {
    stop("'psychosis' must contain 0/1, 0/3, logical, or NA.")
  }
  # 1 and 3 both mean presence; avoid mixing other numerical semantics.
  dat[[pcol]] <- psych

  years <- dat[[columns[["disease_years"]]]]
  if (any(!is.na(years) & years < 0)) stop("Disease years cannot be negative.")
  ids <- dat[[columns[["ID"]]]]
  if (anyNA(ids) || anyDuplicated(ids)) stop("IDs must be nonmissing and unique.")

  if (missing.policy == "error" &&
      (anyNA(dat[, unname(columns[setdiff(expected, "ID")]), drop = FALSE]))) {
    stop("Missing data in INES inputs. Impute first or use missing.policy='propagate'.")
  }

  # The reference cohort cutoffs are taken from the REVISED
  # imputation_and_INES.R (not the obsolete 1-2 / 3-5 / >5 bins).
  fixed <- data.frame(
    event = event_roles,
    cutoff1 = c(2, 1, 2, 3, 3),
    cutoff2 = c(4, 2, 5, 10, 10),
    stringsAsFactors = FALSE
  )
  cutoffs <- data.frame(
    event = event_roles,
    input_column = unname(columns[event_roles]),
    n_total = integer(length(event_roles)),
    n_positive = integer(length(event_roles)),
    p33 = rep(NA_real_, length(event_roles)),
    p66 = rep(NA_real_, length(event_roles)),
    cutoff1 = numeric(length(event_roles)),
    cutoff2 = numeric(length(event_roles)),
    grade_0 = integer(length(event_roles)),
    grade_1 = integer(length(event_roles)),
    grade_2 = integer(length(event_roles)),
    grade_3 = integer(length(event_roles)),
    grade_NA = integer(length(event_roles)),
    stringsAsFactors = FALSE
  )

  # Fit cohort-specific tertiles on the input cohort, INCLUDING participants
  # who will later be excluded for duration? No: first restrict to eligible
  # disease years, so bins refer exactly to the scored cohort.
  eligible <- !is.na(years) & years >= min.disease.years
  if (any(!eligible)) {
    message(sum(!eligible), " participant(s) excluded: disease_years < ",
            min.disease.years, " or missing.")
  }
  dat <- dat[eligible, , drop = FALSE]
  rownames(dat) <- NULL

  for (i in seq_along(event_roles)) {
    role <- event_roles[i]
    col <- columns[[role]]
    x <- dat[[col]]
    positive <- x[!is.na(x) & x > 0]
    cutoffs$n_total[i] <- sum(!is.na(x))
    cutoffs$n_positive[i] <- length(positive)
    if (use.own.bins) {
      if (length(positive) == 0L) {
        # All-zero event: neither tertile nor positive grade boundaries exist.
        a <- b <- NA_real_
      } else {
        q <- as.numeric(stats::quantile(positive, c(1/3, 2/3),
                                        type = 7, names = FALSE))
        cutoffs$p33[i] <- q[1]
        cutoffs$p66[i] <- q[2]
        # Integer boundaries. Round half up, not banker's round.
        a <- floor(q[1] + 0.5)
        b <- floor(q[2] + 0.5)
      }
    } else {
      a <- fixed$cutoff1[i]
      b <- fixed$cutoff2[i]
    }
    cutoffs$cutoff1[i] <- a
    cutoffs$cutoff2[i] <- b

    grade <- rep(NA_integer_, length(x))
    grade[!is.na(x) & x == 0] <- 0L
    pos <- !is.na(x) & x > 0
    if (any(pos)) {
      grade[pos & x <= a] <- 1L
      grade[pos & x > a & x <= b] <- 2L
      grade[pos & x > b] <- 3L
    }
    cname <- paste0("n_", role, "_cat")
    dat[[cname]] <- grade
    for (k in 0:3) cutoffs[i, paste0("grade_", k)] <- sum(grade == k, na.rm = TRUE)
    cutoffs$grade_NA[i] <- sum(is.na(grade))
    if (use.own.bins && length(positive) &&
        length(unique(grade[!is.na(grade) & grade > 0])) < 3) {
      warning("Event '", role, "' has fewer than three populated positive ",
              "grades due to ties/sparse counts.", call. = FALSE)
    }
  }

  dat$psychosis_cat <- ifelse(is.na(dat[[pcol]]), NA_integer_,
                              ifelse(dat[[pcol]] == 0, 0L, 3L))
  score_cols <- c("psychosis_cat", paste0("n_", event_roles, "_cat"))
  ok <- stats::complete.cases(dat[, score_cols, drop = FALSE])
  dat$Sum_INES <- NA_integer_
  dat$INES <- NA_real_
  if (any(ok)) {
    dat$Sum_INES[ok] <- as.integer(rowSums(dat[ok, score_cols, drop = FALSE]))
    disease <- dat[[columns[["disease_years"]]]][ok]
    dat$INES[ok] <- 10 * log2(
      (dat$Sum_INES[ok] / 2) * (1 / log2(disease) + 1) + 1
    ) / log2(19)
  }
  if (any(!is.na(dat$Sum_INES) &
          (dat$Sum_INES < 0 | dat$Sum_INES > 18))) {
    stop("Internal error: Sum_INES out of 0-18 range.")
  }
  list(data = dat, cutoffs = cutoffs,
       mode = if (use.own.bins) "cohort_tertiles" else "fixed_madmanic",
       n_excluded_duration = sum(!eligible))
}

# Typical usage:
# source("compute_INES.R")
# res <- compute_INES(df, use.own.bins = TRUE,
#                     columns = c(ID="ID", psychosis="Psychosis YES/NO",
#                       hospitalizations="Number_hosp",
#                       suicide_attempts="Number_suicide",
#                       manic="Number_manic", hypomanic="Number_hipomanic",
#                       depressive="Number_depressive",
#                       disease_years="Disease years"))
# df_scored <- res$data
# print(res$cutoffs)
# utils::write.csv(res$cutoffs, "INES_cutoffs.csv", row.names=FALSE)
