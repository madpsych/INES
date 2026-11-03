source('compute_INES.R')
example <- data.frame(
  ID = paste0('P', 1:12),
  psychosis = c(0,1,1,0,0,1,0,1,0,1,0,1),
  Number_hosp = c(0,1,2,2,3,4,5,7,9,9,12,20),
  Number_suicide = c(0,0,1,1,1,2,2,2,3,3,4,5),
  Number_manic = c(0,1,2,2,3,5,5,6,8,9,10,20),
  Number_hipomanic = c(0,1,2,3,4,5,10,11,12,15,20,30),
  Number_depressive = c(0,1,2,3,4,5,10,11,12,15,20,30),
  check.names = FALSE
)
example[['Disease years']] <- rep(5, 12)
fixed <- compute_INES(example)
stopifnot(identical(fixed$cutoffs$cutoff1, c(2,1,2,3,3)),
          identical(fixed$cutoffs$cutoff2, c(4,2,5,10,10)),
          fixed$data$Sum_INES[1] == 0,
          fixed$data$psychosis_cat[2] == 3,
          all(fixed$data$INES >= 0 & fixed$data$INES <= 10))
custom <- compute_INES(example, use.own.bins = TRUE)
stopifnot(all(custom$cutoffs$n_positive > 0),
          all(!is.na(custom$cutoffs$p33)),
          custom$data$n_hospitalizations_cat[1] == 0,
          all(custom$data$INES >= 0 & custom$data$INES <= 10))
# A cohort consisting entirely of zeros can still be scored.
zero <- example
zero[setdiff(names(zero), c('ID', 'psychosis','Disease years'))] <- 0
z <- suppressWarnings(compute_INES(zero, TRUE))
stopifnot(all(z$data$Sum_INES %in% c(0L, 3L)),
          all(is.na(z$cutoffs$cutoff1)))
# Missing event values may propagate without being interpreted as zero.
missing <- example
missing$Number_hosp[1] <- NA
m <- compute_INES(missing, missing.policy = 'propagate')
stopifnot(is.na(m$data$INES[1]), !is.na(m$data$INES[2]))
# Illness duration less than two years is excluded before computing tertiles.
short <- example
short[['Disease years']][1] <- 1.5
s <- compute_INES(short, TRUE)
stopifnot(nrow(s$data) == 11, s$n_excluded_duration == 1)
cat('All INES tests passed.\n')
