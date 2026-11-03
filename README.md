# **INES**: Index of Number of Events and Severity

This repository provides an R function, `compute_INES()`, which computes the INES score from a clinical dataset. INES is a continuous severity index ranging from 0 to 10, based on the lifetime burden of affective episodes, psychosis, and disease duration (DD), in years. The affective episodes considered are the following: depressive, manic, hipomanic. Number of lifetime psychiatric hospitalizations and suicide attempts are also considered.

The function is intended for researchers who already have their clinical variables recoded into the required categories.

## Required input

The function requires a data.frame with one row per individual and the columns listed below. All variables must be named and coded exactly as specified. We have now adapted our INES, so it accepts raw counts of number of events during a lifetime: it is not necessary to transform the total number of events into their categorical group before running it. Also, with our new implementation the user can now calculate their own bin-ranges or categories within their own cohort. As INES was tested over N=307, we do not recommend to do so in smaller sample sizes unless INES is expected to vary in the user's chort population. More details on this on the main manuscript.

Lifetime counts of hospitalizations, suicide attempts, and mood episodes are grouped into four ordinal categories (0–3). Grade 0 indicates no events, whereas Grades 1–3 represent increasing event frequencies, with cutoffs derived from the 33rd and 66th percentiles of the non-zero event counts in the study cohort. The presence of psychosis or psychotic symptoms is recorded as +3, and is considered a modulator of the mood episodes.

## INES formula

The categorical coding on the default formula (`use.own.bins=F`) corresponds to:

| Variable | Description | Categorical coding |
|---|---|---|
| `ID` | Unique individual identifier | Character or numeric |
| `psychosis` | Lifetime history of psychosis | 0 = No; 3 = Yes |
| `Number_hosp` | Lifetime number of hospitalizations | 0 = None; 1 = 1–2; 2 = 3–4; 3 = >4 |
| `Number_suicide` | Lifetime number of suicide attempts | 0 = None; 1 = 1; 2 = 2; 3 = >2 |
| `Number_manic` | Lifetime number of manic episodes | 0 = None; 1 = 1–2; 2 = 3–5; 3 = >5 |
| `Number_hipomanic` | Lifetime number of hypomanic episodes | 0 = None; 1 = 1–3; 2 = 4–10; 3 = >10 |
| `Number_depressive` | Lifetime number of depressive episodes | 0 = None; 1 = 1–3; 2 = 4–10; 3 = >10 |
| `disease_years` | Illness duration in years | Numeric, >1 |

**Note**: Individuals with disease_years ≤ 1 are automatically excluded from the calculation. 

For each individual, the INES score is computed as:
```
sum_events = n_hosp_cat + n_sui_cat + n_manic_cat + n_depressive_cat + psychosis + n_hipomanic_cat

INES = (log2(0.5 * (sum_events / log2(disease_years)) + 0.5 * sum_events + 1) / log2(19))*10
```

## How to use **INES**

Source the scoring function in R and provide a data.frame:

```R
source("compute_INES.R")

# Fixed MadManic bins (default)
result <- compute_INES(df_input = df)

# Cohort-specific tertile bins
result <- compute_INES(
  df_input = df,
  use.own.bins = TRUE
)

# Retrieve scores and cutoffs
df_scored <- result$data
cutoffs <- result$cutoffs
```

This function does not perform KNN imputation of missing values: rows with any missing data do not return any INES.
