# INES: Index of Number of Events and Severity

This repository provides an R function, `compute_INES()`, which computes the INES score from a clinical dataset. INES is a continuous severity index ranging from 0 to 1, based on the lifetime burden of affective episodes, rapid cycling, psychosis, and illness duration.

The function is intended for researchers who already have their clinical variables recoded into the required categories.

## Required input

The function expects a data.frame where each row corresponds to an individual. The following columns must be present and coded exactly as specified:

- ID: unique sample identifier (character or numeric)
- rapid_cycl_count:0 = no rapid cycling, 3 = rapid cycling
- psychosis: 0 = no psychosis, 3 = psychosis
- n_hosp_cat (number of hospitalizations): 0 = none,1 = 1–2, 2 = 3–5, 3 = >5
- n_sui_cat (number of suicide attempts): 0 = none,1 = 1–2, 2 = 3–5, 3 = >5
- n_manic_cat (number of manic episodes): 0 = none,1 = 1–2, 2 = 3–5, 3 = >5
- n_hipomanic_cat (number of hypomanic episodes): 0 = none,1 = 1–2, 2 = 3–5, 3 = >5
- n_depressive_cat (number of depressive episodes): 0 = none,1 = 1–2, 2 = 3–5, 3 = >5
- n_mixed_cat (number of mixed episodes): 0 = none,1 = 1–2, 2 = 3–5, 3 = >5
- disease_years: duration of illness in years (numeric > 1). Individuals with disease_years ≤ 1 are automatically excluded.

## INES formula

For each individual, the INES score is computed as:
```
sum_events = rapid_cycl_count + n_hosp_cat + n_sui_cat + n_manic_cat + n_depressive_cat + n_mixed_cat + psychosis + n_hipomanic_cat

INES = log2(0.5 * (sum_events / log2(disease_years)) + 0.5 * sum_events + 1) / log2(25)
```

## How to use the function

```
result_INES <- compute_INES(df_input)
```
