# Set working directory
setwd("~/Documents/missALI/")

# Source fitted models
## Complete-case proportion
full_cc_prop = readRDS("analysis/fitted_models/random_forest/full_complete_case_proportion.rds")
kfold_cc_prop = readRDS("analysis/fitted_models/random_forest/kfold_complete_case_proportion.rds")
## Counts of missing and unhealthy
full_num_miss = readRDS("analysis/fitted_models/random_forest/full_counts_missing_unhealthy.rds")
kfold_num_miss = readRDS("analysis/fitted_models/random_forest/kfold_counts_missing_unhealthy.rds")
## Best-case imputation (summary)
full_best = readRDS("analysis/fitted_models/random_forest/full_best_case_summary.rds")
kfold_best = readRDS("analysis/fitted_models/random_forest/kfold_best_case_summary.rds")
## Worst-case imputation (summary)
full_worst = readRDS("analysis/fitted_models/random_forest/full_worst_case_summary.rds")
kfold_worst = readRDS("analysis/fitted_models/random_forest/kfold_worst_case_summary.rds")
## Best-case imputation (separate)
full_best_sep = readRDS("analysis/fitted_models/random_forest/full_best_case_separate.rds")
kfold_best_sep = readRDS("analysis/fitted_models/random_forest/kfold_best_case_separate.rds")
## Worst-case imputation (separate)
full_worst_sep = readRDS("analysis/fitted_models/random_forest/full_worst_case_separate.rds")
kfold_worst_sep = readRDS("analysis/fitted_models/random_forest/kfold_worst_case_separate.rds")
## Missingness categories 
full_miss_cat = readRDS("analysis/fitted_models/random_forest/full_missing_category.rds")
## Pattern submodels 
full_pat_sub = readRDS("analysis/fitted_models/random_forest/full_pattern_submodels.rds")
kfold_pat_sub = readRDS("analysis/fitted_models/random_forest/kfold_pattern_submodels.rds")

# Stack their metrics data 
full_cc_prop$df |> 
  dplyr::bind_rows(kfold_cc_prop$df) |>
  dplyr::bind_rows(full_num_miss$df) |> 
  dplyr::bind_rows(kfold_num_miss$df) |> 
  dplyr::bind_rows(full_best$df) |> 
  dplyr::bind_rows(kfold_best$df) |> 
  dplyr::bind_rows(full_worst$df) |> 
  dplyr::bind_rows(kfold_worst$df) |> 
  dplyr::bind_rows(full_best_sep$df) |> 
  dplyr::bind_rows(kfold_best_sep$df) |> 
  dplyr::bind_rows(full_worst_sep$df) |> 
  dplyr::bind_rows(kfold_worst_sep$df) |> 
  dplyr::bind_rows(full_miss_cat$df) |> 
  dplyr::bind_rows(full_pat_sub$df) |> 
  dplyr::bind_rows(kfold_pat_sub$df) |> 
  write.csv("~/Documents/missALI/data/random_forest_metrics.csv")
