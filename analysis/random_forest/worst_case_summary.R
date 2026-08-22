# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_random_forest_fit.R")

# Random forest + worst-case imputation (summary)
## Make random forest reproducible
set.seed(918)
## Full sample
mod_log_worst = case_approach(outcome = "ANY_ADMIT", 
                              covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                              ali = ali_comp,
                              data = hosp_dat, 
                              family = "binomial", 
                              best = FALSE, 
                              use_glm = FALSE) 

# Create plots and calculate metrics 
out = summ_plot_fit_rf(
  mod = mod_log_worst, 
  col = "#c1dbd0", 
  method_title = "Worst Case\nImputation", 
  model_type = "Random Forest", 
  missing = "Worst Case Imputation", 
  separate_components = "Summary Measure")
saveRDS(object = out, 
        file = "~/Documents/missALI/analysis/fitted_models/random_forest/full_worst_case_summary.rds")

# 5-Fold Cross-Validated 
## Make fold assignment and bootstrap SEs reproducible (same for all models + approaches)
set.seed(918)
## Fit models
kfold_log_worst = kfold_validate(outcome = "ANY_ADMIT", 
                                 covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                 ali = ali_comp,
                                 data = hosp_dat, 
                                 family = "binomial", 
                                 miss_method = "worst", 
                                 folds = 5, 
                                 use_glm = FALSE)

## Create plots and calculate metrics 
out_kfold = summ_plot_fit_kfold_pooled(
  kfold_res = kfold_log_worst, 
  col = "#c1dbd0", 
  method_title = "Worst Case\nImputation", 
  model_type = "Random Forest", 
  missing = "Worst Case Imputation", 
  separate_components = "Summary Measure",
  xlim = c(0, 0.5), 
  ylim = c(0, 0.5))

# Save AUC to dataframe 
saveRDS(object = out_kfold, 
        file = "~/Documents/missALI/analysis/fitted_models/random_forest/kfold_worst_case_summary.rds")