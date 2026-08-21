# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_logistic_regression_fit.R")

# Logistic regression + worst-case imputation (separate)
## Full sample
mod_log_worst = case_approach(outcome = "ANY_ADMIT", 
                              covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                              ali = ali_comp,
                              data = hosp_dat, 
                              family = "binomial", 
                              best = FALSE, 
                              comp_sep = TRUE) 

# Create plots and calculate metrics 
out = summ_plot_fit(
  mod = mod_log_worst, 
  col = "#6a4c93", 
  method_title = "Worst Case\nImputation", 
  model_type = "Logistic", 
  missing = "Worst Case Imputation", 
  separate_components = "Separate Components")
saveRDS(object = out, 
        file = "~/Documents/missALI/analysis/fitted_models/logistic_regression/full_worst_case_separate.rds")

# 5-Fold Cross-Validated 
## Make fold assignment reproducible (same for all models + approaches)
set.seed(918)
## Fit models
kfold_log_worst_sep = kfold_validate(outcome = "ANY_ADMIT", 
                                     covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                     ali = ali_comp,
                                     data = hosp_dat, 
                                     family = "binomial", 
                                     miss_method = "worst", 
                                     comp_sep = TRUE,
                                     folds = 5)

## Create plots and calculate metrics 
out_kfold = summ_plot_fit_kfold_pooled(
  kfold_res = kfold_log_worst_sep, 
  col = "#6a4c93", 
  method_title = "Worst Case\nImputation", 
  model_type = "Logistic", 
  missing = "Worst Case Imputation", 
  separate_components = "Separate Components", 
  xlim = c(0, 0.5), 
  ylim = c(0, 0.5))

# Save AUC to dataframe 
saveRDS(object = out_kfold, 
        file = "~/Documents/missALI/analysis/fitted_models/logistic_regression/kfold_worst_case_separate.rds")