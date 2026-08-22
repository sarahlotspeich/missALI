# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_logistic_regression_fit.R")

# Logistic regression + best-case imputation (summary)
## Full sample
mod_log_best = case_approach(outcome = "ANY_ADMIT", 
                             covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                             ali = ali_comp,
                             data = hosp_dat, 
                             family = "binomial", 
                             best = TRUE) 

# Create plots and calculate metrics 
out = summ_plot_fit_glm(
  mod = mod_log_best, 
  col = "#1b3c73", 
  method_title = "Best Case\nImputation", 
  model_type = "Logistic", 
  missing = "Best Case Imputation", 
  separate_components = "Summary Measure")
saveRDS(object = out, 
        file = "~/Documents/missALI/analysis/fitted_models/logistic_regression/full_best_case_summary.rds")

# 5-Fold Cross-Validated 
## Make fold assignment and bootstrap SEs reproducible (same for all models + approaches)
set.seed(918)
## Fit models
kfold_log_best = kfold_validate(outcome = "ANY_ADMIT", 
                                covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                ali = ali_comp,
                                data = hosp_dat, 
                                family = "binomial", 
                                miss_method = "best", 
                                folds = 5)

## Create plots and calculate metrics 
out_kfold = summ_plot_fit_kfold_pooled(
  kfold_res = kfold_log_best, 
  col = "#1b3c73", 
  method_title = "Best Case\nImputation", 
  model_type = "Logistic", 
  missing = "Best Case Imputation", 
  separate_components = "Summary Measure", 
  xlim = c(0, 0.5), 
  ylim = c(0, 0.5))

# Save AUC to dataframe 
saveRDS(object = out_kfold, 
        file = "~/Documents/missALI/analysis/fitted_models/logistic_regression/kfold_best_case_summary.rds")