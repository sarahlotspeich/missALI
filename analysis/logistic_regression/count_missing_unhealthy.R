# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_logistic_regression_fit.R")

# Logistic regression + counts of missing and unhealthy
## Make bootstrap SEs reproducible
set.seed(918)
## Full sample
mod_log_num = num_miss_approach(outcome = "ANY_ADMIT", 
                                covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                ali = ali_comp,
                                data = hosp_dat, 
                                family = "binomial") 

# Create plots and calculate metrics 
out = summ_plot_fit(
  mod = mod_log_num, 
  col = "#e76f51", 
  method_title = "Counts of Unhealthy and\nMissing Components", 
  model_type = "Logistic", 
  missing = "Counts of Unhealthy and Missing Components", 
  separate_components = "Summary Measure")
saveRDS(object = out, 
        file = "~/Documents/missALI/analysis/fitted_models/logistic_regression/full_counts_missing_unhealthy.rds")

# 5-Fold Cross-Validated 
## Make fold assignment and bootstrap SEs reproducible (same for all models + approaches)
set.seed(918)
## Fit models
kfold_log_num_miss = kfold_validate(outcome = "ANY_ADMIT", 
                                    covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                    ali = ali_comp,
                                    data = hosp_dat, 
                                    family = "binomial", 
                                    miss_method = "num_miss", 
                                    folds = 5)

## Create plots and calculate metrics 
out_kfold = summ_plot_fit_kfold_pooled(
  kfold_res = kfold_log_num_miss, 
  col = "#e76f51", 
  method_title = "Counts of Unhealthy and\nMissing Components", 
  model_type = "Logistic", 
  missing = "Counts of Unhealthy and Missing Components",
  separate_components = "Summary Measure", 
  xlim = c(0, 0.5), 
  ylim = c(0, 0.5))

# Save AUC to dataframe 
saveRDS(object = out_kfold, 
        file = "~/Documents/missALI/analysis/fitted_models/logistic_regression/kfold_counts_missing_unhealthy.rds")