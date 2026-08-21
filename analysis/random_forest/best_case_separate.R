# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_random_forest_fit.R")

# Random forest + best-case imputation (separate)
## Full sample
mod_log_best = case_approach(outcome = "ANY_ADMIT", 
                             covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                             ali = ali_comp,
                             data = hosp_dat, 
                             family = "binomial", 
                             best = TRUE, 
                             comp_sep = TRUE, 
                             use_glm = FALSE) 

# Create plots and calculate metrics 
out = summ_plot_fit(
  mod = mod_log_best, 
  col = "#a8c56e", 
  method_title = "Best Case\nImputation", 
  model_type = "Random Forest", 
  missing = "Best Case Imputation", 
  separate_components = "Separate Components")
saveRDS(object = out, 
        file = "~/Documents/missALI/analysis/fitted_models/random_forest/full_best_case_separate.rds")

# 5-Fold Cross-Validated 
## Make fold assignment reproducible (same for all models + approaches)
set.seed(918)
## Fit models
kfold_log_best_sep = kfold_validate(outcome = "ANY_ADMIT", 
                                    covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                    ali = ali_comp,
                                    data = hosp_dat, 
                                    family = "binomial", 
                                    miss_method = "best", 
                                    comp_sep = TRUE,
                                    folds = 5, 
                                    use_glm = FALSE)

## Create plots and calculate metrics 
out_kfold = summ_plot_fit_kfold_pooled(
  kfold_res = kfold_log_best_sep, 
  col = "#a8c56e", 
  method_title = "Best Case\nImputation", 
  model_type = "Random Forest", 
  missing = "Best Case Imputation", 
  separate_components = "Separate Components", 
  xlim = c(0, 0.5), 
  ylim = c(0, 0.5))

# Save AUC to dataframe 
saveRDS(object = out_kfold, 
        file = "~/Documents/missALI/analysis/fitted_models/random_forest/kfold_best_case_separate.rds")