# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_random_forest_fit.R")

# Random forest + complete-case proportion
## Make random forest reproducible
set.seed(918)
## Full sample
mod_log_prop = cc_prop_approach(outcome = "ANY_ADMIT", 
                                covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                ali = ali_comp,
                                data = hosp_dat, 
                                family = "binomial", 
                                use_glm = FALSE) 

# Create plots and calculate metrics 
out = summ_plot_fit(
  mod = mod_log_prop, 
  col = "#2b9d8f", 
  method_title = "Complete-Case\nProportion", 
  model_type = "Random Forest", 
  missing = "Complete-Case Proportion", 
  separate_components = "Summary Measure")
saveRDS(object = out, 
        file = "~/Documents/missALI/analysis/fitted_models/random_forest/full_complete_case_proportion.rds")

# 5-Fold Cross-Validated 
## Make fold assignment and bootstrap SEs reproducible (same for all models + approaches)
set.seed(918)
## Fit models
kfold_log_cc_prop = kfold_validate(outcome = "ANY_ADMIT", 
                                   covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                   ali = ali_comp,
                                   data = hosp_dat, 
                                   family = "binomial", 
                                   miss_method = "cc_prop", 
                                   folds = 5, 
                                   use_glm = FALSE)

## Create plots and calculate metrics 
out_kfold = summ_plot_fit_kfold_pooled(
  kfold_res = kfold_log_cc_prop, 
  col = "#2b9d8f", 
  method_title = "Complete-Case\nProportion", 
  model_type = "Random Forest", 
  missing = "Complete-Case Proportion", 
  separate_components = "Summary Measure", 
  xlim = c(0, 0.5), 
  ylim = c(0, 0.5))

# Save AUC to dataframe 
saveRDS(object = out_kfold, 
        file = "~/Documents/missALI/analysis/fitted_models/random_forest/kfold_complete_case_proportion.rds")