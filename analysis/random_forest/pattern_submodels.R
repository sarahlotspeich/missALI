# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_random_forest_fit.R")

# Random forest + worst-case imputation (separate)
## Make random forest reproducible
set.seed(918)
## Full sample
mod_log_patsub = pattern_submod_approach(outcome = "ANY_ADMIT",
                                         covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                         ali = ali_comp,
                                         data = hosp_dat,
                                         family = "binomial")

## Count how many patients had missing data patterns that needed CCS or nesting
table(mod_log_patsub$data$complete_case_submodel) ### complete-case submodel
table(mod_log_patsub$data$nested) ### nested 

# Create plots and calculate metrics 
out = summ_plot_fit_rf(
  mod = mod_log_patsub, 
  col = "#c0404a", 
  method_title = "Pattern\nSubmodels", 
  model_type = "Random Forest", 
  missing = "Pattern Submodels", 
  separate_components = "Separate Components")
saveRDS(object = out, 
        file = "~/Documents/missALI/analysis/fitted_models/random_forest/full_pattern_submodels.rds")

# 5-Fold Cross-Validated 
## Make fold assignment and bootstrap SEs reproducible (same for all models + approaches)
set.seed(918)
## Fit models
kfold_log_submod_sep = kfold_validate(outcome = "ANY_ADMIT", 
                                      covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                      ali = ali_comp,
                                      data = hosp_dat, 
                                      family = "binomial", 
                                      miss_method = "patsub", 
                                      comp_sep = TRUE,
                                      folds = 5)

## Count how many patients had missing data patterns that needed CCS or nesting
kfold_test_dat = lapply(
  kfold_log_submod_sep$all_fold_res,
  function(x) x$test_data) |>
  dplyr::bind_rows()
table(kfold_test_dat$complete_case_submodel) ### complete-case submodel
table(kfold_test_dat$nested) ### nested 

## Create plots and calculate metrics 
out_kfold = summ_plot_fit_kfold_pooled(
  kfold_res = kfold_log_submod_sep, 
  col = "#c0404a", 
  method_title = "Pattern\nSubmodels", 
  model_type = "Random Forest", 
  missing = "Pattern Submodels", 
  separate_components = "Separate Components", 
  xlim = c(0, 0.5), 
  ylim = c(0, 0.5))

# Save AUC to dataframe 
saveRDS(object = out_kfold, 
        file = "~/Documents/missALI/analysis/fitted_models/random_forest/kfold_pattern_submodels.rds")