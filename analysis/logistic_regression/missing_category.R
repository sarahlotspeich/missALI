# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_logistic_regression_fit.R")

# Logistic regression + worst-case imputation (separate)
## Full sample
mod_log_cat = miss_cat_approach(outcome = "ANY_ADMIT",
                                covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                ali = ali_comp,
                                data = hosp_dat,
                                family = "binomial")

# Create plots and calculate metrics 
out = summ_plot_fit_glm(
  mod = mod_log_cat, 
  col = "#e8b89a", 
  method_title = "Missingness as\na Category", 
  model_type = "Logistic", 
  missing = "Missingness as a Category", 
  separate_components = "Separate Components")
saveRDS(object = out, 
        file = "~/Documents/missALI/analysis/fitted_models/logistic_regression/full_missing_category.rds")