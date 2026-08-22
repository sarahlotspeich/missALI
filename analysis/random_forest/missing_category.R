# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_random_forest_fit.R")

# Random forest + worst-case imputation (separate)
## Make random forest reproducible
set.seed(918)
## Full sample
mod_log_cat = miss_cat_approach(outcome = "ANY_ADMIT",
                                covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                ali = ali_comp,
                                data = hosp_dat,
                                family = "binomial", 
                                use_glm = FALSE)

# Create plots and calculate metrics 
out = summ_plot_fit_rf(
  mod = mod_log_cat, 
  col = "#e8b89a", 
  method_title = "Missingness as\na Category", 
  model_type = "Random Forest", 
  missing = "Missingness as a Category", 
  separate_components = "Separate Components")
saveRDS(object = out, 
        file = "~/Documents/missALI/analysis/fitted_models/random_forest/full_missing_category.rds")