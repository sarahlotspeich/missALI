get_p = function(post_imputation, covar, ALI_comp_excl, ALI_comp) {
  ## Save coefficients from each post-imputation model
  if (post_imputation == "cc_prop") {
    p = length(covar) + 2 ### number of coefficients = covar + int + prop
  } else if (post_imputation == "num_miss") {
    p = length(covar) + 3 ### number of coefficients = covar + int + num_ali + num_miss
  } else if (post_imputation == "none") {
    p = length(covar) + length(ALI_comp_excl) + 1 ### number of coefficients = covar + int + 8 comp
  } else if (post_imputation %in% c("best", "worst")) {
    p = length(covar) + length(ALI_comp) + 1 ### number of coefficients = covar + int + 8 comp
  } else if (post_imputation == "miss_ind") {
    p = length(covar) + 11 ### number of coefficients = covar + int + 10 comp
  }
}
## Initialize names with binary components
ALI_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
             "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")
# Essentially, all HCST would be treated as the same and CREAT_C is redundant for A1C
## So, let's exclude them from our prediction model
ALI_comp_excl = ALI_comp[-c(6, 7)] ## Remove the 6th and 7th element of ALI_comp
post_impute_data = function(post_imputation, outcome, covar, data, family, components, use_glm) {
  ### Replace with names of numeric components, if requested
  if (imp_res$components == "numeric") {
    ALI_comp = paste0("NUM_", ALI_comp)
    ALI_comp_excl = paste0("NUM_", ALI_comp_excl)
  }
  ### Post-imputation complete-case proportion approach
  if (post_imputation == "cc_prop") {
    #### Calculate complete-case proportion ALI from it
    imp_dat_b = cc_prop_approach(outcome = outcome,
                                 covar = covar,
                                 data = data,
                                 family = family,
                                 use_glm = use_glm)
  } else if (post_imputation == "num_miss") {
    #### Create missingness indicators for remaining, unimputed values
    imp_dat_b = num_miss_approach(outcome = outcome,
                                  covar = covar,
                                  data = data,
                                  family = family,
                                  use_glm = use_glm)
  } else if (post_imputation == "miss_ind") {
    #### Create missingness indicators for remaining, unimputed values
    imp_dat_b = miss_ind_approach(outcome = outcome,
                                  covar = covar,
                                  data = data,
                                  family = family,
                                  use_glm = use_glm)
  } else if (post_imputation %in% c("best", "worst")) {
    #### Replace unimputed values with best or worst case scenario
    imp_dat_b = case_approach(outcome = outcome,
                              covar = covar,
                              data = data,
                              family = family,
                              best = post_imputation == "best",
                              use_glm = use_glm)
  } else if (post_imputation == "none") {
    if (use_glm) { ## Using a generalized linear model (GLM)
      imp_dat_b = list(data = data,
                       fit = glm(formula = as.formula(paste(outcome, "~", paste(c(ALI_comp_excl, covar), collapse = "+"))),
                                 family = family,
                                 data = data))
    } else { ## Using a random forest
      ### Fit random forest
      if (family == "binomial") {
        imp_dat_b = list(data = data,
                         fit = ranger(
                           formula = as.formula(paste(outcome, "~", paste(c(ALI_comp_excl, covar), collapse = "+"))),
                           data = data,
                           num.trees = 500,
                           mtry = 2,
                           importance = "permutation",
                           probability = TRUE # For classification, to get class probabilities
                         ))
      }
    }
  }
  return(imp_dat_b)
}
