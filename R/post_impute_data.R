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

post_impute_data = function(post_imputation, outcome, covar, data, family, use_glm) {
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
                                  data = imp_dat_b,
                                  family = family,
                                  use_glm = use_glm)
  } else if (post_imputation == "miss_ind") {
    #### Create missingness indicators for remaining, unimputed values
    imp_dat_b = miss_ind_approach(outcome = outcome,
                                  covar = covar,
                                  data = imp_dat_b,
                                  family = family,
                                  use_glm = use_glm)
  } else if (post_imputation %in% c("best", "worst")) {
    #### Replace unimputed values with best or worst case scenario
    imp_dat_b = case_approach(outcome = outcome,
                              covar = covar,
                              data = imp_dat_b,
                              family = family,
                              best = post_imputation == "best",
                              use_glm = use_glm)
  }
  return(imp_dat_b)
}
