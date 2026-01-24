#' Prediction after multiple imputation approach to fitting regression models with missing ALI components
#' This function returns average predictions per patient applying pooled coefficients to each imputed dataset
#'
#' @param imp_res list of results from the \code{mult_imp_approach()} function.
#' @return vector of average predictions per patient
#' @export
#' @importFrom mice complete
#' @importFrom dplyr mutate mutate_at
avg_predict_imp = function(imp_res) {
  # Get constants from list
  n = nrow(imp_res$data$data) ## number of observations
  m = imp_res$data$m ## number of imputations
  post = imp_res$post_imputation ## post-imputation transformation
  comp = imp_res$components ## components treated as numeric or binary
  beta_pooled = matrix(data = imp_res$fit$estimate, ncol = 1) ## pooled coefficients
  rownames(beta_pooled) = imp_res$fit$term ## name rows

  # Initialize names with binary components
  ALI_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
               "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")
  to_excl = ALI_comp[c(6, 7)] ## If no post-imp, remove 6-7th element

  # Get predicted probabilities
  pooled_pred = matrix(data = 0,
                       nrow = 1,
                       ncol = n)

  # Loop over imputed datasets, predicting from each and saving to matrix
  for (b in 1:m) {
    imp_dat_b = cbind(int = 1, complete(data = imp_res$data, b))
    colnames(imp_dat_b)[1] = "(Intercept)"
    imp_dat_b$SEXMale = as.numeric(imp_dat_b$SEX == "Male")
    ## Convert imputed numeric components --> binary
    if (comp == "numeric") {
      imp_dat_b = create_bin_components(data = imp_dat_b)
    }
    ## Remove unused columns for specific approaches
    if (post == "none") {
      ### Get rid of components to exclude if no post-imputation was done
      imp_dat_b = imp_dat_b[, -which(colnames(imp_dat_b) %in% to_excl)]
      ### Get rid of imputed numeric components
      if (comp == "numeric") {
        imp_dat_b = imp_dat_b[, -c(2:11)]
      }
    }
    ## Post-imputation transformations
    ### complete-case proportion approach
    if (post == "cc_prop") {
      ### Calculate complete-case proportion ALI from it
      imp_dat_b$PROP_UNHEALTHY = apply(X = imp_dat_b[, ALI_comp],
                                       MARGIN = 1,
                                       FUN = mean,
                                       na.rm = TRUE)
    } else if (post == "num_miss") {
      ### Calculate number missing per patient
      imp_dat_b$NUM_MISSING = apply(X = is.na(imp_dat_b[, ALI_comp]),
                                    MARGIN = 1,
                                    FUN = sum,
                                    na.rm = TRUE)
      ### Calculate number unhealthy per patient
      imp_dat_b$NUM_UNHEALTHY = apply(X = imp_dat_b[, ALI_comp],
                                      MARGIN = 1,
                                      FUN = sum,
                                      na.rm = TRUE)
    } else if (post == "miss_ind") {
      ### Create missingness indicators for remaining, unimputed values
      # Create factor versions of ALI components with missingness indicators
      imp_dat_b = imp_dat_b |>
        mutate(A1C_FUnhealthy = as.numeric(A1C == 1),
               ALB_FUnhealthy = as.numeric(ALB == 1),
               BMI_FUnhealthy = as.numeric(BMI == 1),
               CHOL_FUnhealthy = as.numeric(CHOL == 1),
               CRP_FUnhealthy = as.numeric(CRP == 1),
               CREAT_C_FUnhealthy = as.numeric(CREAT_C == 1),
               HCST_FMissing = as.numeric(is.na(HCST)),
               TRIG_FUnhealthy = as.numeric(TRIG == 1),
               BP_DIASTOLIC_FUnhealthy = as.numeric(BP_DIASTOLIC == 1),
               BP_SYSTOLIC_FUnhealthy = as.numeric(BP_SYSTOLIC == 1))
    } else if (post %in% c("best", "worst")) {
      ### Replace unimputed values with best or worst case scenario
      # Fill in based on which case
      if (post == "best") {
        imp_dat_b = imp_dat_b |>
          mutate_at(.vars = ALI_comp,
                    replace_na,
                    0)
      } else {
        imp_dat_b = imp_dat_b |>
          mutate_at(.vars = ALI_comp,
                    replace_na,
                    1)
      }
    }
    imp_dat_b = imp_dat_b[, rownames(beta_pooled)] #### reorder cols to match beta
    imp_dat_b = data.matrix(imp_dat_b) #### convert data from df --> matrix
    pred_imp_logodds = t(imp_dat_b %*% beta_pooled)
    pooled_pred = pooled_pred +
      exp(pred_imp_logodds) / (1 + exp(pred_imp_logodds))
  }
  # Calculate average prediction
  pooled_pred = as.vector(pooled_pred / m)
  names(pooled_pred) = as.character(1:length(pooled_pred))
  return(pooled_pred)
}
