#' Prediction after multiple imputation approach to fitting regression models with missing ALI components
#' This function returns average predictions per patient applying pooled coefficients to each imputed dataset
#'
#' @param imp_res list of results from the \code{mult_imp_approach()} function.
#' @return vector of average predictions per patient
#' @export
#' @importFrom mice complete
avg_predict_imp = function(imp_res) {
  # Get constants from list
  n = nrow(imp_res$data$data) ## number of observations
  m = imp_res$data$m ## number of imputations
  post = imp_res$post_imputation ## post-imputation transformation
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
    imp_dat_b = complete(data = imp_res$data, b)
    imp_dat_b = data.matrix(cbind(int = 1, imp_dat_b[, -c(1:2)])) ## remove ID, Y
    if (post == "none") {
      ## Get rid of components to exclude if no post-imputation was done
      imp_dat_b = imp_dat_b[, -which(colnames(imp_dat_b) %in% to_excl)]
    }
    pred_imp_logodds = t(imp_dat_b %*% beta_pooled)
    pooled_pred = pooled_pred +
      exp(pred_imp_logodds) / (1 + exp(pred_imp_logodds))
  }
  # Calculate average prediction
  pooled_pred = pooled_pred / m
  return(pooled_pred)
}
