#' Prediction after multiple imputation approach to fitting regression models with missing ALI components
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
  if(imp_res$use_glm) {
    beta_pooled = matrix(data = imp_res$fit$estimate, ncol = 1) ## pooled coefficients
    rownames(beta_pooled) = imp_res$fit$term ## name rows
  }

  # Get predicted probabilities
  pooled_pred = matrix(data = 0,
                       nrow = 1,
                       ncol = n)

  # Loop over imputed datasets, predicting from each and saving to matrix\
  for (b in 1:m) {
    imp_dat_b = complete(data = imp_res$data, b)
    if (imp_res$components == "numeric") {
      imp_dat_b = create_bin_components(data = imp_dat_b)
    }
    imp_dat_fit_b = post_impute_data(post_imputation = imp_res$post_imputation,
                                     outcome = imp_res$outcome,
                                     covar = imp_res$covar,
                                     data = imp_dat_b,
                                     family = imp_res$family,
                                     components = imp_res$components,
                                     use_glm = imp_res$use_glm)
    if (imp_res$use_glm) {
      mat_imp_dat_b = model.matrix(object = formula(imp_dat_fit_b$fit),
                                   data = imp_dat_fit_b$data)
      #imp_dat_b = imp_dat_b[, rownames(beta_pooled)] #### reorder cols to match beta
      #imp_dat_b = data.matrix(imp_dat_b) #### convert data from df --> matrix
      pred_imp_logodds = t(mat_imp_dat_b[, rownames(beta_pooled)] %*% beta_pooled)
      pooled_pred = pooled_pred +
        exp(pred_imp_logodds) / (1 + exp(pred_imp_logodds))
    } else {
      pooled_pred = pooled_pred +
        imp_dat_fit_b$fit$predictions[, 1]
    }
  }
  # Calculate average prediction
  pooled_pred = as.vector(pooled_pred / m)
  names(pooled_pred) = as.character(1:length(pooled_pred))
  return(pooled_pred)
}
