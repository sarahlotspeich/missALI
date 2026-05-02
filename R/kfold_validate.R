#' K-fold cross validation of ALI prediction model with missing data correction
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param zeros optional, vector of names for covariates of the zero-inflation model (like \code{zeros = c("sex", "age")}). Default is \code{zeros = NULL} (no zero inflation). If zero inflation with only an intercept in the model is requested, use \code{zeros = "intercept"}.
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param miss_method missing data method. Default is \code{miss_method = "cc_prop"}; other options include \code{"num_miss"}, \code{"best"} case scenario, and \code{"worst"} case scenario.
#' @param use_glm logical argument for whether a generalized linear model (GLM) should be used (\code{use_glm = TRUE}, the default). Otherwise, a random forest is used.
#' @param folds number of folds. Default is \code{folds = 5}.
#' @return
#' \item{all_fold_auc}{vector of area under the curve (AUC) values from each fold}
#' \item{all_fold_res}{list of lists containg all results from each fold (including trained model, test predictions, and the receiver operating characteristic (ROC) curve)}
#' @export
#' @importFrom pROC roc auc

kfold_validate = function(outcome, covar = NULL, zeros = NULL, data, family, miss_method = "none", use_glm = TRUE, folds = 5) {
  # Randomly assign folds
  data_folds = sample(x = 1:folds,
                      size = nrow(data),
                      replace = TRUE)

  # Loop over folds 1, ..., k
  kfold_all = list()
  kfold_auc = vector(length = folds)
  for (k in 1:folds) {
    ## Subset to fold k (for training data)
    train = data[data_folds == k, ]
    ## Subset to all folds except k (for testing data)
    test = data[data_folds != k, ]
    ## Fit the model + missing data approach using train data
    ### And add corresponding ALI column(s) to test data for prediction
    if (use_glm) {
      if (miss_method == "cc_prop") {
        train_res = cc_prop_approach(outcome = outcome,
                                     covar = covar,
                                     zeros = zeros,
                                     data = train,
                                     family = family,
                                     use_glm = use_glm)
        test = calc_cc_prop_ali(data = test)
      } else if (miss_method == "num_miss") {
        train_res = num_miss_approach(outcome = outcome,
                                      covar = covar,
                                      zeros = zeros,
                                      data = train,
                                      family = family,
                                      use_glm = use_glm)
        test = calc_num_miss_ali(data = test)
      } else if (miss_method %in% c("best", "worst")) {
        train_res = case_approach(outcome = outcome,
                                  covar = covar,
                                  zeros = zeros,
                                  data = train,
                                  family = family,
                                  best = miss_method == "best",
                                  use_glm = use_glm)
        test = calc_case_ali(data = test,
                             best = miss_method == "best",
                             comp_sep = FALSE)
      } else {
        warning("Please select a valid missing data correction from the available options.")
      }
      ### Calculate predictions using trained model x test data
      #### Probs for logistic, counts for Poisson
      pred_test = predict(object = train_res$fit,
                          type = "response",
                          newdata = test)

      #### Make ROC curve object
      roc_test = roc(response = test[, outcome],
                     predictor = pred_test)

      #### Extract AUC from it
      auc_test = auc(roc_test)
      kfold_auc[k] = auc_test ##### save it to vector

      ### Save all results from this fold to list (to be returned)
      kfold_all[[k]] = list(train_data = train,
                            test_data = test,
                            train_fit = train_res,
                            test_pred = pred_test,
                            test_roc = roc_test,
                            test_auc = auc_test)
    } else {
      warning("K-fold cross-validation is not currently implemented for random forest.")
    }
  }

  # Return list with the data and model
  return(list(all_fold_auc = kfold_auc,
              all_fold_res = kfold_all))
}
