#' K-fold cross validation of ALI prediction model with missing data correction
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param zeros optional, vector of names for covariates of the zero-inflation model (like \code{zeros = c("sex", "age")}). Default is \code{zeros = NULL} (no zero inflation). If zero inflation with only an intercept in the model is requested, use \code{zeros = "intercept"}.
#' @param ali vector of names for the columns containing the ALI components.
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param miss_method missing data method. Default is \code{miss_method = "cc_prop"}; other options include \code{"num_miss"}, \code{"best"} case scenario, \code{"worst"} case scenario, missingness categories (\code{"cat"}), and pattern submodels (\code{"patsub"}).
#' @param use_glm logical argument for whether a generalized linear model (GLM) should be used (\code{use_glm = TRUE}, the default). Otherwise, a random forest is used.
#' @param comp_sep logical argument for whether the 10 ALI components should be modeled as separate covariates in the model or be combined into a composite proportion score. Default is \code{comp_sep = FALSE} (summary score).
#' @param folds number of folds. Default is \code{folds = 5}.
#' @return
#' \item{all_fold_auc}{vector of area under the curve (AUC) values from each fold}
#' \item{all_fold_res}{list of lists containg all results from each fold (including trained model, test predictions, and the receiver operating characteristic (ROC) curve)}
#' @export
#' @importFrom pROC roc auc
#' @import ranger

kfold_validate = function(outcome, covar = NULL, zeros = NULL, ali, data, family, miss_method = "cc_prop", use_glm = TRUE, comp_sep = FALSE, folds = 5) {
  # Randomly assign folds
  data_folds = sample(
    x = rep(x = 1:folds, length.out = nrow(data)),
    size = nrow(data),
    replace = FALSE
    )

  # Loop over folds 1, ..., k
  kfold_all = list()
  kfold_auc = vector(length = folds)
  for (k in 1:folds) {
    ## Subset to fold k (for training data)
    train = data[data_folds != k, ]
    ## Subset to all folds except k (for testing data)
    test = data[data_folds == k, ]
    ## Fit the model + missing data approach using train data
    ### And add corresponding ALI column(s) to test data for prediction
    if (miss_method == "cc_prop") {
      if (comp_sep) {
        warning("The complete-case proportion approach can only be used with the summary score model. Please set \\code{comp_sep = FALSE} and try again.")
      } else {
        train_res = cc_prop_approach(
          outcome = outcome,
          covar = covar,
          zeros = zeros,
          ali = ali,
          data = train,
          family = family,
          use_glm = use_glm
        )
        test = calc_cc_prop_ali(
          data = test, 
          ali = ali
        )
      }
    } else if (miss_method == "num_miss") {
      train_res = num_miss_approach(
        outcome = outcome,
        covar = covar,
        zeros = zeros,
        ali = ali,
        data = train,
        family = family,
        use_glm = use_glm
      )
      test = calc_num_miss_ali(
        data = test, 
        ali = ali
      )
    } else if (miss_method %in% c("best", "worst")) {
      train_res = case_approach(
        outcome = outcome,
        covar = covar,
        zeros = zeros,
        ali = ali,
        data = train,
        family = family,
        best = miss_method == "best",
        use_glm = use_glm,
        comp_sep = comp_sep
      )
      test = calc_case_ali(
        data = test,
        ali = ali,
        best = miss_method == "best",
        comp_sep = FALSE
      )
    } else if (miss_method == "cat") {
      if (comp_sep) {
        train_res = miss_cat_approach(
          outcome = outcome,
          covar = covar,
          zeros = zeros,
          ali = ali,
          data = train,
          family = family,
          use_glm = use_glm
        )
        test = make_all_miss_factor(
          data = test, 
          ali = ali
        )
      } else {
        warning("The missingness as a category approach can only be used with the separate components model. Please set \\code{comp_sep = TRUE} and try again.")
      }
    } else if (miss_method == "patsub") {
      if (comp_sep) {
        train_res = pattern_submod_approach(
          outcome = outcome,
          covar = covar,
          zeros = zeros,
          ali = ali,
          data = train,
          family = family,
          use_glm = use_glm
        )
      } else {
        warning("The pattern submodels approach can only be used with the separate components model. Please set \\code{comp_sep = TRUE} and try again.")
      }
    } else {
      stop(warning("Please select a valid missing data correction from the available options."))
    }
    ### Calculate predictions using trained model x test data
    if(miss_method == "patsub") { #### Bespoke function for pattern submodels
      pred_test = predict_pattern_submod(
        submod_res = train_res,
        ali = ali,
        newdata = test)
    } else {
      if (use_glm) { #### Probs for logistic, counts for Poisson
          pred_test = predict(
            object = train_res$fit,
            type = "response",
            newdata = test
          )
        } else {
        pred_test = predict(
          object = train_res$fit,
          data = test,
          type = "response"
        )$predictions[, 1]
      }
    }
    #### Make ROC curve object
    roc_test = roc(
      response = test[, outcome],
      predictor = pred_test
    )

    #### Extract AUC from it
    auc_test = auc(roc_test)
    kfold_auc[k] = auc_test ##### save it to vector

    ### Save all results from this fold to list (to be returned)
    kfold_all[[k]] = list(
      train_data = train,
      test_data = test,
      train_fit = train_res,
      test_pred = pred_test,
      test_roc = roc_test,
      test_auc = auc_test
    )
  }

  # Return list with the data and model
  return(
    list(
      all_fold_auc = kfold_auc,
      all_fold_res = kfold_all
      )
    )
}
