#' Complete-case proportion approach to fitting regression models with missing ALI components
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param zeros optional, vector of names for covariates of the zero-inflation model (like \code{zeros = c("sex", "age")}). Default is \code{zeros = NULL} (no zero inflation). If zero inflation with only an intercept in the model is requested, use \code{zeros = "intercept"}.
#' @param ali vector of names for the columns containing the ALI components.
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param use_glm logical argument for whether a generalized linear model (GLM) should be used (\code{use_glm = TRUE}, the default). Otherwise, a random forest is used.
#' @param rf_mtry optional (only if \code{use_glm = FALSE}), number of predictor variables randomly selected as candidates at each split in the random forest. Default is \code{rf_mtry = NULL}, which uses the \code{ranger()} default of the square root of the number of predictors (rounded down).
#' @param rf_min_node_size optional (only if \code{use_glm = FALSE}), minimum node size at which a node can be split in the random forest. Default is \code{rf_min_node_size = NULL}, which uses the \code{ranger()} default (10 for probability forests).
#' @param rf_splitrule optional (only if \code{use_glm = FALSE}), splitting rule used in the random forest. Default is \code{rf_splitrule = "gini"}; other options for probability forests include \code{"extratrees"} and \code{"hellinger"}.
#' @param rf_num_trees optional (only if \code{use_glm = FALSE}), number of trees to grow in the random forest. Default is \code{rf_num_trees = 500}.
#' @return
#' \item{data}{dataframe with proportion of unhealthy among nonmissing ALI components.}
#' \item{fit}{fitted regression model object.}
#' @export
#' @importFrom dplyr select group_by summarize left_join
#' @importFrom tidyr gather
#' @importFrom ranger ranger
#' @importFrom pscl zeroinfl
cc_prop_approach = function(outcome, covar = NULL, zeros = NULL, ali, data, family, use_glm = TRUE, rf_mtry = NULL, rf_min_node_size = NULL, rf_splitrule = "gini", rf_num_trees = 500) {
  
  # Create indicator of whether zero-inflation is needed
  use_zeroinfl = !is.null(zeros)

  ## If intercept only, overwrite zeros
  if ("intercept" %in% zeros & length(zeros) == 1) {
    zeros = c("1")
  }

  # Summarize by patient and count numbers unhealthy and missing
  sum_data = data |>
    select(PAT_MRN_ID, all_of(ali)) |>
    gather(key = "COMP", value = "VAL", -1) |>
    group_by(PAT_MRN_ID, .inform = FALSE) |>
    summarize(PROP_UNHEALTHY = mean(VAL == 1, na.rm = TRUE))

  # Merge it back into full patient data
  data = data |>
    left_join(sum_data, by = "PAT_MRN_ID")

  # Fit the model of interest
  if (use_glm) { ## Using a generalized linear model (GLM)
    if (use_zeroinfl) {
      fit_prop = zeroinfl(as.formula(paste(outcome, "~", paste(c("PROP_UNHEALTHY", covar), collapse = "+"), "|", paste(zeros, collapse = "+"))),
                          dist = family,
                          data = data)
    } else {
      fit_prop = glm(formula = as.formula(paste(outcome, "~ ", paste(c("PROP_UNHEALTHY", covar), collapse = "+"))),
                     family = family,
                     data = data)
    }
  } else { ## Using a random forest
    if (family == "binomial") {
      fit_prop = ranger(
        formula = as.formula(paste(outcome, "~ ", paste(c("PROP_UNHEALTHY", covar),
                                                        collapse = "+"))),
        data = data,
        num.trees = rf_num_trees,
        mtry = rf_mtry,
        min.node.size = rf_min_node_size,
        splitrule = rf_splitrule,
        importance = "permutation",
        probability = TRUE # For classification, to get class probabilities
      )
    } else {
      fit_prop = NULL
    }
  }

  # Return list with the data and model
  return(list(data = data,
              fit = fit_prop))
}
