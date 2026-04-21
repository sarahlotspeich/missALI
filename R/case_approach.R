#' Best/worst case scenario approach to fitting regression models with missing ALI components
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param zeros optional, vector of names for covariates of the zero-inflation model (like \code{zeros = c("sex", "age")}). Default is \code{zeros = NULL} (no zero inflation). If zero inflation with only an intercept in the model is requested, use \code{zeros = "intercept"}.
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param best if \code{TRUE} (the default), then all missing ALI components are replaced with \code{"healthy"}; otherwise, they are replaced with \code{"unhealthy"}.
#' @param use_glm logical argument for whether a generalized linear model (GLM) should be used (\code{use_glm = TRUE}, the default). Otherwise, a random forest is used.
#' @param comp_sep logical argument for whether the 10 ALI components should be modeled as separate covariates in the model or be combined into a composite proportion score.
#' @return
#' \item{data}{dataframe with the factor versions of the ALI components (with missing values replaced by best/worst case scenario).}
#' \item{fit}{fitted regression model object.}
#' @export
#' @importFrom dplyr mutate_at
#' @importFrom tidyr replace_na
#' @importFrom ranger ranger
#' @importFrom pscl zeroinfl
case_approach = function(outcome, covar = NULL, zeros = NULL, data, family, best = TRUE, use_glm = TRUE, comp_sep = FALSE) {
  # Create indicator of whether zero-inflation is needed
  use_zeroinfl = !is.null(zeros)

  ## If intercept only, overwrite zeros
  if ("intercept" %in% zeros & length(zeros) == 1) {
    zeros = c("1")
  }

  # Define vector of binary component names
  bin_ALI_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
                   "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")

  # Fill in based on which case
  if (best) {
    data = data |>
      mutate_at(.vars = bin_ALI_comp,
                replace_na,
                0)
  } else {
    data = data |>
      mutate_at(.vars = bin_ALI_comp,
                replace_na,
                1)
  }

  # Fit the model of interest
  if (comp_sep) {
    if (use_glm) { ## Using a generalized linear model (GLM)
      if (use_zeroinfl) {
        fit_case = zeroinfl(as.formula(paste(outcome, "~", paste(c(bin_ALI_comp, covar), collapse = "+"), "|", paste(zeros, collapse = "+"))),
                            dist = family,
                            data = data)
      } else {
        fit_case = glm(as.formula(paste(outcome, "~", paste(c(bin_ALI_comp, covar), collapse = "+"))),
                       family = family,
                       data = data)
      }
    } else { ## Using a random forest
      if (family == "binomial") {
        fit_case = ranger(
          formula = as.formula(paste(outcome, "~", paste(c(bin_ALI_comp, covar), collapse = "+"))),
          data = data,
          num.trees = 500,
          mtry = 2,
          importance = "permutation",
          probability = TRUE # For classification, to get class probabilities
        )
      } else {
        fit_case = NULL
      }
    }
  } else {
    ## Calculates proportion of unhealthy components (after imputation)
    data$CASE_ALI = rowSums(data[, bin_ALI_comp]) / 10
    if (use_glm) { ## Using a generalized linear model (GLM)
      if (use_zeroinfl) {
        fit_case = zeroinfl(as.formula(paste(outcome, "~ CASE_ALI +", paste(covar, collapse = "+"), "|", paste(zeros, collapse = "+"))),
                            dist = family,
                            data = data)
      } else {
        fit_case = glm(as.formula(paste(outcome, "~ CASE_ALI +", paste(covar, collapse = "+"))),
                       family = family,
                       data = data)
      }
    } else { ## Using a random forest
      if (family == "binomial") {
        fit_case = ranger(
          formula = as.formula(paste(outcome, "~ CASE_ALI +", paste(covar, collapse = "+"))),
          data = data,
          num.trees = 500,
          mtry = 2,
          importance = "permutation",
          probability = TRUE # For classification, to get class probabilities
        )
      } else {
        fit_case = NULL
      }
    }
  }

  # Return list with the data and model
  return(list(data = data,
              fit = fit_case))
}
