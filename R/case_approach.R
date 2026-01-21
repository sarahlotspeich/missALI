#' Best/worst case scenario approach to fitting regression models with missing ALI components
#' This function returns the dataset with missing values replaced by 0s (best case scenario) or 1s (worst case scenario) and the fitted model.
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param best if \code{TRUE} (the default), then all missing ALI components are replaced with \code{"healthy"}; otherwise, they are replaced with \code{"unhealthy"}.
#' @return
#' \item{data}{dataframe with the factor versions of the ALI components (with missing values replaced by best/worst case scenario).}
#' \item{fit}{fitted regression model object.}
#' @export
#' @importFrom dplyr mutate_at
#' @importFrom tidyr replace_na
case_approach = function(outcome, covar = NULL, data, family, best = TRUE) {
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
  if (!is.null(covar)) {
    fit_case = glm(as.formula(paste(outcome, "~", paste(c(bin_ALI_comp, covar), collapse = "+"))),
                   family = family,
                   data = data)
  } else {
    fit_case = glm(as.formula(paste(outcome, "~", paste(bin_ALI_comp, collapse = "+"))),
                   family = family,
                   data = data)
  }

  # Return list with the data and model
  return(list(data = data,
              fit = fit_case))
}
