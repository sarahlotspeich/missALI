#' Count of missing components approach to fitting regression models with missing ALI components
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param use_glm logical argument for whether a generalized linear model (GLM) should be used (\code{use_glm = TRUE}, the default). Otherwise, a random forest is used.
#' @return
#' \item{data}{dataframe with counts of unhealthy and missing ALI components.}
#' \item{fit}{fitted regression model object.}
#' @export
#' @importFrom ranger ranger
num_miss_approach = function(outcome, covar = NULL, data, family, use_glm = TRUE) {
  # Define vector of binary component names
  ALI_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
               "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")

  # Calculate number missing per patient
  data$NUM_MISSING = apply(X = is.na(data[, ALI_comp]),
                           MARGIN = 1,
                           FUN = sum,
                           na.rm = TRUE)

  # Calculate number unhealthy per patient
  data$NUM_UNHEALTHY = apply(X = data[, ALI_comp],
                             MARGIN = 1,
                             FUN = sum,
                             na.rm = TRUE)

  # Fit the model of interest
  if (use_glm) { ## Using a generalized linear model (GLM)
    fit_num = glm(as.formula(paste(outcome, "~", paste(c("NUM_UNHEALTHY", "NUM_MISSING", covar), collapse = "+"))),
                  family = family,
                  data = data)
  } else { ## Using a random forest
    if (family == "binomial") {
      fit_num = ranger(
        formula = as.formula(paste(outcome, "~", paste(c("NUM_UNHEALTHY", "NUM_MISSING", covar), collapse = "+"))),
        data = data,
        num.trees = 500,
        mtry = 2,
        importance = "permutation",
        probability = TRUE # For classification, to get class probabilities
      )
    } else {
      fit_num = NULL
    }
  }

  # Return list with the data and model
  return(list(data = data,
              fit = fit_num))
}
