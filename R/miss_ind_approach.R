#' Missingness indicator approach to fitting regression models with missing ALI components
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the outcome model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param zeros optional, vector of names for covariates of the zero-inflation model (like \code{zeros = c("sex", "age")}). Default is \code{zeros = NULL} (no zero inflation). If zero inflation with only an intercept in the model is requested, use \code{zeros = "intercept"}.
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param use_glm logical argument for whether a generalized linear model (GLM) should be used (\code{use_glm = TRUE}, the default). Otherwise, a random forest is used.
#' @return
#' \item{data}{dataframe with the factor versions of the ALI components (with missingness as a level).}
#' \item{fit}{fitted regression model object.}
#' @export
#' @importFrom dplyr mutate
#' @importFrom ranger ranger
#' @importFrom pscl zeroinfl
miss_ind_approach = function(outcome, covar = NULL, zeros = NULL, data, family, use_glm = TRUE) {
  # Create indicator of whether zero-inflation is needed
  use_zeroinfl = !is.null(zeroinfl)

  ## If intercept only, overwrite zeros
  if ("intercept" %in% zeros & length(zeros) == 1) {
    zeros = c("1")
  }

  # Create factor versions of ALI components with missingness indicators
  data = data |>
    mutate(A1C_F = make_miss_factor(x = A1C),
           ALB_F = make_miss_factor(x = ALB),
           BMI_F = make_miss_factor(x = BMI),
           CHOL_F = make_miss_factor(x = CHOL),
           CRP_F = make_miss_factor(x = CRP),
           CREAT_C_F = make_miss_factor(x = CREAT_C),
           HCST_F = make_miss_factor(x = HCST),
           TRIG_F = make_miss_factor(x = TRIG),
           BP_DIASTOLIC_F = make_miss_factor(x = BP_DIASTOLIC),
           BP_SYSTOLIC_F = make_miss_factor(x = BP_SYSTOLIC))

  # Define vector of factor component names
  factor_ALI_comp = c("A1C_F", "ALB_F", "BMI_F", "CHOL_F", "CRP_F",
                      "CREAT_C_F", "HCST_F", "TRIG_F", "BP_DIASTOLIC_F","BP_SYSTOLIC_F")

  # Fit the model of interest
  if (use_glm) { ## Using a generalized linear model (GLM)
    ## Count how many unique levels per factor variable
    ### Any variables that are constant (only one level) are excluded
    count_levels = apply(X = data[, factor_ALI_comp],
                         MARGIN = 2,
                         FUN = function(x) length(unique(x)))
    # warning(paste("The following variables were constant and excluded from the model:",
    #               paste(factor_ALI_comp[count_levels == 1], collapse = ", ")))
    if (use_zeroinfl) {
      fit_ind = zeroinfl(as.formula(paste(outcome, "~", paste(c(factor_ALI_comp[count_levels > 1], covar), collapse = "+"), "|", paste(zeros, collapse = "+"))),
                         dist = family,
                         data = data)
    } else {
      fit_ind = glm(as.formula(paste(outcome, "~", paste(c(factor_ALI_comp[count_levels > 1], covar), collapse = "+"))),
                    family = family,
                    data = data)
    }
  } else { ## Using a random forest
    if (family == "binomial") {
      fit_ind = ranger(
        formula = as.formula(paste(outcome, "~", paste(c(factor_ALI_comp, covar), collapse = "+"))),
        data = data,
        num.trees = 500,
        mtry = 2,
        importance = "permutation",
        probability = TRUE # For classification, to get class probabilities
      )
    } else {
      fit_ind = NULL
    }
  }

  # Return list with the data and model
  return(list(data = data,
              fit = fit_ind))
}
