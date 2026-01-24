#' Missingness indicator approach to fitting regression models with missing ALI components
#' This function returns the dataset with added factor columns for the ALI components (including missingess as a level) and the fitted model.
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @return
#' \item{data}{dataframe with the factor versions of the ALI components (with missingness as a level).}
#' \item{fit}{fitted regression model object.}
#' @export
#' @importFrom dplyr mutate
miss_ind_approach = function(outcome, covar = NULL, data, family) {
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
  fit_ind = glm(as.formula(paste(outcome, "~", paste(c(factor_ALI_comp, covar), collapse = "+"))),
                family = family,
                data = data)

  # Return list with the data and model
  return(list(data = data,
              fit = fit_ind))
}
