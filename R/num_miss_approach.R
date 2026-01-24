#' Count of missing components approach to fitting regression models with missing ALI components
#' This function returns the dataset with added columns for the numbers of unhealthy and missing ALI components and the fitted model.
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @return
#' \item{data}{dataframe with counts of unhealthy and missing ALI components.}
#' \item{fit}{fitted regression model object.}
#' @export
num_miss_approach = function(outcome, covar = NULL, data, family) {
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
  fit_num = glm(as.formula(paste(outcome, "~", paste(c("NUM_UNHEALTHY", "NUM_MISSING", covar), collapse = "+"))),
                family = family,
                data = data)

  # Return list with the data and model
  return(list(data = data,
              fit = fit_num))
}
