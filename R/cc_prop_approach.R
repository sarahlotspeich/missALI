#' Complete-case proportion approach to fitting regression models with missing ALI components
#' This function returns the dataset with an added column for the proportion of unhealthy ALI components (out of those that were nonmissing) and the fitted model.
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @return
#' \item{data}{dataframe with proportion of unhealthy among nonmissing ALI components.}
#' \item{fit}{fitted regression model object.}
#' @export
#' @importFrom dplyr select group_by summarize left_join
#' @importFrom tidyr gather
cc_prop_approach = function(outcome, covar = NULL, data, family) {
  # Define vector of binary component names
  bin_ALI_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
                   "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")

  # Summarize by patient and count numbers unhealthy and missing
  sum_data = hosp_dat |>
    select(PAT_MRN_ID, all_of(bin_ALI_comp)) |>
    gather(key = "COMP", value = "VAL", -1) |>
    group_by(PAT_MRN_ID, .inform = FALSE) |>
    summarize(PROP_UNHEALTHY = mean(VAL == 1, na.rm = TRUE))

  # Merge it back into full patient data
  data = data |>
    left_join(sum_data, .inform = FALSE)

  # Fit the model of interest
  if (!is.null(covar)) {
    fit_prop = glm(as.formula(paste(outcome, "~ PROP_UNHEALTHY + ", paste(covar, collapse = "+"))),
                   family = family,
                   data = data)
  } else {
    fit_prop = glm(as.formula(paste(outcome, "~ PROP_UNHEALTHY")),
                   family = family,
                   data = data)
  }

  # Return list with the data and model
  return(list(data = data,
              fit = fit_prop))
}
