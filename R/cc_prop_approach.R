#' Complete-case proportion approach to fitting regression models with missing ALI components
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param use_glm logical argument for whether a generalized linear model (GLM) should be used (\code{use_glm = TRUE}, the default). Otherwise, a random forest is used.
#' @return
#' \item{data}{dataframe with proportion of unhealthy among nonmissing ALI components.}
#' \item{fit}{fitted regression model object.}
#' @export
#' @importFrom dplyr select group_by summarize left_join
#' @importFrom tidyr gather
#' @importFrom ranger ranger
cc_prop_approach = function(outcome, covar = NULL, data, family, use_glm = TRUE) {
  # Define vector of binary component names
  bin_ALI_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
                   "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")

  # Summarize by patient and count numbers unhealthy and missing
  sum_data = data |>
    select(PAT_MRN_ID, all_of(bin_ALI_comp)) |>
    gather(key = "COMP", value = "VAL", -1) |>
    group_by(PAT_MRN_ID, .inform = FALSE) |>
    summarize(PROP_UNHEALTHY = mean(VAL == 1, na.rm = TRUE))

  # Merge it back into full patient data
  data = data |>
    left_join(sum_data, by = "PAT_MRN_ID")

  # Fit the model of interest
  if (use_glm) { ## Using a generalized linear model (GLM)
    fit_prop = glm(formula = as.formula(paste(outcome, "~ ", paste(c("PROP_UNHEALTHY", covar),
                                                                   collapse = "+"))),
                   family = family,
                   data = data)
  } else { ## Using a random forest
    if (family == "binomial") {
      fit_prop = ranger(
        formula = as.formula(paste(outcome, "~ ", paste(c("PROP_UNHEALTHY", covar),
                                                        collapse = "+"))),
        data = data,
        num.trees = 500,
        mtry = 2,
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
