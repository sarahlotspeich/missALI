#' Multiple imputation approach to fitting regression models with missing ALI components
#' This function returns a list of all imputed datasets and the fitted model.
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param components type of ALI components to be imputed. Current options are \code{components = "binary"} (the default) and \code{components = "numeric"}.
#' @param m number of imputations. Default is \code{m = 100}.
#' @param post_imputation optional, post-imputation transformation of the ALI components before fitting the model. Default is \code{post_imputation = "none"}; other options include \code{"cc_prop"}, \code{"miss_ind"}, and \code{"num_miss"}, which call the named approaches after imputing.
#' @return
#' \item{data}{multiple imputed dataset (mids) object, returned by the mice function}
#' \item{fit}{fitted regression model object.}
#' @export
#' @importFrom mice mice pool
mult_imp_approach = function(outcome, covar = NULL, data, family, components = "binary", m = 100, post_imputation = "none") {
  # Setup
  ## Initialize names with binary components
  ALI_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
               "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")
  ## But replace with names of numeric components, if requested
  if (components == "numeric") {
    ALI_comp = paste0("NUM_", ALI_comp)
  } else if (components != "binary") {
    stop("Invalid components argument provided. Please choose from either binary or numeric.")
  }

  # Impute missing components, including outcome and covar in models
  imp_data = mice(data = data[, c(outcome, ALI_comp, covar)],
                  m = m,
                  printFlag = FALSE)

  # Essentially, all HCST would be treated as the same and CREAT_C is redundant for A1C
  ## So, let's exclude them from our prediction model
  ALI_comp_excl = ALI_comp[-c(6, 7)] ## Remove the 6th and 7th element of ALI_comp

  # Fit the model of interest to each imputed dataset
  if (post_imputation == "none" & components == "binary") {
    fit_imp = with(imp_data,
                   glm(formula = as.formula(paste(outcome, "~", paste(c(ALI_comp_excl, covar), collapse = "+"))),
                       family = family))
    ## Pool the coefficients and variance estimates
    summ_fit_imp = summary(pool(fit_imp))
  } else {
    ## Save coefficients from each post-imputation model
    if (post_imputation == "cc_prop") {
      p = length(covar) + 2 ### number of coefficients = covar + int + prop
    } else if (post_imputation == "num_miss") {
      p = length(covar) + 3 ### number of coefficients = covar + int + num_ali + num_miss
    } else if (post_imputation == "none" || post_imputation == "miss_ind") {
      p = length(covar) + 11 ### number of coefficients = covar + int + 10 comp
    }
    per_imp_coeff = matrix(nrow = m, ncol = p)
    per_imp_vars = matrix(nrow = m, ncol = p)
    ## Loop over the imputed datasets
    for (b in 1:m) {
      ### Get complete data from bth imputation
      imp_dat_b = complete(data = imp_data, b)
      ### Post-imputation complete-case proportion approach
      if (post_imputation == "cc_prop") {
        #### Calculate complete-case proportion ALI from it
        imp_dat_b$PROP_UNHEALTHY = apply(X = imp_dat_b[, ALI_comp],
                                         MARGIN = 1,
                                         FUN = mean,
                                         na.rm = TRUE)
        #### Fit the model
        imp_fit_b = glm(formula = as.formula(paste(outcome, "~ ", paste(c("PROP_UNHEALTHY", covar), collapse = "+"))),
                        family = family,
                        data = imp_dat_b)
      } else if (post_imputation == "num_miss") {
        #### Create missingness indicators for remaining, unimputed values
        imp_dat_b = num_miss_approach(outcome = outcome,
                                      covar = covar,
                                      data = imp_dat_b,
                                      family = family)
        #### Fit the model
        fit_imp = imp_dat_b$fit
      } else if (post_imputation == "miss_ind") {
        #### Create missingness indicators for remaining, unimputed values
        imp_dat_b = miss_ind_approach(outcome = outcome,
                                      covar = covar,
                                      data = imp_dat_b,
                                      family = family)
        #### Fit the model
        fit_imp = imp_dat_b$fit
      } else if (post_imputation == "none") {
        #### Convert imputed numeric components --> binary
        imp_dat_b = create_bin_components(data = imp_dat_b)
        #### Fit the model
        fit_imp = glm(formula = as.formula(paste(outcome, "~", paste(c(ALI_comp_excl, covar), collapse = "+"))),
                      family = family,
                      data = imp_dat_b)
      }
      ### Save its coefficients to the matrix
      per_imp_coeff[b,] = imp_fit_b$coefficients
      ### And its standard errors
      per_imp_vars[b, ] = diag(vcov(imp_fit_b))
    }
    ## Calculate pooled model estimates
    ### Coefficients
    beta_pooled = colMeans(per_imp_coeff)
    beta_pooled_long = matrix(data = beta_pooled,
                              nrow = m,
                              ncol = length(beta_pooled),
                              byrow = TRUE)
    ### Standard errors
    vw = 1 / m * colSums(per_imp_vars) #### within-imputation variance
    vb = (1 + 1 / m) * colSums((per_imp_coeff - beta_pooled_long) ^ 2) / (m - 1) #### between-imputation variance
    vt = vw + vb #### total variance
    ### Calculate degrees of freedom
    lambda = ((1 + 1 / m) * vb) / vt       # fraction missing information
    df_rubin = (m - 1) / (lambda ^ 2)         # Rubin df
    ## Pool the coefficients and variance estimates
    summ_fit_imp = data.frame(term = names(imp_fit_b$coefficients),
                              estimate = beta_pooled,
                              std.error = se_beta_pooled,
                              statistic = beta_pooled / se_beta_pooled,
                              df = df_rubin,
                              p.value = pt(q = beta_pooled / se_beta_pooled,
                                           df = df_rubin))
  }

  # Return list with the data and model
  return(list(data = data,
              fit = summ_fit_imp))
}
