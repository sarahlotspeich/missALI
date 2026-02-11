#' Multiple imputation approach to fitting regression models with missing ALI components
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param components type of ALI components to be imputed. Current options are \code{components = "binary"} (the default) and \code{components = "numeric"}.
#' @param m number of imputations. Default is \code{m = 100}.
#' @param post_imputation optional, post-imputation transformation of the ALI components before fitting the model. Default is \code{post_imputation = "none"}; other options include \code{"cc_prop"}, \code{"miss_ind"}, \code{"num_miss"}, \code{"best"} case scenario, and \code{"worst"} case scenario, which call the named approaches after imputing.
#' @param use_glm logical argument for whether a generalized linear model (GLM) should be used (\code{use_glm = TRUE}, the default). Otherwise, a random forest is used.
#' @return
#' \item{data}{multiple imputed dataset (mids) object, returned by the mice function}
#' \item{fit}{fitted regression model object.}
#' \item{components}{reminder of whether components were treated as binary or numeric}
#' \item{post_imputation}{reminder of whether post-imputation transformation was performed}
#' @export
#' @importFrom mice mice pool complete
#' @importFrom ranger ranger
mult_imp_approach = function(outcome, covar = NULL, data, family, components = "binary", m = 100, post_imputation = "none", use_glm = TRUE) {
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

  # Keep ID column in dataset, but don't use it in imputations
  ## Initialize predictor matrix by not letting it iterate
  init_imp_data = suppressWarnings(
    mice(data = data[, c("PAT_MRN_ID", outcome, ALI_comp, covar)],
                       m = m,
                       printFlag = FALSE,
                       maxit = 0)
  )
  pred_without_id = init_imp_data$predictorMatrix ## predictor matrix
  pred_without_id[, "PAT_MRN_ID"] = 0 ## set id column to zero to exclude

  # Impute missing components, including outcome and covar in models
  imp_data = mice(data = data[, c("PAT_MRN_ID", outcome, ALI_comp, covar)],
                  m = m,
                  printFlag = FALSE,
                  predictorMatrix = pred_without_id)

  # Essentially, all HCST would be treated as the same and CREAT_C is redundant for A1C
  ## So, let's exclude them from our prediction model
  ALI_comp_excl = ALI_comp[-c(6, 7)] ## Remove the 6th and 7th element of ALI_comp

  # Fit the model of interest to each imputed dataset
  if (post_imputation == "none") {
    if (use_glm) { ## Using a generalized linear model (GLM)
      fit_imp = with(imp_data,
                     glm(formula = as.formula(paste(outcome, "~", paste(c(ALI_comp_excl, covar), collapse = "+"))),
                         family = family))
      ## Pool the coefficients and variance estimates
      summ_fit_imp = summary(pool(fit_imp))
    } else { ## Using a random forest
      p = 10 ### all 10 components
      per_imp_imp = matrix(nrow = m, ncol = p)
      ## Loop over the imputed datasets
      for (b in 1:m) {
        ### Get complete data from bth imputation
        imp_dat_b = complete(data = imp_data, b)
        ### Convert imputed numeric components --> binary
        if (components == "numeric") {
          imp_dat_b = create_bin_components(data = imp_dat_b)
        }
        ### Fit random forest
        if (family == "binomial") {
          imp_fit_b = ranger(
            formula = as.formula(paste(outcome, "~", paste(c(ALI_comp_excl, covar), collapse = "+"))),
            data = imp_dat_b,
            num.trees = 500,
            mtry = 2,
            importance = "permutation",
            probability = TRUE # For classification, to get class probabilities
          )
        }
        ### Save its variable importance to the matrix
        per_imp_imp[b,] = imp_fit_b$variable.importance
      }
      ## Calculate pooled model estimates
      ### Coefficients
      imp_pooled = colMeans(per_imp_imp)
      ## Pool the coefficients and variance estimates
      summ_fit_imp = data.frame(term = names(imp_fit_b$variable.importance),
                                importance = imp_pooled)
    }
  } else {
    if (use_glm) { ## Using generalized linear model (GLM)
      ## Save coefficients from each post-imputation model
      p = get_p(post_imputation = post_imputation,
                covar = covar,
                ALI_comp_excl = ALI_comp_excl,
                ALI_comp = ALI_comp)
      per_imp_coeff = matrix(nrow = m, ncol = p)
      per_imp_vars = matrix(nrow = m, ncol = p)
      ## Loop over the imputed datasets
      for (b in 1:m) {
        ### Get complete data from bth imputation
        imp_dat_b = complete(data = imp_data, b)
        ### Convert imputed numeric components --> binary
        if (components == "numeric") {
          imp_dat_b = create_bin_components(data = imp_dat_b)
        }
        ### Post-imputation approach
        post_imp_dat_b = post_impute_data(post_imputation = post_imputation,
                                          outcome = outcome,
                                          covar = covar,
                                          data = imp_dat_b,
                                          family = family,
                                          components = components,
                                          use_glm = use_glm)
        ### Save its coefficients to the matrix
        per_imp_coeff[b,] = post_imp_dat_b$fit$coefficients
        ### And its standard errors
        per_imp_vars[b, ] = diag(vcov(post_imp_dat_b$fit))
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
      se_beta_pooled = sqrt(vt) #### standard errors
      ### Calculate degrees of freedom
      lambda = ((1 + 1 / m) * vb) / vt       # fraction missing information
      df_rubin = (m - 1) / (lambda ^ 2)         # Rubin df
      ## Pool the coefficients and variance estimates
      summ_fit_imp = data.frame(term = names(post_imp_dat_b$fit$coefficients),
                                estimate = beta_pooled,
                                std.error = se_beta_pooled,
                                statistic = beta_pooled / se_beta_pooled,
                                df = df_rubin,
                                p.value = pt(q = beta_pooled / se_beta_pooled,
                                             df = df_rubin))
    } else {
      ## Save coefficients from each post-imputation model
      p = get_p(post_imputation = post_imputation,
                covar = covar,
                ALI_comp_excl = ALI_comp_excl,
                ALI_comp = ALI_comp)
      per_imp_imp = matrix(nrow = m, ncol = p)
      ## Loop over the imputed datasets
      for (b in 1:m) {
        ### Get complete data from bth imputation
        imp_dat_b = complete(data = imp_data, b)
        ### Convert imputed numeric components --> binary
        if (components == "numeric") {
          imp_dat_b = create_bin_components(data = imp_dat_b)
        }
        ### Post-imputation approach
        post_imp_dat_b = post_impute_data(post_imputation = post_imputation,
                                          outcome = outcome,
                                          covar = covar,
                                          data = imp_dat_b,
                                          family = family,
                                          components = components,
                                          use_glm = use_glm)
        ### Save its variable importance to the matrix
        per_imp_imp[b,] = imp_fit_b$variable.importance
      }
      ## Calculate pooled model estimates
      ### Coefficients
      imp_pooled = colMeans(per_imp_coeff)
      ## Pool the coefficients and variance estimates
      summ_fit_imp = data.frame(term = names(importance(imp_fit_b)),
                                importance = imp_pooled)
    }
  }

  # Return list with the data and model
  return(list(data = imp_data,
              fit = summ_fit_imp,
              components = components,
              post_imputation = post_imputation,
              outcome = outcome,
              covar = covar,
              family = family,
              use_glm = use_glm))
}
