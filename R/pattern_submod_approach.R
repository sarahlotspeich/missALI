#' Pattern submodels approach to fitting regression models with missing ALI components
#'
#' @param outcome name of the outcome of the model (like \code{outcome = "disease"}).
#' @param covar optional, vector of names for covariates of the outcome model (like \code{covar = c("sex", "age")}). Default is \code{covar = NULL} (no additional covariates).
#' @param zeros optional, vector of names for covariates of the zero-inflation model (like \code{zeros = c("sex", "age")}). Default is \code{zeros = NULL} (no zero inflation). If zero inflation with only an intercept in the model is requested, use \code{zeros = "intercept"}.
#' @param ali vector of names for the columns containing the ALI components.
#' @param data dataframe containing at least the variables included in \code{outcome}, \code{covar}, and the binary ALI components.
#' @param family description of the error distribution and link function to be used in the model, to be passed to \code{glm()}.
#' @param use_glm logical argument for whether a generalized linear model (GLM) should be used (\code{use_glm = TRUE}, the default). Otherwise, a random forest is used.
#' @param rf_mtry optional (only if \code{use_glm = FALSE}), number of predictor variables randomly selected as candidates at each split in the random forest. Default is \code{rf_mtry = NULL}, which uses the \code{ranger()} default of the square root of the number of predictors (rounded down).
#' @param rf_min_node_size optional (only if \code{use_glm = FALSE}), minimum node size at which a node can be split in the random forest. Default is \code{rf_min_node_size = NULL}, which uses the \code{ranger()} default (10 for probability forests).
#' @param rf_splitrule optional (only if \code{use_glm = FALSE}), splitting rule used in the random forest. Default is \code{rf_splitrule = "gini"}; other options for probability forests include \code{"extratrees"} and \code{"hellinger"}.
#' @param rf_num_trees optional (only if \code{use_glm = FALSE}), number of trees to grow in the random forest. Default is \code{rf_num_trees = 500}.
#' @return
#' \item{data}{dataframe with the factor versions of the ALI components (with an added column identifying the missing data pattern).}
#' \item{fit_list}{list of fitted regression model objects from all pattern submodels.}
#' @export
#' @import dplyr
#' @importFrom ranger ranger
#' @importFrom pscl zeroinfl
pattern_submod_approach = function(outcome, covar = NULL, zeros = NULL, ali, data, family, use_glm = TRUE, rf_mtry = NULL, rf_min_node_size = NULL, rf_splitrule = "gini", rf_num_trees = 500) {
  # Create indicator of whether zero-inflation is needed
  use_zeroinfl = !is.null(zeros)

  ## If intercept only, overwrite zeros
  if ("intercept" %in% zeros & length(zeros) == 1) {
    zeros = c("1")
  }

  # Create missingness indicators for each component
  data = data |>
    mutate(
      across(
        all_of(ali),
        .fns = ~ if_else(is.na(.), 1, 0),
        .names = "MISS_{.col}"
        )
      )

  # Define missing data patterns
  all_miss_pat = data |>
    group_by(across(starts_with("MISS"))) |>
    summarize(n = n(), .groups = "drop_last") |>
    arrange(desc(n)) |>
    ungroup() |>
    mutate(miss_pat = paste0("MP-", 1:n()))

  ## Define number of predictors
  p = length(ali) + length(covar)
  
  ## Create indicator of being "big enough" for each pattern at 2p + 2 threshold
  all_miss_pat = all_miss_pat |>
    mutate(big_enough = n >= (2 * p + 2))
  
  ## Merge missing data pattern IDs back into patient data (to define subgroups)
  data = data |>
    left_join(y = all_miss_pat,
              by = grep(pattern = "MISS",
                        x = colnames(data),
                        ignore.case = FALSE,
                        value = TRUE)) |> 
    ### Initialize complete_case_submodel, nested = FALSE 
    mutate(complete_case_submodel = FALSE, 
           nested = FALSE)  

  # Fit the (sub)model(s) of interest
  submod_list = list()
  nonmiss_comp_list = list()
  for (m in 1:nrow(all_miss_pat)) {
    ## Subset to patients with that missing data pattern
    miss_pat_dat = data |>
      filter(miss_pat == all_miss_pat$miss_pat[m])
    ## Identify which patients were non-missing for them
    nonmiss_comp = sub(pattern = "MISS_",
                       replacement = "",
                       x = colnames(all_miss_pat[m, 1:10])[which(all_miss_pat[m, 1:10] == 0)])
    nonmiss_comp_list[[m]] = nonmiss_comp ### save in case we need to revisit for CCS-Reduced
    if (all_miss_pat$big_enough[m]) {
      if (use_glm) { ## Using a generalized linear model (GLM)
        if (use_zeroinfl) {
          ## Count how many unique levels per factor variable
          ### Any variables that are constant (only one level) are excluded
          count_levels = apply(X = miss_pat_dat[, nonmiss_comp],
                               MARGIN = 2,
                               FUN = function(x) length(unique(x)))
          warning(paste("The following variables were constant and excluded from the submodel:",
                        paste(nonmiss_comp[count_levels == 1], collapse = ", ")))
          submod_list[[m]] = zeroinfl(formula = as.formula(paste(outcome, "~", paste(c(nonmiss_comp[count_levels > 1], covar), collapse = "+"),  "|", paste(zeros, collapse = "+"))),
                                      dist = family,
                                      data = miss_pat_dat)
        } else {
          submod_list[[m]] = glm(formula = as.formula(paste(outcome, "~", paste(c(nonmiss_comp, covar), collapse = "+"))),
                                 family = family,
                                 data = miss_pat_dat)
        }
      } else { ## Using a random forest
        if (family == "binomial") {
          submod_list[[m]] = ranger(
            formula = as.formula(paste(outcome, "~", paste(c(nonmiss_comp, covar), collapse = "+"))),
            data = miss_pat_dat,
            num.trees = rf_num_trees,
            mtry = rf_mtry,
            min.node.size = rf_min_node_size,
            splitrule = rf_splitrule,
            probability = TRUE # For classification, to get class probabilities
          )
        } else {
          submod_list[[m]] = NULL
        }
      }
    } else {
      ## Subset to complete cases based on nonmiss_comp (Mercaldo and Blume's CCS)
      cc_nonmiss_comp = data[complete.cases(data[, nonmiss_comp]), c(outcome, covar, nonmiss_comp)]
      if (nrow(cc_nonmiss_comp) >= (2 * (length(nonmiss_comp) + length(covar)) + 2)) { ### check whether we have big enough sample for CCS
        if (use_glm) { ## Using a generalized linear model (GLM)
          if (use_zeroinfl) {
            submod_list[[m]] = zeroinfl(formula = as.formula(paste(outcome, "~", paste(c(nonmiss_comp, covar), collapse = "+"),  "|", paste(zeros, collapse = "+"))),
                                        dist = family,
                                        data = cc_nonmiss_comp)
          } else {
            submod_list[[m]] = glm(formula = as.formula(paste(outcome, "~", paste(c(nonmiss_comp, covar), collapse = "+"))),
                                   family = family,
                                   data = cc_nonmiss_comp)
          }
        } else { ## Using a random forest
          if (family == "binomial") {
            submod_list[[m]] = ranger(
              formula = as.formula(paste(outcome, "~", paste(c(nonmiss_comp, covar), collapse = "+"))),
              data = cc_nonmiss_comp,
              num.trees = rf_num_trees,
              mtry = rf_mtry,
              min.node.size = rf_min_node_size,
              splitrule = rf_splitrule,
              probability = TRUE # For classification, to get class probabilities
            )
          } else {
            submod_list[[m]] = NULL
          }
        }
        ## Flip complete_case_submodel to TRUE 
        data$complete_case_submodel[data$miss_pat == all_miss_pat$miss_pat[m]] = TRUE
        message(
          paste(
            "Missing pattern", all_miss_pat$miss_pat[m], 
            "(only", paste0(nonmiss_comp_list[[m]], collapse = ","), 
            "observed) was too small and fit via complete-case submodel"
          )
        )
      } else { ## If not, take fitted submodel from parent missing data pattern
        ## Look up which large, stable parent model index this tiny pattern belongs to
        ### Find the parent pattern that contains the same missing values 
        #### And drops the least other variables 
        parent_index = nest_miss_pat(all_miss_pat = all_miss_pat, 
                                     child_index = m, 
                                     miss_cols = grep(pattern = "MISS",
                                                      x = colnames(data),
                                                      ignore.case = FALSE,
                                                      value = TRUE))
        ## Take the parent pattern's fitted model "as-is"
        submod_list[[m]] = submod_list[[parent_index]]
        ## Flip nested to TRUE 
        data$nested[data$miss_pat == all_miss_pat$miss_pat[m]] = TRUE
        message(
          paste(
            "Missing pattern", all_miss_pat$miss_pat[m], 
            "(only", paste0(nonmiss_comp_list[[m]], collapse = ","), 
            "observed) was too small and nested within\n",
            "  -> Missing pattern", all_miss_pat$miss_pat[parent_index],
            "(only", paste0(nonmiss_comp_list[[parent_index]], collapse = ","), 
            "observed)."
            )
          )
      }
    }
  }
  # Name submodels by missing data patterns (for use to define subgroups in prediction)
  names(submod_list) = all_miss_pat$miss_pat

  # Return list with the data and model
  return(list(data = data,
              fit_list = submod_list))
}
