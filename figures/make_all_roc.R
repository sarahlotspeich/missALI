# Load packages
library(missALI) ## for models x missing data corrections 
library(pROC) ## for ROC curves and AUC 
library(ggplot2) ## for plots (in general)
library(patchwork) ## for multi-panel plots 
library(naniar) ## for upset plots of missing data patterns

# Read in data on hospitalizations (suppressed for privacy)
hosp_dat = read.csv("~/Documents/missALI_prediction/data/deidentified_ali_hospitalizations.csv")

# Helper function to make ggplot ROC curves from full-sample fits 
full_sample_roc = function(roc_obj, line_col, method_title) {
  ## Create label AUC = 0.XXX
  label_auc = paste0("= ", round(auc(roc_obj), 3), " (", 
                     round(ci.auc(roc_obj)[1], 3), ", ", 
                     round(ci.auc(roc_obj)[2], 3), ")")
  
  ## Make dataframe of ROC coordinates to plot
  #plot_roc_df = coords(roc_obj)
  
  ## Plot ROC curve(s) using ggplot2
  roc_obj |>
    ### Plot ROC coordinates
    ggroc(legacy.axes = TRUE, color = line_col) + 
    ### Add dashed line of equality for reference
    geom_abline(slope = 1,
                intercept = 0,
                linewidth = 0.5,
                linetype = "dashed") + 
    ### Final formatting
    theme_minimal(base_size = 14) +
    coord_equal() +
    annotate(geom = "text",
             x = 1,
             y = 0.07,
             hjust = 1,
             vjust = 0,
             label = "bold('AUC (95% CI)')", 
             parse = TRUE) +
    annotate(geom = "text",
             x = 1,
             y = 0,
             hjust = 1,
             vjust = 0,
             label = label_auc) +
    labs(x = "1 - Specificity",
         y = "Sensitivity", 
         title = method_title) + 
    theme(axis.title = element_text(face = "bold"), 
          title = element_text(face = "bold"))
}

# Define color scheme 
miss_meth_cols = c("#2b9d8f", "#e76f51", "#1b3c73", "#c1dbd0", 
                   "#a8c56e", "#6a4c93", "#e8b89a", "#c0404a")

# Function to plot all ROC curves for summary measures -------------------------
all_summ_meas_roc = function(zeros, family, use_glm, file_name) {
  ## Approach 1: Proportion of Non-Missing Components That Are Unhealthy -------
  ### Fit to Full Sample 
  mod_prop = cc_prop_approach(outcome = "ANY_ADMIT", 
                              covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                              zeros = zeros, 
                              data = hosp_dat, 
                              family = family, 
                              use_glm = use_glm) 
  # Calculate predicted probabilities
  if (use_glm) {
    pred_prob_prop = mod_prop$fit |> 
      predict(type = "response")
  } else {
    pred_prob_prop = mod_prop$fit$predictions[, 1]
  }
  # Make the ROC curve 
  ## Include AUC (95% Confidence Interval)
  roc_curve = roc(hosp_dat$ANY_ADMIT, pred_prob_prop)
  roc_prop = full_sample_roc(roc_obj = roc_curve, 
                             line_col = miss_meth_cols[1], 
                             method_title = "Complete-Case\nProportion")
  ##### 5-Fold Cross-Validated 
  # Make fold assignment reproducible (same for all models + approaches)
  set.seed(918)
  # 5-fold cross validation of the logistic regression model with complete-case proportion ALI
  kfold_cc_prop = kfold_validate(outcome = "ANY_ADMIT", 
                                 covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                 zeros = zeros,
                                 data = hosp_dat, 
                                 family = family,
                                 use_glm = use_glm,
                                 miss_method = "cc_prop", 
                                 folds = 5)
  ## Plot individual ROC curves from 5 folds with median over top 
  roc_prop_k = kfold_cc_prop |> 
    kfold_roc(plot_folds = 1:5, 
              overlay_average = TRUE,
              line_col = miss_meth_cols[1]) 
  
  ## Approach 2: Counts of Unhealthy and Missing Components --------------------
  ### Fit to Full Sample 
  mod_num = num_miss_approach(outcome = "ANY_ADMIT", 
                              covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                              zeros = zeros,
                              data = hosp_dat, 
                              family = family, 
                              use_glm = use_glm) 
  # Calculate predicted probabilities
  if (use_glm) {
    pred_prob_num = mod_num$fit |> 
      predict(type = "response")
  } else {
    pred_prob_num = mod_num$fit$predictions[, 1]
  }
  # Make the ROC curve 
  ## Include AUC (95% Confidence Interval)
  roc_curve = roc(hosp_dat$ANY_ADMIT, pred_prob_num)
  roc_num = full_sample_roc(roc_obj = roc_curve, 
                            line_col = miss_meth_cols[2], 
                            method_title = "Counts of Unhealthy and\nMissing Components")
  ##### 5-Fold Cross-Validated 
  # Make fold assignment reproducible (same for all models + approaches)
  set.seed(918)
  # 5-fold cross validation of the logistic regression model with complete-case proportion ALI
  kfold_num_miss = kfold_validate(outcome = "ANY_ADMIT", 
                                      covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                      zeros = zeros,
                                      data = hosp_dat, 
                                      family = family,
                                      use_glm = use_glm,
                                      miss_method = "num_miss", 
                                      folds = 5)
  ## Plot individual ROC curves from 5 folds with median over top 
  roc_num_k = kfold_num_miss |> 
    kfold_roc(plot_folds = 1:5, 
              overlay_average = TRUE, 
              line_col = miss_meth_cols[2]) 
  
  ## Approach 3a: Best-Case Imputation -----------------------------------------
  ### Fit to Full Sample 
  mod_best = case_approach(outcome = "ANY_ADMIT", 
                           covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                           zeros = zeros,
                           data = hosp_dat, 
                           family = family,
                           use_glm = use_glm,
                           best = TRUE) 
  # Calculate predicted probabilities
  if (use_glm) {
    pred_prob_best = mod_best$fit |> 
      predict(type = "response")
  } else {
    pred_prob_best = mod_best$fit$predictions[, 1]
  }
  # Make the ROC curve 
  ## Include AUC (95% Confidence Interval)
  roc_curve = roc(hosp_dat$ANY_ADMIT, pred_prob_best)
  roc_best = full_sample_roc(roc_obj = roc_curve, 
                             line_col = miss_meth_cols[3], 
                             method_title = "Best Case\nImputation")
  
  ##### 5-Fold Cross-Validated 
  # Make fold assignment reproducible (same for all models + approaches)
  set.seed(918)
  # 5-fold cross validation of the logistic regression model with best-case imputation
  kfold_best = kfold_validate(outcome = "ANY_ADMIT", 
                              covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                              zeros = zeros,
                              data = hosp_dat, 
                              family = family,
                              use_glm = use_glm,
                              miss_method = "best", 
                              folds = 5)
  ## Plot individual ROC curves from 5 folds with median over top 
  roc_best_k = kfold_best |> 
    kfold_roc(plot_folds = 1:5, 
              overlay_average = TRUE, 
              line_col = miss_meth_cols[3]) 
  
  ## Approach 3b: Worst-Case Imputation ----------------------------------------
  ### Fit to Full Sample 
  mod_worst = case_approach(outcome = "ANY_ADMIT", 
                            covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                            zeros = zeros,
                            data = hosp_dat, 
                            family = family,
                            use_glm = use_glm,
                            best = FALSE) 
  # Calculate predicted probabilities
  if (use_glm) {
    pred_prob_worst = mod_worst$fit |> 
      predict(type = "response")
  } else {
    pred_prob_worst = mod_worst$fit$predictions[, 1]
  }
  # Make the ROC curve 
  ## Include AUC (95% Confidence Interval)
  roc_curve = roc(hosp_dat$ANY_ADMIT, pred_prob_worst)
  roc_worst = full_sample_roc(roc_obj = roc_curve, 
                              line_col = miss_meth_cols[4], 
                              method_title = "Worst Case\nImputation")
  ### 5-Fold Cross-Validated 
  # Make fold assignment reproducible (same for all models + approaches)
  set.seed(918)
  # 5-fold cross validation of the logistic regression model with worst-case imputation
  kfold_worst = kfold_validate(outcome = "ANY_ADMIT", 
                               covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                               zeros = zeros,
                               data = hosp_dat, 
                               family = family,
                               use_glm = use_glm,
                               miss_method = "worst", 
                               folds = 5)
  ## Plot individual ROC curves from 5 folds with median over top 
  roc_worst_k = kfold_worst |> 
    kfold_roc(plot_folds = 1:5, 
              overlay_average = TRUE, 
              line_col = miss_meth_cols[4]) 
  
  # Create combined plot 
  comb_plot = (roc_prop | roc_num | roc_best | roc_worst) / 
    (roc_prop_k |  roc_num_k | roc_best_k | roc_worst_k) 
  comb_plot
  ggsave(filename = paste0("~/Documents/missALI_prediction/figures/", file_name), 
         plot = comb_plot, 
         device = "png", width = 14, height = 8, units = "in")
}

## Make all ROC curves for RF w/ summary measures 
all_summ_meas_roc(zeros = NULL, 
                  family = "binomial",
                  use_glm = FALSE, 
                  file_name = "all_summ_meas_roc_rf.png")

## Make all ROC curves for Poisson w/ summary measures 
all_summ_meas_roc(zeros = NULL, 
                  family = "poisson",
                  use_glm = TRUE, 
                  file_name = "all_summ_meas_roc_pois.png")

## Make all ROC curves for Poisson w/ summary measures 
all_summ_meas_roc(zeros = c("SEX", "AGE_AT_ENCOUNTER"), 
                  family = "poisson",
                  use_glm = TRUE, 
                  file_name = "all_summ_meas_roc_zip.png")

# Function to plot all ROC curves for separate components ----------------------
all_sep_comp_roc = function(zeros, family, use_glm, file_name) {
  #### Approach 4a: Best-Case Case Imputation (Separate)
  ##### Fit to Full Sample 
  # Logistic regression + best-case imputation (separate components)
  mod_best = case_approach(outcome = "ANY_ADMIT", 
                           covar = c("SEX", "AGE_AT_ENCOUNTER"),  
                           zeros = zeros,
                           data = hosp_dat, 
                           family = family, 
                           use_glm = use_glm, 
                           best = TRUE, 
                           comp_sep = TRUE) 
  # Calculate predicted probabilities
  if (use_glm) {
    pred_prob_best = mod_best$fit |> 
      predict(type = "response")
  } else {
    pred_prob_best = mod_best$fit$predictions[, 1]
  }
  
  # Make the ROC curve 
  ## Include AUC (95% Confidence Interval)
  roc_curve = roc(hosp_dat$ANY_ADMIT, pred_prob_best)
  roc_best = full_sample_roc(roc_obj = roc_curve, 
                             line_col = miss_meth_cols[5], 
                             method_title = "Best Case\nImputation")
  
  ##### 5-Fold Cross-Validated 
  # Make fold assignment reproducible (same for all models + approaches)
  set.seed(918)
  
  # 5-fold cross validation of the logistic regression model with best-case imputation
  kfold_best_sep = kfold_validate(outcome = "ANY_ADMIT", 
                                  covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                  zeros = zeros,
                                  data = hosp_dat, 
                                  family = family, 
                                  use_glm = use_glm, 
                                  miss_method = "best", 
                                  comp_sep = TRUE,
                                  folds = 5)
  
  ## Plot individual ROC curves from 5 folds with median over top 
  roc_best_k = kfold_best_sep |> 
    kfold_roc(plot_folds = 1:5, 
              overlay_average = TRUE, 
              line_col = miss_meth_cols[5]) 

  #### Approach 4b: Worst-Case Case Imputation (Separate)
  ##### Fit to Full Sample 
  mod_worst = case_approach(outcome = "ANY_ADMIT", 
                            covar = c("SEX", "AGE_AT_ENCOUNTER"),  
                            zeros = zeros,
                            data = hosp_dat, 
                            family = family, 
                            use_glm = use_glm, 
                            best = FALSE, 
                            comp_sep = TRUE) 
  
  # Calculate predicted probabilities
  if (use_glm) {
    pred_prob_worst = mod_worst$fit |> 
      predict(type = "response")
  } else {
    pred_prob_worst = mod_worst$fit$predictions[, 1]
  }
  
  # Make the ROC curve 
  ## Include AUC (95% Confidence Interval)
  roc_curve = roc(hosp_dat$ANY_ADMIT, pred_prob_worst)
  roc_worst = full_sample_roc(roc_obj = roc_curve, 
                              line_col = miss_meth_cols[6], 
                              method_title = "Worst Case\nImputation")
  
  ##### 5-Fold Cross-Validated 
  # Make fold assignment reproducible (same for all models + approaches)
  set.seed(918)
  
  # 5-fold cross validation of the logistic regression model with best-case imputation
  kfold_worst_sep = kfold_validate(outcome = "ANY_ADMIT", 
                                   covar = c("SEX", "AGE_AT_ENCOUNTER"),  
                                   zeros = zeros,
                                   data = hosp_dat, 
                                   family = family, 
                                   use_glm = use_glm, 
                                   miss_method = "worst", 
                                   comp_sep = TRUE,
                                   folds = 5)
  
  ## Plot individual ROC curves from 5 folds with median over top 
  roc_worst_k = kfold_worst_sep |> 
    kfold_roc(plot_folds = 1:5, 
              overlay_average = TRUE, 
              line_col = miss_meth_cols[6]) 

  #### Approach 5: Missingness as a Category (Separate)
  ##### Fit to Full Sample
  # Logistic regression + missing as a category (separate components)
  mod_cat = miss_cat_approach(outcome = "ANY_ADMIT",
                              covar = c("SEX", "AGE_AT_ENCOUNTER"),
                              zeros = zeros,
                              data = hosp_dat,
                              family = family, 
                              use_glm = use_glm)
  # Calculate predicted probabilities
  if (use_glm) {
    pred_prob_cat = mod_cat$fit |> 
      predict(type = "response")
  } else {
    pred_prob_cat = mod_cat$fit$predictions[, 1]
  }
  # Make the ROC curve 
  ## Include AUC (95% Confidence Interval)
  roc_curve = roc(hosp_dat$ANY_ADMIT, pred_prob_cat)
  roc_cat = full_sample_roc(roc_obj = roc_curve, 
                            line_col = miss_meth_cols[7], 
                            method_title = "Missingness as\na Category")
 
  # Make fold assignment reproducible (same for all models + approaches)
  # set.seed(918)
  # 
  # # 5-fold cross validation of the logistic regression model with best-case imputation
  # kfold_cat_sep = kfold_validate(outcome = "ANY_ADMIT", 
  #                                covar = c("SEX", "AGE_AT_ENCOUNTER"),  
  #                                zeros = zeros,
  #                                data = hosp_dat, 
  #                                family = family, 
  #                                use_glm = use_glm, 
  #                                miss_method = "cat", 
  #                                comp_sep = TRUE,
  #                                folds = 5)
  # 
  # ## Plot individual ROC curves from 5 folds with median over top 
  # roc_worst_k = kfold_worst_sep |> 
  #   kfold_roc(plot_folds = 1:5, 
  #             overlay_average = TRUE, 
  #             line_col = miss_meth_cols[6]) 
  
  #### Approach 6: Pattern Submodels (Separate)
  ##### Fit to Full Sample
  mod_patsub = pattern_submod_approach(outcome = "ANY_ADMIT",
                                       covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                       zeros = zeros,
                                       data = hosp_dat,
                                       family = family, 
                                       use_glm = use_glm)
  
  # Calculate predicted probabilities
  pred_prob_patsub = mod_patsub |> 
    predict_pattern_submod()
  
  # Make the ROC curve 
  ## Include AUC (95% Confidence Interval)
  roc_curve = roc(hosp_dat$ANY_ADMIT, pred_prob_patsub)
  roc_submod = full_sample_roc(roc_obj = roc_curve, 
                               line_col = miss_meth_cols[8], 
                               method_title = "Pattern\nSubmodels")
  
  ##### 5-Fold Cross-Validated 
  # Make fold assignment reproducible (same for all models + approaches)
  set.seed(918)
  
  # 5-fold cross validation of the logistic regression model with best-case imputation
  kfold_submod_sep = kfold_validate(outcome = "ANY_ADMIT", 
                                    covar = c("SEX", "AGE_AT_ENCOUNTER"),  
                                    zeros = zeros,
                                    data = hosp_dat, 
                                    family = family, 
                                    use_glm = use_glm, 
                                    miss_method = "patsub", 
                                    comp_sep = TRUE,
                                    folds = 5)
  
  ## Plot individual ROC curves from 5 folds with median over top 
  roc_submod_k = kfold_submod_sep |> 
    kfold_roc(plot_folds = 1:5, 
              overlay_average = TRUE, 
              line_col = miss_meth_cols[8]) 
  
  design <- "
  ABCD
  EF#G
  "
  comb_plot <- (roc_best + roc_worst + roc_cat + roc_submod +
                  roc_best_k + roc_worst_k + roc_submod_k) +
    plot_layout(design = design)
  ggsave(filename = paste0("~/Documents/missALI_prediction/figures/", file_name), 
         plot = comb_plot, 
         device = "png", width = 14, height = 8, units = "in")
}

## Make all ROC curves for RF w/ separate components
all_sep_comp_roc(zeros = NULL, 
                  family = "binomial",
                  use_glm = FALSE, 
                  file_name = "all_sep_comp_roc_rf.png")

## Make all ROC curves for Poisson w/ separate components
all_sep_comp_roc(zeros = NULL, 
                  family = "poisson",
                  use_glm = TRUE, 
                  file_name = "all_sep_comp_roc_pois.png")

## Make all ROC curves for Poisson w/ separate components
all_sep_comp_roc(zeros = c("SEX", "AGE_AT_ENCOUNTER"), 
                  family = "poisson",
                  use_glm = TRUE, 
                  file_name = "all_sep_comp_roc_zip.png")