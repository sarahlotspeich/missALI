## Load packages
library(missALI) ## for models x missing data corrections 
library(pROC) ## for ROC curves and AUC 
library(patchwork) ## to combine plots

## Read in data on hospitalizations (suppressed for privacy)
#hosp_dat = read.csv("~/Documents/missALI/data/deidentified_ali_hospitalizations.csv")
hosp_dat = read.csv("~/Documents/Allostatic_load_audits/revision_analysis_dat.csv") |> 
  ## Exclude one person with no vitals/labs before hospitalization day so all components missing
  filter(!is.na(ALI))

## Define color scheme 
miss_meth_cols = c("#2b9d8f", "#e76f51", "#1b3c73", "#c1dbd0", 
                   "#a8c56e", "#6a4c93", "#e8b89a", "#c0404a")

# Define vector of binary ALI component column names
ali_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
             "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")

# Source helper functions for performance plots and metrics
source("~/Documents/missALI/analysis/full_sample_roc.R")
source("~/Documents/missALI/analysis/calibration_functions.R")
source("~/Documents/missALI/analysis/summ_plot_predictions.R")

## For full-sample 
summ_plot_fit = function(mod, col, method_title, model_type, missing, separate_components, B = 10000) {
  if (missing == "Pattern Submodels") {
    pred_prob = mod |>
      predict_pattern_submod() |>
      pull(PRED)
  } else {
    pred_prob = predict(
      mod$fit,
      type = "response"
    )
  }
  
  summ_plot_predictions(
    obs = hosp_dat$ANY_ADMIT,
    pred = pred_prob,
    col = col,
    method_title = method_title,
    model_type = model_type,
    missing = missing,
    separate_components = separate_components,
    fit_type = "Full-Sample",
    B = B,
    xlim = c(0, 0.5),
    ylim = c(0, 0.5),
    annotate = FALSE) ## don't report full-sample calibration slope/intercept for logistic regression
}
## For K-fold cross-validated
summ_plot_fit_kfold_pooled = function(kfold_res, col, method_title, model_type, missing, separate_components, B = 10000, xlim = c(0, 1), ylim = c(0, 1)) {
  ### Pool out-of-sample predictions across all folds
  cv_dat = do.call(
    rbind,
    lapply(
      kfold_res$all_fold_res,
      function(fold) {
        data.frame(
          obs = fold$test_roc$response,
          pred = fold$test_roc$predictor
        )
      }
    )
  )
  ### Calculate their metrics and make plots
  summ_plot_predictions(
    obs = cv_dat$obs,
    pred = cv_dat$pred,
    col = col,
    method_title = method_title,
    model_type = model_type,
    missing = missing,
    separate_components = separate_components,
    fit_type = "K-Fold",
    B = B,
    xlim = xlim,
    ylim = ylim,
    annotate = TRUE
  )
}
