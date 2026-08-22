# Column name stems for metrics 
metric_stems = c("AUC", "sensitivity", "specificity", "ppv", "npv",
                 "cal_intercept", "cal_slope", "brier", "threshold")

# Load metrics data 
metrics = read.csv("~/Documents/missALI/data/logistic_regression_metrics.csv") |> 
  dplyr::mutate(dplyr::across(dplyr::all_of(metric_stems), 
                ~ sprintf("$%.2f$ $(%.2f, %.2f)$",#"%.2f (%.2f, %.2f)", 
                          .x, 
                          get(paste0(cur_column(), "_lb")), 
                          get(paste0(cur_column(), "_ub"))),
                .names = "{.col}_combined")) |> 
  dplyr::select(Model, Missing, Separate_Components, Fit, dplyr::ends_with("combined")) |> 
  tidyr::pivot_longer(cols = dplyr::ends_with("combined"), 
                      names_to = "metric", 
                      values_to = "value") |> 
  dplyr::mutate(
    value = dplyr::if_else(condition = Fit == "Full-Sample" & 
                             grepl(pattern = "cal", x = metric), 
                           true = NA, 
                           false = value)
  ) |>
  tidyr::pivot_wider(
    id_cols = c(Model, Missing, Separate_Components, metric),
    names_from = Fit,
    values_from = value,
    names_glue = "{Fit}"
  )

# Make LaTex table 
library(kableExtra)
metrics |> 
  mutate(
    Missing = dplyr::if_else(condition = Missing == "Best Case Imputation" & 
                               Separate_Components == "Summary Measure", 
                             true = "Best Case Imputation (Summary)", 
                             false = Missing),
    Missing = dplyr::if_else(condition = Missing == "Worst Case Imputation" & 
                               Separate_Components == "Summary Measure", 
                             true = "Worst Case Imputation (Summary)", 
                             false = Missing),
    Missing = dplyr::if_else(condition = Missing == "Best Case Imputation" & 
                               Separate_Components == "Separate Components", 
                             true = "Best Case Imputation (Separate)", 
                             false = Missing),
    Missing = dplyr::if_else(condition = Missing == "Worst Case Imputation" & 
                               Separate_Components == "Separate Components", 
                             true = "Worst Case Imputation (Separate)", 
                             false = Missing),
    Missing = factor(x = Missing, 
                     levels = c("Complete-Case Proportion", 
                                "Counts of Unhealthy and Missing Components", 
                                "Best Case Imputation (Summary)", 
                                "Worst Case Imputation (Summary)",
                                "Best Case Imputation (Separate)", 
                                "Worst Case Imputation (Separate)",
                                "Missingness as a Category",
                                "Pattern Submodels")), 
    metric = factor(x = metric, 
                    levels = c("AUC_combined", "brier_combined", 
                               "cal_intercept_combined", "cal_slope_combined", 
                               "threshold_combined", "sensitivity_combined", 
                               "specificity_combined", "ppv_combined", 
                               "npv_combined"), 
                    labels = c("AUC", "Brier Score", "Cal. Intercept", 
                               "Cal. Slope", "Threshold", "Sensitivity", 
                               "Specifity", "PPV", "NPV"))) |> 
  dplyr::arrange(metric, Missing) |> 
  dplyr::select(metric, Missing, dplyr::everything(), -Model, -Separate_Components) |> 
  kable(format = "latex", escape = FALSE, booktabs = TRUE)
