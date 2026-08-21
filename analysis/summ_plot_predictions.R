library(ggplot2)
summ_plot_predictions <- function(obs, pred, col, method_title,
                                  model_type, missing, separate_components,
                                  fit_type, B = 10000,
                                  xlim = c(0, 1), ylim = c(0, 1),
                                  annotate = TRUE) {
  
  # ROC + AUC
  roc_curve <- roc(obs, pred)
  roc_plot <- full_sample_roc(
    roc_obj = roc_curve,
    line_col = col,
    method_title = method_title
  )
  auc_ci <- ci.auc(
    roc_curve,
    method = "bootstrap",
    boot.n = B
    ) |>
    as.numeric()
  
  # Youden's J threshold + diagnostic metrics
  diag <- coords(roc = roc_curve,
    x = "best",
    best.method = "youden",
    ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"),
    transpose = FALSE
    ) |>
    as.data.frame()
  diag_boot <- suppressMessages(
    replicate(B, {
      i <- sample(seq_along(pred), replace = TRUE)
      coords(roc = roc(obs[i], pred[i]),
             x = "best",
             best.method = "youden",
             ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"),
             transpose = FALSE
             ) |>
        as.data.frame() |>
        dplyr::slice(1) |>
        unlist()
      },
      simplify = FALSE)
    )
  diag_boot <- do.call(rbind, diag_boot)
  diag_ci <- apply(X = diag_boot, MARGIN = 2, 
                   FUN = quantile, probs = c(0.025, 0.975))
  diag$threshold_lb <- diag_ci[1, "threshold"]
  diag$threshold_ub <- diag_ci[2, "threshold"]
  diag$sensitivity_lb <- diag_ci[1, "sensitivity"]
  diag$sensitivity_ub <- diag_ci[2, "sensitivity"]
  diag$specificity_lb <- diag_ci[1, "specificity"]
  diag$specificity_ub <- diag_ci[2, "specificity"]
  diag$ppv_lb <- diag_ci[1, "ppv"]
  diag$ppv_ub <- diag_ci[2, "ppv"]
  diag$npv_lb <- diag_ci[1, "npv"]
  diag$npv_ub <- diag_ci[2, "npv"]
  # Calibration
  cal_dat <- data.frame(
    obs = obs,
    pred = pred
  )
  cal_res <- calibration_plot_custom(
    data = cal_dat,
    obs = "obs",
    pred = "pred",
    line_col = col,
    title = method_title,
    xlim = xlim,
    ylim = ylim,
    annotate = annotate
  )
  diag$cal_intercept <- cal_res$cal_coeff$calibration_intercept
  diag$cal_intercept_lb <- cal_res$cal_coeff$calibration_intercept_lb
  diag$cal_intercept_ub <- cal_res$cal_coeff$calibration_intercept_ub
  diag$cal_slope <- cal_res$cal_coeff$calibration_slope
  diag$cal_slope_lb <- cal_res$cal_coeff$calibration_slope_lb
  diag$cal_slope_ub <- cal_res$cal_coeff$calibration_slope_ub
  
  # Brier score
  diag$brier <- mean((pred - obs)^2)
  brier_boot <- replicate(B, {
    i <- sample(seq_along(pred), replace = TRUE)
    mean((pred[i] - obs[i])^2)
  })
  diag$brier_lb <- quantile(brier_boot, 0.025)
  diag$brier_ub <- quantile(brier_boot, 0.975)
  
  return(
    list(
      roc = roc_plot,
      cal = cal_res$plot,
      df = data.frame(
        Model = model_type,
        Missing = missing,
        Separate_Components = separate_components,
        Fit = fit_type,
        AUC = as.numeric(auc(roc_curve)),
        AUC_lb = auc_ci[1],
        AUC_ub = auc_ci[3],
        diag
      ), 
      cal_table = cal_res$cal_data
    )
  )
}

summ_plot_fit <- function(mod, col, method_title, model_type,
                          missing, separate_components, B = 10000) {
  if (missing == "Pattern Submodels") {
    pred_prob <- mod |>
      predict_pattern_submod() |>
      pull(PRED)
  } else {
    pred_prob <- predict(
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
    annotate = TRUE
  )
}