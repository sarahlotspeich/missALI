#' Build calibration curves from K-fold cross validation of ALI prediction model with missing data correction
#'
#' @param kfold_validate_res list returned by \code{kfold_validat()} function.
#' @param plot_folds numeric indexes of folds to plot. Default is \code{plot_folds = 1} (the first only).
#' @param overlay_average logical, if \code{overlay_average = TRUE} (default) the average ROC curve across all folds is included in the plot.
#' @param line_col string, color for the smoother line if \code{overlay_average = TRUE}. The default is \code{line_col = "blue"}.
#' @param n_groups integer, number of equal-sized groups used to bin predicted probabilities within each fold. Default is \code{10} (deciles).
#' @param xlim vector, numeric vector of length 2 giving the x-axis limits for the plot. Default is \code{c(0, 1)}.
#' @param ylim vector, numeric vector of length 2 giving the y-axis limits for the plot. Default is \code{c(0, 1)}.
#' @return \code{ggplot2} object
#' @export
#' @importFrom dplyr mutate group_by summarize ntile
#' @import ggplot2
kfold_calibration = function(kfold_validate_res, plot_folds = 1, 
                             overlay_average = TRUE, line_col, n_groups = 10, 
                             xlim = c(0, 1), ylim = c(0, 1)) {
  
  ## Extract response (y) and predictor (p) from each fold's test ROC object
  fold_data = lapply(
    X = kfold_validate_res$all_fold_res, 
    FUN = function(x) {
      data.frame(y = x$test_roc$response, 
                 p = x$test_roc$predictor)
      }
    )
  names(fold_data) = paste0("Fold ", seq_along(fold_data))
  
  ## Subset to folds being plotted
  plot_data = fold_data[plot_folds]
  
  ## Calculate calibration intercept AND slope per fold
  cal_per_fold = lapply(plot_data, function(d) {
    calibration_intercept_slope(d$y, d$p)
  })
  ## 
  intercept_per_fold = sapply(cal_per_fold, function(x) x$calibration_intercept)
  slope_per_fold     = sapply(cal_per_fold, function(x) x$calibration_slope)
  ## Take median calibration slope/intercept if plotting >1 fold
  if (length(plot_folds) > 1) {
    label_cal = paste0("Intercept = ", round(median(intercept_per_fold), 3),
                       "\nSlope = ", round(median(slope_per_fold), 3))
  } else {
    label_cal = paste0("Intercept = ", round(intercept_per_fold, 3),
                       "\nSlope = ", round(slope_per_fold, 3))
  }
  
  ## Build decile-level points/lines per fold (analogous to each fold's ROC curve)
  decile_data = do.call(
    what = rbind, 
    args = lapply(names(plot_data), function(fold_name) {
      plot_data[[fold_name]] |>
        mutate(decile = ntile(p, n_groups)) |>
        group_by(decile) |>
        summarize(predRate = mean(p), obsRate = mean(y), .groups = "drop") |>
        mutate(Fold = fold_name)
  }))
  
  ## Pooled raw (patient-level) data across requested folds - used for the
  ## overlay average smoother, same role as aes(group = NULL) in kfold_roc
  pooled_data = do.call(rbind, plot_data)
  
  ## Plot calibration curve(s) using ggplot2
  ### Initialize ggplot object with per-fold decile curves
  p = ggplot() +
    #### Add dashed line of equality for reference
    geom_abline(slope = 1,
                intercept = 0,
                linewidth = 0.5,
                linetype = "dashed") +
    geom_line(data = decile_data,
              aes(x = predRate, y = obsRate, group = Fold),
              color = "black",
              alpha = ifelse(test = overlay_average, yes = 0.5, no = 1)) +
    geom_point(data = decile_data,
               aes(x = predRate, y = obsRate, group = Fold),
               color = "black",
               alpha = ifelse(test = overlay_average, yes = 0.5, no = 1),
               size = 1.5)
  
  ### Overlay average (if requested) - pooled loess across all requested folds
  if (overlay_average) {
    p = p +
      geom_smooth(data = pooled_data,
                  aes(x = p, y = y, group = NULL),
                  method = "loess", se = FALSE,
                  color = line_col)
  }
  
  ### Final formatting
  p = p +
    theme_minimal(base_size = 14) +
    coord_equal(xlim = xlim, ylim = ylim) +
    annotate(geom = "text",
             x = xlim[2],
             y = ylim[1] + 0.07 * diff(ylim),
             hjust = 1,
             vjust = 0,
             label = "bold('Median Calibration Slope')",
             parse = TRUE) +
    annotate(geom = "text",
             x = xlim[2],
             y = ylim[1],
             hjust = 1,
             vjust = 0,
             label = label_cal) +
    labs(x = "Predicted probability",
         y = "Observed proportion") +
    theme(axis.title = element_text(face = "bold"))
  
  ### Return plot
  return(p)
}