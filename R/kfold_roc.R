#' Build receiver operating characteristic (ROC) curves from K-fold cross validation of ALI prediction model with missing data correction
#'
#' @param kfold_validate_res list returned by \code{kfold_validat()} function.
#' @param plot_folds numeric indexes of folds to plot. Default is \code{plot_folds = 1} (the first only).
#' @param overlay_average logical, if \code{overlay_average = TRUE} (default) the average ROC curve across all folds is included in the plot.
#' @param line_col string, color for the smoother line if \code{overlay_average = TRUE}. The default is \code{line_col = "blue"}.
#' @param color_by_fold logical, if \code{color_by_fold = TRUE} the ROC curves for each fold will be identified by color. Default is \code{color_by_fold = FALSE}.
#' @return \code{ggplot2} object
#' @export
#' @importFrom pROC coords
#' @import ggplot2
kfold_roc = function(kfold_validate_res, plot_folds = 1, overlay_average = TRUE, line_col = "blue", color_by_fold = FALSE) {
  ## Extract all AUC from res list
  all_fold_auc = kfold_validate_res$all_fold_auc
  ### Subset to folds being plotted
  plot_folds_auc = all_fold_auc[plot_folds]
  ### If plotting more than 1 fold, calculate median AUC
  if (length(plot_folds) > 1) {
    label_auc = paste0("= ", round(median(plot_folds_auc), 3))
  } else {
    label_auc = paste0("AUC = ", round(plot_folds_auc, 3))
  }

  ## Make dataframe of ROC coordinates to plot
  plot_roc_df = data.frame() ### initialize empty dataframe
  for (k in plot_folds) {
    ### Add row with sensitivity, specificity at all thresholds for kth fold
    plot_roc_df = rbind(plot_roc_df,
                        data.frame(fold = k,
                                   coords(kfold_validate_res$all_fold_res[[k]]$test_roc)))
  }

  ## Plot ROC curve(s) using ggplot2
  ### Initialize ggplot object
  if (color_by_fold) {
    p = plot_roc_df |>
      ggplot(aes(x = (1 - specificity),
                 y = sensitivity)) +
      ### Add step for per-fold ROC curves
      geom_step(aes(color = fold),
                linewidth = 0.5,
                #### If overlaying average, make transparent
                alpha = ifelse(test = overlay_average,
                               yes = 0.5,
                               no = 1))
  } else {
    p = plot_roc_df |>
      ggplot(aes(x = (1 - specificity),
                 y = sensitivity)) +
      ### Add step for per-fold ROC curves
      geom_step(aes(group = fold),
                linewidth = 0.5,
                #### If overlaying average, make transparent
                alpha = ifelse(test = overlay_average,
                               yes = 0.5,
                               no = 1))
  }
  ### Add dashed line of equality for reference
  p = p +
    geom_abline(slope = 1,
                intercept = 0,
                linewidth = 0.5,
                linetype = "dashed")
  ### Overlay average (if requested)
  if (overlay_average) {
    p = p +
      geom_smooth(color = line_col)
  }
  ### Final formatting
  p = p +
    theme_minimal(base_size = 14) +
    coord_equal() +
    annotate(geom = "text",
             x = 1,
             y = 0.07,
             hjust = 1,
             vjust = 0,
             label = "bold('Median AUC')",
             parse = TRUE) +
    annotate(geom = "text",
             x = 1,
             y = 0,
             hjust = 1,
             vjust = 0,
             label = label_auc) +
    labs(x = "1 - Specificity",
         y = "Sensitivity") +
    theme(axis.title = element_text(face = "bold"))

  ### Return plot
  return(p)
}
