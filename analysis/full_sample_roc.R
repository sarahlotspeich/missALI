full_sample_roc = function(roc_obj, line_col, method_title) {
  ## Create label AUC = 0.XXX
  label_auc = paste0("= ", sprintf("%.2f", round(auc(roc_obj), 2)), " (", 
                     sprintf("%.2f", round(ci.auc(roc_obj)[1], 2)), ", ", 
                     sprintf("%.2f", round(ci.auc(roc_obj)[2], 2)), ")")
  
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