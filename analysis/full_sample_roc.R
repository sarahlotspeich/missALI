full_sample_roc = function(roc_obj, line_col, method_title, B = 10000) {
  ## Calculate 95% CI for AUC (bootstrapped)
  auc_ci = ci.auc(roc_obj,
                  method = "bootstrap",
                  boot.n = B) |>
    as.numeric()
  ## Create label AUC = 0.XX (95% CI)
  label_auc = paste0("= ", sprintf("%.2f", round(auc(roc_obj), 2)), " (", 
                     sprintf("%.2f", round(auc_ci[1], 2)), ", ", 
                     sprintf("%.2f", round(auc_ci[3], 2)), ")")
  
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
    ### Annotation of AUC (95% CI)
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