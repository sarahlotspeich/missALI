calibration_intercept_slope = function(y, p) {
  # Calculate log-odds based on predicted probabilities
  logit_p = log(p / (1 - p))
  
  # Calibration slope
  slope_model = glm(formula = y ~ logit_p,
                    family = binomial)
  
  # Calibration-in-the-large
  intercept_model = glm(formula = y ~ offset(logit_p), 
                        family = binomial)
  
  # Extract estimates and SEs
  intercept = coef(intercept_model)[1]
  intercept_se = sqrt(diag(vcov(intercept_model)))[1]
  slope = coef(slope_model)[2]
  slope_se = sqrt(diag(vcov(slope_model)))[2]
  
  # Return dataframe 
  data.frame(
    calibration_intercept = intercept,
    calibration_intercept_lb = intercept - 1.96 * intercept_se,
    calibration_intercept_ub = intercept + 1.96 * intercept_se,
    calibration_slope = slope,
    calibration_slope_lb = slope - 1.96 * slope_se,
    calibration_slope_ub = slope + 1.96 * slope_se
  )
}

calibration_plot_custom = function(data, obs, pred, line_col, point_col = line_col, n_groups = 10, title = NULL, xlim = c(0, 1), ylim = c(0, 1), annotate = FALSE) {
  ## Calculate calibration intercept and slope
  cal = calibration_intercept_slope(y = data[, obs],
                                    p = data[, pred])
  
  # Calculate avg predicted / observed event probabilities (and 95% CIs)
  data = data |>
    mutate(decile = ntile(.data[[pred]], n_groups)) |>
    group_by(decile) |>
    summarise(
      decile_size = n(), ## number of observations 
      phat  = mean(.data[[obs]]), ## observed event rate
      avg_pihat = mean(.data[[pred]]) ## avg predicted event prob
    ) |>
    mutate(
      phat_se = sqrt(phat * (1 - phat) / decile_size), ## observed event rate SE
      phat_ub = phat + 1.96 * phat_se,
      phat_lb = phat - 1.96 * phat_se
    )
  
  # Make the calibration plot 
  p = ggplot(data, aes(x = avg_pihat, y = phat)) +
    ## Reference line of equality (perfect calibration)
    geom_abline(slope = 1, intercept = 0,
                linewidth = 0.5, linetype = "dashed") +
    ## Observed event rates and 95% CIs as points/error bars
    geom_errorbar(aes(ymin = phat_lb, ymax = phat_ub),
                  color = point_col, width = 0.01) +
    geom_point(color = point_col, size = 2) +
    ### Final formatting - matches ROC plot styling
    theme_minimal(base_size = 14) +
    coord_equal(xlim = xlim, ylim = ylim) +
    labs(x = "Predicted Hospitalization Probability",
         y = "Observed Hospitalization Proportion", 
         title = title) +
    theme(axis.title = element_text(face = "bold"),
          title = element_text(face = "bold"))
  if (annotate) {
    ## Separate calibration slope/intercept for labels 
    cal = round(cal, 2) ### round to 2 digits
    label_intercept = paste0("Intercept = ",
                              sprintf("%.2f", cal$calibration_intercept),
                              " (", sprintf("%.2f",cal$calibration_intercept_lb), ", ",
                              sprintf("%.2f",cal$calibration_intercept_ub), ")")
    label_slope = paste0("Slope = ",
                          sprintf("%.2f",cal$calibration_slope),
                          " (", sprintf("%.2f",cal$calibration_slope_lb), ", ",
                          sprintf("%.2f",cal$calibration_slope_ub), ")")
    # Put annotation in upper-left corner
    x_pos = xlim[1]
    y_title = ylim[2] 
    y_int = ylim[2] - 0.06 * diff(ylim)
    y_slope = ylim[2] - 0.12 * diff(ylim)
    p = p +
      annotate(geom = "text",
               x = x_pos,
               y = y_title,
               hjust = 0,
               vjust = 1,
               label = "bold('Calibration (95% CI)')",
               parse = TRUE) +
      annotate(geom = "text",
               x = x_pos,
               y = y_int,
               hjust = 0,
               vjust = 1,
               label = label_intercept) +
      annotate(geom = "text",
               x = x_pos,
               y = y_slope,
               hjust = 0,
               vjust = 1,
               label = label_slope)
  } 
  
  # Return calibration plot and slope/intercept as a list
  return(
    list(plot = p, 
         cal_coeff = cal, 
         cal_data = data)
  )
}