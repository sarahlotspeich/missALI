# Read in metrics from model fits
res = read.csv("~/Documents/missALI/data/logistic_regression_metrics.csv") |> 
  dplyr::bind_rows(read.csv("~/Documents/missALI/data/random_forest_metrics.csv"))
res |> 
  write.csv("~/Documents/missALI/data/both_metrics.csv", row.names = FALSE)
barbell_data = res |>
  dplyr::group_by(Missing, Fit, Separate_Components) |> 
  dplyr::summarize(barbell_lb = min(AUC), 
                   barbell_ub = max(AUC)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    Separate_Components = factor(x = Separate_Components,
                                 levels = c("Summary Measure", 
                                            "Separate Components"))
  ) 

plot_data <- res |>
  dplyr::select(
    Model, Missing, Separate_Components, Fit,
    AUC, sensitivity, specificity, ppv, npv,
    cal_intercept, cal_slope, brier
  ) |>
  tidyr::pivot_longer(
    cols = c(
      AUC, sensitivity, specificity, ppv, npv,
      cal_intercept, cal_slope, brier
    ),
    names_to = "Metric",
    values_to = "Value"
  ) |>
  dplyr::mutate(
    Fit = factor(
      Fit,
      levels = c("Full-Sample", "K-Fold")
    ),
    Separate_Components = factor(
      Separate_Components,
      levels = c("Summary Measure", "Separate Components")
    ),
    Metric = factor(
      Metric,
      levels = c(
        "AUC", "brier",
        "cal_intercept", "cal_slope",
        "sensitivity", "specificity",
        "ppv", "npv"
      ),
      labels = c(
        "Area Under the ROC Curve (AUC)", "Brier Score",
        "Calibration Intercept", "Calibration Slope",
        "Sensitivity", "Specificity",
        "Positive Predictive Value (PPV)", "Negative Predictive Value (NPV)"
      )
    )
  ) |> 
  dplyr::mutate(
    Value = dplyr::if_else(condition = Fit == "Full-Sample" & 
                             Metric %in% c("Calibration Intercept", "Calibration Slope"), 
                           true = NA, false = Value)
  )
metric_plot <- plot_data |>
  dplyr::filter(Model == "Logistic") |> 
  ggplot(
    aes(
      x = Value,
      y = Fit,
      color = Missing,
      shape = Separate_Components
    )
  ) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(~Metric, 
             nrow = 4, ncol = 2, 
             scales = "free") + 
  labs(
    x = "Metric Value",
    y = "Fit",
    color = "Missing Data Method",
    shape = NULL
  ) +
  scale_color_manual(
    values = miss_meth_cols,
    labels = scales::label_wrap(20)
  ) + 
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(
      face = "bold",
      color = "white"
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  scale_shape_discrete(name = "Treatment of ALI\nComponents")
metric_plot
ggsave(
  filename = "~/Documents/missALI/figures/revision_logreg_metric_plot.png",
  plot = metric_plot,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)
metric_plot <- plot_data |>
  dplyr::filter(Model == "Random Forest") |> 
  ggplot(
    aes(
      x = Value,
      y = Fit,
      color = Missing,
      shape = Separate_Components
    )
  ) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(~Metric, 
             nrow = 4, ncol = 2, 
             scales = "free") + 
  labs(
    x = "Metric Value",
    y = "Fit",
    color = "Missing Data Method",
    shape = NULL
  ) +
  scale_color_manual(
    values = miss_meth_cols,
    labels = scales::label_wrap(20)
  ) + 
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(
      face = "bold",
      color = "white"
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  scale_shape_discrete(name = "Treatment of ALI\nComponents")
metric_plot
ggsave(
  filename = "~/Documents/missALI/figures/revision_rf_metric_plot.png",
  plot = metric_plot,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)
