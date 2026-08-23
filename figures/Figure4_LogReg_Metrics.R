# Read in metrics from model fits
res = read.csv("~/Documents/missALI/data/logistic_regression_metrics.csv")

# Define color scheme 
miss_meth_cols = c("#2b9d8f", "#e76f51", "#1b3c73", "#c1dbd0", 
                   "#a8c56e", "#6a4c93", "#e8b89a", "#c0404a")

# Create long data for plot (one row per fit per metric)
plot_data = res |>
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
    Fit = factor(x = Fit, 
                 levels = c("K-Fold", "Full-Sample"),
                 labels = c("K-Fold", "Full-\nSample")),
    Separate_Components = factor(x = Separate_Components,
                                 levels = c("Summary Measure", "Separate Components"), 
                                 labels = c("Summary\nMeasure", "Separate\nComponents")),
    Metric = factor(x = Metric,
                    levels = c("AUC", "brier", "cal_intercept", "cal_slope",
                               "sensitivity", "specificity", "ppv", "npv"),
      labels = c("AUC", "Brier Score",
                 "Calibration Intercept", "Calibration Slope",
                 "Sensitivity", "Specificity",
                 "PPV", "NPV"))) |> 
  dplyr::mutate(
    Value = dplyr::if_else(condition = Fit == "Full-\nSample" & 
                             Metric %in% c("Calibration Intercept", "Calibration Slope"), 
                           true = NA, 
                           false = Value)
  )
metric_plot = plot_data |>
  ggplot(aes(x = Value,
             y = Fit,
             color = Missing,
             shape = Separate_Components)) +
  geom_point(size = 5,
             position = position_dodge(width = 0)) +
  facet_wrap(~Metric, 
             nrow = 4, ncol = 2, 
             scales = "free") + 
  labs(
    x = "Metric Value",
    y = "Data Used to Fit Model",
    color = "Missing Data\nMethod",
    shape = NULL
  ) +
  scale_color_manual(
    values = miss_meth_cols,
    labels = scales::label_wrap(15)
  ) + 
  theme_minimal(base_size = 24) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(face = "bold",
                              color = "white"),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  scale_shape_discrete(name = "Treatment of\nComponents") + 
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01), 
                     n.breaks = 3)
ggsave(
  filename = "~/Documents/missALI/figures/revision_logreg_metric_plot.png",
  plot = metric_plot,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)
