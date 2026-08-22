# Read in data on hospitalizations (suppressed for privacy)
# To run example code, replace with: hosp_dat = read.csv("missALI/data/deidentified_ali_hospitalizations.csv")
hosp_dat = read.csv("~/Documents/Allostatic_load_audits/revision_analysis_dat.csv") |> 
  ## Exclude one person with no vitals/labs before hospitalization day so all components missing
  dplyr::filter(!is.na(ALI))

# Define vector of binary ALI component column names
ali_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
             "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")

# Function to convert 0/1/NA into labeled factor
make_miss_factor = function(x) {
  dplyr::case_when(
    is.na(x) ~ "Missing",
    x == 0   ~ "Healthy",
    x == 1   ~ "Unhealthy") |>
    factor(levels = c("Missing", "Unhealthy", "Healthy"))
}

# Select components + convert to string using function above
ali_components_only = hosp_dat |>
  dplyr::select(all_of(ali_comp)) |>
  dplyr::mutate(across(everything(), make_miss_factor))

# Pivot from wide (columns per component) to long (rows per component)
long_ali = ali_components_only |>
  tidyr::pivot_longer(cols = everything(),
                      names_to = "Component",
                      values_to = "Value")

# Order components by MOST missing to LEAST
order_levels = long_ali |>
  dplyr::group_by(Component) |>
  dplyr::summarize(num_missing = sum(Value == "Missing")) |>
  dplyr::arrange(desc(num_missing)) |>
  dplyr::pull(Component)

# Create plot 
fig2 = long_ali |>
  dplyr::mutate(COMP = factor(Component, levels = order_levels),
                Finding = factor(Value, levels = c("Missing", "Unhealthy", "Healthy"))) |>
  ggplot(aes(x = COMP, fill = Finding)) +
  geom_bar(position = "fill", color = "black", linewidth = 0.25) +
  scale_fill_manual(values = c("Missing"   = "#218288",
                               "Unhealthy" = "#FF855D",
                               "Healthy"   = "#c1dbd0"), 
                    name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(
    labels = function(x) {
      clean_names = c(
        BP_SYSTOLIC  = "SBP", #"BP Systolic",
        BP_DIASTOLIC = "DBP", #"BP Diastolic",
        BMI          = "BMI", #"Body Mass Index",
        ALB          = "ALB", #"Serum Albumin",
        TRIG         = "TRIG", #"Triglycerides",
        CHOL         = "CHOL", #"Cholesterol",
        A1C          = "HBA1C", #"Hemoglobin A1C",
        HCST         = "HCST", #"Homocysteine",
        CRP          = "CRP", #"C-Reactive Protein",
        CREAT_C      = "CREAT" #"Creatinine Clearance"
      )
      stringr::str_wrap(clean_names[x], width = 10)
    }) +
  labs(x = "Allostatic Load Index Component",
       y = "Proportion of Measurements") +
  theme_minimal(base_size = 24) +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip()

ggsave(filename = "~/Documents/missALI/figures/revision_miss_healthy_unhealthy_barplot.png",
       plot = fig2, device = "png", width = 10, height = 7, units = "in")