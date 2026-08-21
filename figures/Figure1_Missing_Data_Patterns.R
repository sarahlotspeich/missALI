# Load data 
# Read in data on hospitalizations (suppressed for privacy)
# To run example code, replace with: hosp_dat = read.csv("missALI/data/deidentified_ali_hospitalizations.csv")
hosp_dat = read.csv("~/Documents/Allostatic_load_audits/revision_analysis_dat.csv") |> 
  ## Exclude one person with no vitals/labs before hospitalization day so all components missing
  dplyr::filter(!is.na(ALI))

# Create dataframe of the missing data patterns from raw patient-level data 
ali_miss_pat = hosp_dat |> 
  ## Create missingness indicators for each ALI component
  dplyr::mutate(
    across(
      all_of(ali_comp), 
      .fns = ~ if_else(is.na(.), 1, 0), 
      .names = "MISS_{.col}"
    )
  ) |> 
  ## Group by all missingness indicators to create patterns
  dplyr::group_by(across(starts_with("MISS"))) |> 
  ## Count sample size and hospitalizations per group/pattern
  dplyr::summarize(n = n(), 
                   num_hosp = sum(ANY_ADMIT)) |> 
  ## Arrange from largest to smallest group/pattern
  dplyr::arrange(desc(n))

# Create readable labels for each pattern for axis 
annotated_data = ali_miss_pat |>
  dplyr::ungroup() |> 
  ## For each pattern, show as Pattern xx (xx patients, xx hospitalized)
  dplyr::mutate(
    Pattern_Label = paste0("Pattern ", row_number(), "\n(", n, " Patients,\n", num_hosp, " Hospitalized)"),
    Pattern_Label = reorder(Pattern_Label, row_number())
  ) |>
  ## Transform data from wide --> long to get one row per pattern per ALI component
  tidyr::pivot_longer(
    cols = starts_with("MISS_", ignore.case = FALSE), 
    names_to = "Variable", 
    values_to = "Missing"
  ) |>
  ## Clean up variable names and add an "X" for missing components 
  dplyr::mutate(
    Variable = sub("MISS_", "", Variable),
    cross_label = ifelse(Missing == 1, "X", "")
  )
annotated_data |> 
  write.csv("~/Downloads/annotated_data.csv")

# Create the plot 
fig1 = annotated_data |> 
  ggplot(aes(x = Variable, 
             y = Pattern_Label)) +
  geom_tile(aes(fill = factor(Missing)), 
            color = "white", 
            linewidth = 0.4) +
  geom_text(aes(label = cross_label), 
            color = "#f6cec3", 
            size = 10) + 
  scale_fill_manual(values = c("0" = "#f0ede6", "1" = "#e76f51"), 
                    labels = c("0" = "Observed", "1" = "Missing"), 
                    name = "", 
                    guide = "none") +
  theme_minimal(base_size = 24) +
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
       y = "Missing Data Pattern (Number of Patients, Number Hospitalized)") +
  theme(axis.title = element_text(face = "bold"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        legend.title = element_blank(), 
        legend.position = "top")

# Save it 
ggsave(plot = fig1, 
       filename = "~/Documents/missALI/figures/revision_ali_miss_patterns.png", 
       device = "png", width = 12, height = 16, units = "in")