library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Prepare data and add custom labels
annotated_data <- ali_miss_pat |>
  mutate(
    Pattern_Label = paste0("Pattern ", row_number(), "\n(", n, " Patients)"),
    Pattern_Label = reorder(Pattern_Label, row_number())
  ) |>
  pivot_longer(
    cols = starts_with("MISS_", ignore.case = FALSE), 
    names_to = "Variable", 
    values_to = "Missing"
  ) |>
  mutate(
    Variable = sub("MISS_", "", Variable),
    # Create an explicit text column: put an "X" if missing, leave blank if observed
    cross_label = ifelse(Missing == 1, "X", "")
  )

# 2. Plot the tile map with the text overlay
ggplot(annotated_data, aes(x = Variable, y = Pattern_Label)) +
  # The base grid layer
  geom_tile(aes(fill = factor(Missing)), color = "white", linewidth = 0.4) +
  
  # The text overlay layer: places the "X" right in the center of the tiles
  geom_text(aes(label = cross_label), color = "#f6cec3", size = 3.5, fontface = "bold") +
  
  # Color scale styling (using clean, modern muted tones)
  scale_fill_manual(
    values = c("0" = "#f0ede6", "1" = "#e76f51"), # Off-white for observed, light peach/red tint for missing
    labels = c("0" = "Observed", "1" = "Missing"), 
    name = ""
  ) +
  theme_minimal(base_size = 16) +
  scale_x_discrete(
    labels = function(x) {
      # 1. Map your raw dataframe strings to clean clinical names
      clean_names <- c(
        BP_SYSTOLIC  = "BP Systolic",
        BP_DIASTOLIC = "BP Diastolic",
        BMI          = "Body Mass Index",
        ALB          = "Serum Albumin",
        TRIG         = "Triglycerides",
        CHOL         = "Cholesterol",
        A1C          = "Hemoglobin A1C",
        HCST         = "Homocysteine",
        CRP          = "C-Reactive Protein",
        CREAT_C      = "Creatinine Clearance"
      )
      
      # 2. Look up the clean name, and wrap it at a width of 10 characters
      stringr::str_wrap(clean_names[x], width = 10)
    }
  ) +
  labs(
    x = "Allostatic Load Index Component", 
    y = "Missing Data Pattern (Number of Patients)"
  ) +
  theme(
    axis.title = element_text(face = "bold"),
    #axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    legend.title = element_blank(), 
    legend.position = "top"
  )
ggsave(filename = "~/Documents/missALI_prediction/figures/ali_miss_patterns.png", 
       device = "png", width = 13, height = 9, units = "in")
