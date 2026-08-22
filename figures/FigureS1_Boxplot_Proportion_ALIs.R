# Read in data on hospitalizations (suppressed for privacy)
# To run example code, replace with: hosp_dat = read.csv("missALI/data/deidentified_ali_hospitalizations.csv")
hosp_dat = read.csv("~/Documents/Allostatic_load_audits/revision_analysis_dat.csv") |> 
  ## Exclude one person with no vitals/labs before hospitalization day so all components missing
  dplyr::filter(!is.na(ALI))

# Define vector of binary ALI component column names
ali_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
             "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")

# Transform data from wide --> long to have one row per version of ALI 
ali_box_dat = hosp_dat |> 
  dplyr::select(PAT_MRN_ID, all_of(ali_comp)) |> 
  tidyr::gather(key = "COMPONENT", value = "VALUE", -1) |> 
  dplyr::group_by(PAT_MRN_ID) |> 
  dplyr::summarize(CC_PROP_ALI = mean(VALUE, na.rm = TRUE), 
                   BEST_CASE_ALI = sum(ifelse(is.na(VALUE), 0, VALUE), na.rm = TRUE) / 10, 
                   WORST_CASE_ALI = sum(ifelse(is.na(VALUE), 1, VALUE), na.rm = TRUE) / 10)

# Make the boxplot
figS1 = ali_box_dat |> 
  tidyr::gather(key = "MISS_METH", value = "VALUE", -1) |> 
  dplyr::mutate(MISS_METH = factor(x = MISS_METH, 
                                   levels = c("BEST_CASE_ALI",
                                              "CC_PROP_ALI", 
                                              "WORST_CASE_ALI"), 
                                   labels = c("Best Case Imputation", 
                                              "Complete-Case Proportion", 
                                              "Worst Case Imputation"))) |> 
  ggplot(aes(x = MISS_METH, y = VALUE, fill = MISS_METH)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#218288", "#FF855D", "#c1dbd0"), guide = "none") + 
  scale_x_discrete(labels = scales::label_wrap(12)) +
  theme_minimal(base_size = 14) +
  labs(x = "Missing Data Technique",
       y = "Allostatic Load Index (ALI)") + 
  theme(axis.title = element_text(face = "bold"), 
        title = element_text(face = "bold")) + 
  coord_flip()

# Save it 
ggsave(filename = "~/Documents/missALI/figures/revision_compare_ali_boxplot.png", 
       plot = figS1, device = "png", width = 10, height = 7, units = "in")