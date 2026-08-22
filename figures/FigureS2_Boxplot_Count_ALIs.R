# Read in data on hospitalizations (suppressed for privacy)
# To run example code, replace with: hosp_dat = read.csv("missALI/data/deidentified_ali_hospitalizations.csv")
hosp_dat = read.csv("~/Documents/Allostatic_load_audits/revision_analysis_dat.csv") |> 
  ## Exclude one person with no vitals/labs before hospitalization day so all components missing
  dplyr::filter(!is.na(ALI))

# Transform data from wide --> long to have one row per version of ALI 
ali_box_dat = hosp_dat |> 
  dplyr::mutate(ALI_MISS = 10 - ALI_DENOM) |> 
  dplyr::select(PAT_MRN_ID, ALI_NUM, ALI_MISS)

# Make the boxplot
figS2 = ali_box_dat |> 
  tidyr::gather(key = "MISS_METH", value = "VALUE", -1) |> 
  dplyr::mutate(MISS_METH = factor(x = MISS_METH, 
                                   levels = c("ALI_NUM",
                                              "ALI_MISS"), 
                                   labels = c("Unhealthy", 
                                              "Missing"))) |> 
  ggplot(aes(x = MISS_METH, y = VALUE, fill = MISS_METH)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#1b3c73", "#a8c56e"), guide = "none") + 
  scale_x_discrete(labels = scales::label_wrap(12)) +
  theme_minimal(base_size = 14) +
  labs(x = "Component Status",
       y = "Count") + 
  theme(axis.title = element_text(face = "bold"), 
        title = element_text(face = "bold")) + 
  coord_flip()

# Save it 
ggsave(filename = "~/Documents/missALI/figures/revision_compare_counts_missing_unhealthy_boxplot.png",
       plot = figS2, device = "png", width = 10, height = 5, units = "in")
