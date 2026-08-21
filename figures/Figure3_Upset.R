# Read in data on hospitalizations (suppressed for privacy)
# To run example code, replace with: hosp_dat = read.csv("missALI/data/deidentified_ali_hospitalizations.csv")
hosp_dat = read.csv("~/Documents/Allostatic_load_audits/revision_analysis_dat.csv") |> 
  ## Exclude one person with no vitals/labs before hospitalization day so all components missing
  dplyr::filter(!is.na(ALI))

## Rename columns before making upset plot so that labels are readable
fig3 = hosp_dat |> 
  dplyr::rename(
    "Hemoglobin A1C" = A1C, 
    "Serum Albumin" = ALB,
    "Body Mass Index" = BMI,
    "Homocysteine" = HCST,
    "Systolic Blood Pressure" = BP_SYSTOLIC,
    "Diastolic Blood Pressure" = BP_DIASTOLIC,
    "Cholesterol" = CHOL,
    "C-Reactive Protein" = CRP,
    "Creatinine Clearance" = CREAT_C,
    "Triglycerides" = TRIG
  ) |> 
  dplyr::select(-starts_with("NUM")) |> 
  naniar::gg_miss_upset(nsets = 10,
                        sets.bar.color = "#218288", 
                        main.bar.color = "#218288",
                        matrix.color = "#218288")

# Save it 
png("~/Documents/missALI/figures/revision_ali_components_upset.png", width = 800, height = 800, res = 150)
fig3
dev.off()