## Load packages
library(missALI) ## for models x missing data corrections 
library(pROC) ## for ROC curves and AUC 
library(patchwork) ## to combine plots

## Read in data on hospitalizations (suppressed for privacy)
#hosp_dat = read.csv("~/Documents/missALI/data/deidentified_ali_hospitalizations.csv")
hosp_dat = read.csv("~/Documents/Allostatic_load_audits/revision_analysis_dat.csv") |> 
  ## Exclude one person with no vitals/labs before hospitalization day so all components missing
  filter(!is.na(ALI))

## Define color scheme 
miss_meth_cols = c("#2b9d8f", "#e76f51", "#1b3c73", "#c1dbd0", 
                   "#a8c56e", "#6a4c93", "#e8b89a", "#c0404a")

# Define vector of binary ALI component column names
ali_comp = c("A1C", "ALB", "BMI", "CHOL", "CRP",
             "CREAT_C", "HCST", "TRIG", "BP_DIASTOLIC", "BP_SYSTOLIC")

# Source helper functions for performance plots and metrics
source("~/Documents/missALI/analysis/full_sample_roc.R")
source("~/Documents/missALI/analysis/calibration_functions.R")
source("~/Documents/missALI/analysis/summ_plot_predictions.R")