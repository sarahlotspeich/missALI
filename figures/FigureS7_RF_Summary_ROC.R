# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_random_forest_fit.R")

# Source fitted models and extract their ROC curves 
## Complete-case proportion
full_cc_prop = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/full_complete_case_proportion.rds")
kfold_cc_prop = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/kfold_complete_case_proportion.rds")
## Counts of missing and unhealthy
full_num_miss = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/full_counts_missing_unhealthy.rds")
kfold_num_miss = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/kfold_counts_missing_unhealthy.rds")
## Best-case imputation (summary)
full_best = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/full_best_case_summary.rds")
kfold_best = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/kfold_best_case_summary.rds")
## Worst-case imputation (summary)
full_worst = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/full_worst_case_summary.rds")
kfold_worst = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/kfold_worst_case_summary.rds")
# ROC Curves for Approaches 1-3b 
comb_plot = (full_cc_prop$roc | full_num_miss$roc | full_best$roc | full_worst$roc) / 
  (kfold_cc_prop$roc |  kfold_num_miss$roc | kfold_best$roc | kfold_worst$roc)
ggsave(filename = "~/Documents/missALI/figures/revision_all_summ_meas_roc_rf.png", 
       plot = comb_plot, 
       device = "png", width = 16, height = 10, units = "in")