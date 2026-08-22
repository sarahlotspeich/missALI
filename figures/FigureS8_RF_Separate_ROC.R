# Setup data and helper functions
source("~/Documents/missALI/analysis/setup_for_random_forest_fit.R")

# Source fitted models and extract their ROC curves 
## Best-case imputation (separate)
full_best = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/full_best_case_separate.rds")
kfold_best = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/kfold_best_case_separate.rds")
## Worst-case imputation (separate)
full_worst = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/full_worst_case_separate.rds")
kfold_worst = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/kfold_worst_case_separate.rds")
## Missingness categories 
full_miss_cat = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/full_missing_category.rds")
## Pattern submodels 
full_pat_sub = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/full_pattern_submodels.rds")
kfold_pat_sub = readRDS("~/Documents/missALI/analysis/fitted_models/random_forest/kfold_pattern_submodels.rds")
# ROC curves for Approaches 4-6
## Create custom design since missingness categories doesn't have k-fold
design = "
ABCD
EF#G
"
comb_plot = (full_best$roc + full_worst$roc + full_miss_cat$roc + full_pat_sub$roc +
               kfold_best$roc + kfold_worst$roc + kfold_pat_sub$roc) +
  plot_layout(design = design)
ggsave(filename = "~/Documents/missALI/figures/revision_all_sep_comp_roc_rf.png", 
       plot = comb_plot, 
       device = "png", width = 16, height = 10, units = "in")