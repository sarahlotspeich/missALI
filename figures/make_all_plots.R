# Setup 
setwd("~/Documents/missALI/figures/")

# Re-make metrics data (just in case models have been updated)
source("~/Documents/missALI/analysis/logistic_regression_metrics.R")
source("~/Documents/missALI/analysis/random_forest_metrics.R")

# Loop over plots and generate them 
## one .R file per figure
plot_scripts = grep(x = list.files(pattern = ".R"), 
                    pattern = "Figure", 
                    value = TRUE) 
for (p in plot_scripts) {
  print(p)
  ## Clear environment just to be sure there's no conflict between plots
  ## But leave index miss_approaches and m
  rm(list = setdiff(ls(), c("p", "plot_scripts")))
  ## Fit full-sample and k-fold models 
  source(p)  
}