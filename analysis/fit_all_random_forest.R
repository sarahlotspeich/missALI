# Setup 
setwd("~/Documents/missALI/analysis/random_forest/")

# Loop over approaches and run models 
miss_approaches = list.files() ## one .R file per missing data approach
for (m in miss_approaches) {
  ## Clear environment just to be sure there's no conflict between methods
  ## But leave index miss_approaches and m
  rm(list = setdiff(ls(), c("m", "miss_approaches")))
  ## Fit full-sample and k-fold models 
  source(m)  
}
# Create metrics dataset 
source("~/Documents/missALI/analysis/random_forest_metrics.R")
