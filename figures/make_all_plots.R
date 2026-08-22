# Setup 
setwd("~/Documents/missALI/figures/")

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