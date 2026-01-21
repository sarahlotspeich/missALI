# Create factor versions of components with missingness indicators
make_miss_factor = function(x) {
  factor(x,
         levels = c(0, 1, NA),
         labels = c("Healthy", "Unhealthy", "Missing"),
         exclude = NULL)
}
