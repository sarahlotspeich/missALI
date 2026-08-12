#' Find Closest Stable Parent Pattern for Dynamic Nesting
#'
#' @param child_pat vector of the current child pattern row being evaluated.
#' @param all_miss_pat summary dataframe of missingness patterns.
#' @param miss_cols character vector containing the names of the missing indicator columns (e.g., \code{c("MISS_A1C", ...)}).
#' @return String identifying the missing pattern in \code{all_miss_pat} representing the optimal parent model.
nest_miss_pat_indiv = function(child_pat, all_miss_pat, miss_cols) {
  # Isolate large, stable candidate patterns within this specific data split
  candidates = all_miss_pat[all_miss_pat$big_enough == TRUE, ]
  
  best_parent_idx = NULL
  min_dropped_vars = Inf
  
  if (nrow(candidates) > 0) {
    # Extract structural missing indices for the child (where indicator is 1)
    child_missing = which(child_pat[miss_cols] == 1)
    
    for (c_idx in 1:nrow(candidates)) {
      # Extract structural missing indices for the candidate parent
      candidate_missing = which(candidates[c_idx, miss_cols] == 1)
      
      # Condition 1: Candidate missing must be a true subset of the child's
      # (All variables missing in the child must also be missing in the parent)
      if (all(child_missing %in% candidate_missing)) {
        
        # Condition 2: Calculate distance (how many extra variables are dropped)
        dropped_difference = length(candidate_missing) - length(child_missing)
        
        # Keep the candidate that drops the fewest variables
        if (dropped_difference < min_dropped_vars) {
          min_dropped_vars = dropped_difference
          # Map back to the master row position inside the original all_miss_pat
          best_parent_idx = which(all_miss_pat$miss_pat == candidates$miss_pat[c_idx])
        }
      }
    }
  }
  
  # Absolute Safety Net: If an orphan pattern doesn't nest anywhere, default to MP-1
  if (is.null(best_parent_idx)) {
    best_parent_idx = 1
  }
  
  return(all_miss_pat$miss_pat[best_parent_idx]) ## return pattern
}