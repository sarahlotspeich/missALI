#' Find Closest Stable Parent Pattern for Dynamic Nesting
#'
#' @param all_miss_pat Summary dataframe of missingness patterns.
#' @param child_index Integer index of the current child pattern row being evaluated.
#' @param miss_cols Character vector containing the names of the missing indicator columns (e.g., \code{c("MISS_A1C", ...)}).
#' @return Integer index pointing to the row in \code{all_miss_pat} representing the optimal parent model.
 nest_miss_pat = function(all_miss_pat, child_index, miss_cols) {
  # Isolate large, stable candidate patterns within this specific data split
  candidates = all_miss_pat[all_miss_pat$big_enough == TRUE, ]
  
  best_parent_idx = NULL
  min_dropped_vars = Inf
  
  if (nrow(candidates) > 0) {
    # Extract structural missing indices for the child (where indicator is 1)
    child_missing = which(all_miss_pat[child_index, miss_cols] == 1)
    
    for (c_idx in 1:nrow(candidates)) {
      # Extract structural missing indices for the candidate parent
      candidate_missing = which(candidates[c_idx, miss_cols] == 1)
      
      # Condition 1: Child missingness must be a true subset of candidate parent
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
  # if (is.null(best_parent_idx)) {
  #   best_parent_idx = 1
  # }
  if (is.null(best_parent_idx)) {
    stop(
      paste(
        "No valid stable parent found for",
        all_miss_pat$miss_pat[child_index]
      )
    )
  }
  
  return(best_parent_idx)
}