# Calculate complete-case proportion ALI
#' @importFrom dplyr select group_by summarize left_join
#' @importFrom tidyr gather
calc_cc_prop_ali = function(data, ali) {
  ## Summarize by patient and count numbers unhealthy and missing
  sum_data = data |>
    select(PAT_MRN_ID, all_of(ali)) |>
    gather(key = "COMP", value = "VAL", -1) |>
    group_by(PAT_MRN_ID, .inform = FALSE) |>
    summarize(PROP_UNHEALTHY = mean(VAL == 1, na.rm = TRUE))

  ## Merge it back into full patient data
  data = data |>
    left_join(sum_data, by = "PAT_MRN_ID")

  ## Return data with added column: PROP_UNHEALTHY
  return(data)
}

# Calculate count unhealthy/count missing ALI
calc_num_miss_ali = function(data, ali) {
  ## Calculate number missing per patient
  data$NUM_MISSING = apply(X = is.na(data[, ali]),
                           MARGIN = 1,
                           FUN = sum,
                           na.rm = TRUE)

  ## Calculate number unhealthy per patient
  data$NUM_UNHEALTHY = apply(X = data[, ali],
                             MARGIN = 1,
                             FUN = sum,
                             na.rm = TRUE)

  ## Return data with added column: PROP_UNHEALTHY
  return(data)
}

# Calculate best/worst case imputation ALI
#' @importFrom dplyr mutate_at
#' @importFrom tidyr replace_na
calc_case_ali = function(data, ali, best, comp_sep = FALSE) {
  ## Fill in based on which case
  if (best) {
    data = data |>
      mutate_at(.vars = ali,
                replace_na,
                0)
  } else {
    data = data |>
      mutate_at(.vars = ali,
                replace_na,
                1)
  }

  ## Calculates proportion of unhealthy components
  ### (after imputation)
  data$CASE_ALI = rowSums(data[, ali]) / 10

  ## Return data with added column: CASE_ALI
  return(data)
}

# Create 3-level factor versions of each component: unhealthy, healthy, missing
#' @importFrom dplyr mutate
make_all_miss_factor = function(data, ali) {
  # Create factor versions of ALI components with missingness indicators
  data = data |>
    mutate(across(all_of(ali),
                  .fns = ~ make_miss_factor(x = .),
                  .names = "{.col}_F"))
  
  ## Return data with added factor columns
  return(data)
}
