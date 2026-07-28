#' Prediction after pattern submodels approach to fitting regression models with missing ALI components
#'
#' @param submod_res list of results from the \code{pattern_submod_approach()} function.
#' @param ali vector of names for the columns containing the ALI components.
#' @param newdata optional, dataframe from which to predict. If \code{newdata = NULL} (default), predictions are made based on data used to fit the pattern submodels.
#' @return vector of predictions per patient, using their pattern-specific submodel
#' @export
#' @import dplyr
predict_pattern_submod = function(submod_res, ali, newdata = NULL) {
  if (is.null(newdata)) {
    # Make a copy of data (to subset from)
    pred_data = submod_res$data
  } else {
    # Create missingness indicators for each component
    newdata = newdata |>
      mutate(across(all_of(ali),
                    .fns = ~ if_else(is.na(.), 1, 0),
                    .names = "MISS_{.col}"))

    # Take missing data patterns defined for training data
    train_miss_pat = submod_res$data |>
      arrange(desc(n)) |>
      select(starts_with("MISS", ignore.case = FALSE), n, miss_pat, big_enough) |>
      unique()

    # Define the missingness indicators' column names
    miss_ind_cols = grep(pattern = "MISS", 
                         x = colnames(newdata), 
                         ignore.case = FALSE, 
                         value = TRUE)
    
    ## Merge missing data pattern IDs back into patient data (to define subgroups)
    newdata = newdata |>
      left_join(y = train_miss_pat,
                by = miss_ind_cols) |> 
      mutate(nested = FALSE)
    
    ## Check for missing data patterns in test data but not train data
    if (any(is.na(newdata$miss_pat))) {
      ### Subset to rows in newdata that need to be nested
      nest_newdata = newdata |> 
        filter(is.na(miss_pat))

      ### Use apply to get parent pattern for each row 
      parent_newdata = apply(
        X = nest_newdata[, miss_ind_cols], #### only missingness indicators 
        MARGIN = 1, 
        FUN = nest_miss_pat_indiv, 
        all_miss_pat = train_miss_pat, 
        miss_cols = miss_ind_cols
      )
      
      ### Replace NA miss_pat with parent 
      nest_newdata$miss_pat = parent_newdata 
      
      ### Create indicator of being nested 
      nest_newdata$nested = TRUE
      
      ### Combine back with the rest of newdata
      newdata = newdata |> 
        filter(!is.na(miss_pat)) |> 
        bind_rows(nest_newdata)
    }
    
    # Make a copy of newdata (to subset from)
    pred_data = newdata
  }
  ## Save original rownumbers (to ensure returned in same order)
  pred_data$ROW_NUM = 1:nrow(pred_data)

  # Loop over missing data patterns
  for (mp in unique(pred_data$miss_pat)) {
    ## Subset to observations in that pattern
    miss_pat_data = pred_data |>
      filter(miss_pat == mp)

    ## Predict for this subset of patients
    if ("ranger" %in% class(submod_res$fit_list[[mp]])) {
      miss_pat_data$PRED = predict(
        object = submod_res$fit_list[[mp]],
        data = miss_pat_data,
        type = "response")$predictions[, 1]
    } else if ("glm" %in% class(submod_res$fit_list[[mp]])) {
      miss_pat_data$PRED = predict(
        object = submod_res$fit_list[[mp]],
        newdata = miss_pat_data,
        type = "response")
    } else if ("zeroinfl" %in% class(submod_res$fit_list[[mp]])) {
      miss_pat_data$PRED = predict(
        object = submod_res$fit_list[[mp]],
        newdata = miss_pat_data,
        type = "response")
    }
    pred_data = pred_data |>
      filter(miss_pat != mp) |>
      bind_rows(miss_pat_data)
  }

  # Put data back into original order
  pred_data = pred_data[order(pred_data$ROW_NUM), ]

  ## And then return just a vector of predictions
  return(pred_data$PRED)
}
