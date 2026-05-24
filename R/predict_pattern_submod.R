#' Prediction after pattern submodels approach to fitting regression models with missing ALI components
#'
#' @param submod_res list of results from the \code{pattern_submod_approach()} function.
#' @return vector of predictions per patient, using their pattern-specific submodel
#' @export
#' @import dplyr
predict_pattern_submod = function(submod_res) {
  # Make a copy of data (to subset from)
  pred_data = submod_res$data

  ## Save original rownumbers (to ensure returned in same order)
  pred_data$ROW_NUM = 1:nrow(pred_data)

  # Loop over missing data patterns
  for (mp in names(submod_res$fit_list)) {
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
