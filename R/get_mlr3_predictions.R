#' Get predictions after a mlr3 training/tuning with an custom data splitting
#'
#' @param splits (rmlx_splits) \cr Splits that were utilized in mlr3 training/tuning process
#' via "custom" resampling.
#' @param instance (OptimInstance) \cr \code{OptimInstance} object resulting from mlr3 training/tuning process
#' with \code{$archive$predictions()} function.
#' @param idx_model (NULL | numeric) \cr either NULL to select all models or an integer (vector) in the range
#' 1:instance$archive$n_evals
#'
#' @returns (data.table) A table of predictions and labels for all observations.
#'
#' @details
#' See ["overview"](overview.html) vignette.
#'
#'
#' @export
get_mlr3_predictions <- function(splits, instance, idx_model = NULL) {
  if (is.null(idx_model)) {
    idx_model <- 1:instance$archive$n_evals
  }
  lapply(idx_model, \(i) cbind(idx_model = i, get_mlr3_predictions_1(i, splits, instance))) %>%
    data.table::rbindlist() %>%
    return()
}

#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
get_mlr3_predictions_1 <- function(idx_model = 1, splits, instance) {
  map <- get_mlr3_map(splits)
  lapply(
    seq_along(map),
    \(i) cbind(
      map[[i]],
      instance$archive$predictions(idx_model)[[i]] %>%
        data.table::as.data.table()
    )
  ) %>%
    data.table::rbindlist() %>%
    return()
}
