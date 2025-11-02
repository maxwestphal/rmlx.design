#' Derive metrics for all data splits and models.
#'
#' @param predictions (data.frame) data with variables defined by \code{by} and predictions and labels.
#' For instance the result of \code{\link{get_mlr3_predictions}}.
#' @param splits (rmlx_splits) splits object
#' @param ... (...) calculation of metrics based on variables in \code{predictions},
#' will be passed to \code{dplyr::reframe}
#' @param by (character) grouping variables defining the associated predictions
#'
#' @returns (data.frame), usually passed on \code{\link{get_mlr3_predictions}}.
#' @export
derive_metrics <- function(predictions,
                           splits,
                           ...,
                           by = c("idx_split", "idx_model")) {
  . <- NULL

  predictions %>%
    dplyr::group_by_at(by) %>%
    dplyr::reframe(...) %>%
    dplyr::left_join(get_info(splits), ., by = by[1])
}
