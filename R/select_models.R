#' Select models based on the results of the inner loop of a nested data splitting scheme.
#'
#' @param data (data.frame) Result from \code{derive_metrics} or similar data.frame.
#' @param metric (character)
#' @param aggregate (function) How should results be aggregated across inner folds? (Default: mean)
#' @param select (function) Which model should be chosen? (e.g. \code{max} for performance metrics,
#'  \code{min} for error metrics)
#' @param groups (character) <experimental> should the selection be conducted per group?
#' @param break_ties (character) How should ties be broken? (Default: "randomly")
#' @param args_aggregate (list) further named arguments passed to \code{aggregate}
#' @param args_select (list) further named arguments passed to \code{select}
#'
#' @returns (data.frame) Benchmark results for selected models
#' @export
select_models <- function(data,
                          metric,
                          aggregate = mean,
                          select = max,
                          groups = NULL,
                          break_ties = "random",
                          args_aggregate = list(),
                          args_select = list()) {
  idx_model <- idx_model_opt <- metric_aggr <- metric_sel <- type <- NULL

  metric <- as.name(metric)
  break_ties <- match.arg(break_ties)


  data_sel <- data %>%
    dplyr::filter(type == "inner") %>%
    dplyr::group_by_at(c("idx_outer", "idx_model", groups)) %>%
    dplyr::summarize(
      metric_aggr =
        do.call(aggregate, args = c(list(!!as.name(metric)), args_aggregate))
    ) %>%
    dplyr::group_by_at(c("idx_outer", groups)) %>%
    dplyr::mutate(
      metric_sel =
        rep(do.call(select, args = c(list(!!as.name("metric_aggr")), args_select)), dplyr::n())
    ) %>%
    dplyr::filter(metric_aggr == metric_sel) %>%
    dplyr::select_at(c("idx_outer", groups, "idx_model")) %>%
    dplyr::rename(idx_model_opt = idx_model)

  if (break_ties == "random") {
    data_sel <- data_sel %>%
      dplyr::group_by_at(c("idx_outer", groups)) %>%
      dplyr::slice_sample(n = 1)
  }

  data %>%
    dplyr::left_join(data_sel, by = c("idx_outer", groups)) %>%
    dplyr::filter(idx_model == idx_model_opt) %>%
    dplyr::select(-idx_model_opt)
}
