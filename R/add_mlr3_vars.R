#' Add mlr3 variables to an \code{rmlx_splits} object
#'
#' @param splits (rmlx_splits)
#'
#' @returns (rmlx_splits)
#' @export
add_mlr3_vars <- function(splits) {
  splits$mlr3 <- derive_mlr3_vars(splits)
  return(splits)
}

derive_mlr3_vars <- function(splits) {
  idx_obs <- idx_split <- idx_train <- idx_test <- NULL

  idx_train <- unique(splits$info$idx_train)
  idx_test_list <- splits$info %>%
    dplyr::group_by(idx_train) %>%
    dplyr::reframe(idx_test_list = list(idx_test)) %>%
    "[["("idx_test_list")

  splits$sets <- get_sets(splits)

  sets_full <- get_sets(splits)
  sets <- list()

  sets$train <- lapply(idx_train, \(i) sets_full[[i]])

  idx_split_per_idx_train <-
    lapply(
      idx_train,
      \(i) filter(splits$info, idx_train == i) %>%
        dplyr::select("idx_split") %>%
        unlist() %>%
        unname()
    )


  map <- lapply(
    idx_split_per_idx_train,
    \(x) lapply(
      x,
      \(i) data.table(
        idx_split = i,
        idx_obs = get_sets(splits)[[splits$info[idx_split == i, idx_test]]]
      )
    ) %>%
      data.table::rbindlist()
  )

  sets$test <- lapply(map, \(x) x[, idx_obs])

  return(list(sets = sets, map = map))
}
