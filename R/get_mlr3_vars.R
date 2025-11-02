#' @name get_mlr3_
#' @rdname get_mlr3_
#'
#' @title Retrieve objects needed for mlr3 training/tuning.
#'
#' @param splits (rmlx_splits) \cr the splits object
#'
#' @details To avoid re-calculation of these objects, consider adding them to the splits object via
#' \code{\link{add_mlr3_vars}}.
#'
#'
#' @return Depends on the function name:
#'
#' - sets_train: list indices of train sets.
#' - sets_test: list indices of test sets.
#' - sets = list(train=sets_train, test=sets_test)
#' - map: data.table which gives the idx_split for each test observation
#' - vars: list(sets=sets, map=map)
NULL


#' @rdname get_mlr3_
#' @export
get_mlr3_sets_train <- function(splits) {
  get_mlr3_vars(splits)$sets$train
}

#' @rdname get_mlr3_
#' @export
get_mlr3_sets_test <- function(splits) {
  get_mlr3_vars(splits)$sets$test
}

#' @rdname get_mlr3_
#' @export
get_mlr3_sets <- function(splits) {
  get_mlr3_vars(splits)$sets
}

#' @rdname get_mlr3_
#' @export
get_mlr3_map <- function(splits) {
  get_mlr3_vars(splits)$map
}

#' @rdname get_mlr3_
#' @export
get_mlr3_vars <- function(splits) {
  if (is.null(splits$mlr3)) {
    message("[rmlx] deriving mlr3 variables - see `?add_mlr3_vars` to avoid repetition of this step...")
    return(derive_mlr3_vars(splits))
  } else {
    return(splits$mlr3)
  }
}
