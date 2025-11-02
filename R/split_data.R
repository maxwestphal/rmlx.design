#' @title Split data
#' @name split_data
#'
#' @description
#' Subset the \code{data} to retrieve the train and/or test data corresponding to \code{splits[[idx]]},
#' where \code{length(idx) == 1}.
#'
#' @param data (data.frame) \cr \code{data.frame} that was used in \code{\link{derive_splits}()}
#' @param splits (rmlx_splits) \cr a \code{rmlx_splits} object corresponding to \code{data}
#' @param idx (numeric) \cr index of the target split (with length 1)
#' @param set (character) \cr What data sets should be retreived?
#' @param simplify (character) \cr Should the output be simplified?
#' (only possible for \code{length(idx) == 1} and default in this case)
#'
#' @return
#' - \code{get_data_splits}: retrieve list (of lists) with train and test data sets
#' - \code{get_data_split}: retrieve list with single train set and test data set (\code{length(idx) == 1})
#' - \code{get_train_sets}: retrieve list of train data sets
#' - \code{get_train_set}: retrieve single train data set (\code{length(idx) == 1})
#' - \code{get_test_sets}: retrieve list of test data sets
#' - \code{get_test_set}: retrieve single test data set (\code{length(idx) == 1})
#' - \code{split_data}: a list containing one or multiple train and/or test data sets (depending on \code{set} and \code{simplify})
#' @export
get_data_splits <- function(data,
                            splits,
                            idx = get_valid_idx(splits)) {
  split_data_internal(data, splits, idx, set = "both", simplify = FALSE)
}

#' @rdname split_data
#' @export
get_data_split <- function(data, splits, idx) {
  split_data_internal(data, splits, idx, set = "both", simplify = TRUE, idx_length = 1)
}

#' @rdname split_data
#' @export
get_train_sets <- function(data, splits, idx = get_valid_idx(splits)) {
  split_data_internal(data, splits, idx, set = "train", simplify = FALSE)
}

#' @rdname split_data
#' @export
get_train_set <- function(data, splits, idx) {
  split_data_internal(data, splits, idx, set = "train", simplify = TRUE, idx_length = 1)
}

#' @rdname split_data
#' @export
get_test_sets <- function(data, splits, idx = get_valid_idx(splits)) {
  split_data_internal(data, splits, idx, set = "test", simplify = FALSE)
}

#' @rdname split_data
#' @export
get_test_set <- function(data, splits, idx) {
  split_data_internal(data, splits, idx, set = "test", simplify = TRUE, idx_length = 1)
}


#' @rdname split_data
#' @export
split_data <- function(data,
                       splits,
                       idx = get_valid_idx(splits),
                       set = c("both", "train", "test"),
                       simplify = length(idx) == 1) {
  split_data_internal(data, splits, idx, set, simplify)
}


#' Get valid ids
#'
#' @param splits (rmlx_splits) \cr an object created with \code{\link{derive_splits}}
#'
#' @return (numeric) \cr a vector with valid indices
#' @export
get_valid_idx <- function(splits) {
  1:nrow(splits$info)
}


split_data_internal <- function(data,
                                splits,
                                idx,
                                set = c("both", "train", "test"),
                                simplify = length(idx) == 1,
                                idx_length = 1:length(get_valid_idx(splits))) {
  set <- match.arg(set)
  check_splits_data_args(
    data = data, splits = splits, idx = idx,
    set = set, simplify = simplify, idx_length = idx_length
  )

  splits <- get_splits(splits)

  result <-
    lapply(idx, function(i) {
      if (set == "both") {
        return(
          list(
            train = data[splits[[i]]$train, ],
            test  = data[splits[[i]]$test, ]
          )
        )
      }

      if (set == "train") {
        return(
          data[splits[[i]]$train, ]
        )
      }

      if (set == "test") {
        return(
          data[splits[[i]]$test, ]
        )
      }

      stop("argument set should be 'both', 'train' or 'test'")
    })

  if (simplify) {
    result <- result[[1]]
  }

  return(result)
}


check_splits_data_args <- function(data,
                                   splits,
                                   idx,
                                   set = c("both", "train", "test"),
                                   simplify = length(idx) == 1,
                                   idx_length = 1:length(get_valid_idx(splits))) {
  if (missing(idx)) {
    message("[rmlx] argument idx is missing - valid idx values are listed below")
    return(get_valid_idx(splits))
  }

  stopifnot(methods::is(data, "data.frame"))
  stopifnot(methods::is(splits, "rmlx_splits"))
  stopifnot(is.numeric(idx))

  stopifnot(is.logical(simplify))
  stopifnot(!(length(idx) > 1 & simplify))

  stopifnot(is.numeric(idx_length))

  stopifnot(length(idx) %in% idx_length)
  stopifnot(idx %in% get_valid_idx(splits))

  return(invisible(TRUE))
}
