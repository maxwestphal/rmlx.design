get_x <- function(splits, x) {
  checkmate::assert_class(splits, "rmlx_splits")

  splits[[x]]
}

get_info <- function(splits) {
  get_x(splits, x = "info")
}

get_sets <- function(splits) {
  sets <- get_x(splits, x = "sets")
  if (is.null(sets)) {
    sets <- list()
    for (i in seq_along(1:nrow(get_info(splits)))) {
      sets[[get_info(splits)$idx_train[i]]] <- get_splits(splits, i, "train")
      sets[[get_info(splits)$idx_test[i]]] <- get_splits(splits, i, "test")
    }
  }
  return(sets)
}

get_splits <- function(splits,
                       idx = get_valid_idx(splits),
                       set = c("both", "train", "test"),
                       simplify = length(idx) == 1) {
  stopifnot(all(idx %in% get_valid_idx(splits)))
  checkmate::assert_logical(simplify, len = 1)
  set <- match.arg(set)

  result <- get_x(splits, x = "splits")[idx]

  if (is.null(result)) {
    result <- lapply(idx, \(x){
      list(
        train = splits$sets[[splits$info$idx_train[x]]],
        test = splits$sets[[splits$info$idx_test[x]]]
      )
    })
  }

  if (set != "both") {
    result <- lapply(result, "[[", set)
  }

  # return:
  if (simplify) {
    return(result[[1]])
  } else {
    return(result)
  }
}
