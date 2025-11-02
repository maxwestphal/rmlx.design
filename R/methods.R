valid_methods <- c("none", "holdout", "cv", "extensive_cv")

none <- function(data) {
  return(list())
}

holdout <- function(data, pr_test = 0.2, strata = NULL) {
  folds <- draw_folds(data, n_folds = 2, strata = strata, fold_size = c(1 - pr_test, pr_test))
  names(folds) <- c("train", "test")

  return(list(folds))
}


#' @importFrom splitTools partition
cv <- function(data, n_folds = 5, strata = NULL) {
  folds <- draw_folds(data = data, n_folds = n_folds, strata = strata)
  n_obs <- nrow(data)
  splits <- lapply(folds, function(x) list(train = setdiff(1:n_obs, x), test = x))
  names(splits) <- NULL

  ## add info_folds attribute:
  fi <- matrix("train", n_folds, n_folds)
  fi[row(fi) == col(fi)] <- "test"
  colnames(fi) <- paste0("fold_", LETTERS[1:n_folds])
  info_folds <- cbind(idx_split = 1:n_folds, as.data.frame(fi))

  attr(splits, "info_folds") <- info_folds

  return(splits)
}

#' @importFrom utils combn
#' @importFrom magrittr set_colnames
#' @importFrom splitTools partition
extensive_cv <- function(data, n_folds = 3, strata = NULL) {
  folds <- draw_folds(data = data, n_folds = n_folds, strata = strata)

  idx_fold <- do.call(c, lapply(1:(n_folds - 1), \(x){
    utils::combn(1:n_folds, x, simplify = FALSE)
  })) %>%
    lapply(\(x) {
      list(train = x, test = setdiff(1:n_folds, x))
    })

  splits <- lapply(idx_fold, \(s){
    lapply(s, \(x){
      do.call(c, folds[x])
    })
  })

  ## add info_folds attribute:
  fi <- lapply(idx_fold, \(x){
    y <- rep("train", n_folds)
    y[x$test] <- "test"
    y
  })
  fi <- do.call(rbind, fi) %>%
    magrittr::set_colnames(paste0("fold_", LETTERS[1:n_folds]))
  info_folds <- cbind(idx_split = 1:length(idx_fold), as.data.frame(fi))

  attr(splits, "info_folds") <- info_folds

  return(splits)
}



draw_folds <- function(data,
                       n_folds,
                       strata = NULL,
                       fold_size = rep(1 / n_folds, n_folds)) {
  ## check args:
  stopifnot(is.data.frame(data))
  stopifnot(n_folds >= 2 & (n_folds %% 1 == 0))

  ## prepare data splitting:
  n_obs <- nrow(data)

  if (!is.null(strata)) {
    stopifnot(length(strata) == 1)
    stopifnot(strata %in% names(data))
    y <- data[, strata]
    type <- "stratified"
  } else {
    y <- 1:n_obs
    type <- "basic"
  }

  ## conduct data splitting:
  splitTools::partition(y = y, p = fold_size, type = type)
}



## TODO (result should be dict / data.table)
# rmlx_splitting_methods <- list() %>%
#   add_item(specify_splitting_method(name, synonym_of=NULL, fun, args=formals(fun))) # name = get_name()
