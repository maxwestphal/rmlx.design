valid_methods <- c("none", "holdout", "cv", "extensive_cv")

none <- function(data) {
  return(list())
}

holdout <- function(data, pr_test = 0.2, strata = NULL) {

  folds <- draw_folds(data = data,
                      n_folds = 2,
                      pr_folds=c(1-pr_test, pr_test),
                      strata = strata)

  names(folds) <- c("train", "test")

  return(list(folds))
}


#' @importFrom splitTools partition
cv <- function(data, n_folds = 5, pr_folds = NULL, strata = NULL) {
  if(is.null(pr_folds)){pr_folds <- rep(1/n_folds, n_folds)}
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
extensive_cv <- function(data, n_folds = 3, pr_folds = NULL, strata = NULL) {

  if(is.null(pr_folds)){pr_folds <- rep(1/n_folds, n_folds)}
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
                       pr_folds=rep(1/n_folds, n_folds),
                       strata = NULL,
                       n_min_stratum = n_folds) {

  ## check args:
  checkmate::assert_data_frame(data)
  checkmate::assert_integerish(n_folds, lower=2)
  checkmate::assert_numeric(pr_folds, lower=1/nrow(data), upper = 1-1/nrow(data))
  checkmate::assert_character(strata, null.ok = TRUE)
  checkmate::assert_integerish(n_min_stratum, lower=0, upper=nrow(data))

  ## normalize pr_folds im needed:
  pr_folds <- pr_folds/sum(pr_folds)

  # basic case:
  if (is.null(strata)) {
    return(draw_folds_simple(data, n_folds=n_folds, pr_folds=pr_folds))
  }

  # stratified case:
  checkmate::check_true(all(strata %in% colnames(data)))

  n_obs <- nrow(data)
  idx_obs <- 1:nrow(data)
  n <- paste(strata, collapse="|")
  v <- do.call(paste, c(lapply(strata, function(x) data[[x]]), sep="|"))
  u <- unique(v)
  v_table <- table(v)
  v_table_sel <- v_table[v_table < n_min_stratum]

  if(length(v_table_sel) > 0){
    stop(
      paste0("[rmlx] The following strata of the (combined) stratification variable ", n,
             " have too low number of observations (n < n_min_stratum):\n",
             paste0(names(v_table_sel), " (n=", unname(v_table_sel), ")\n", collapse = ""))
    )
  }

  # repeat steps above but with randomized column order for stratification variables:
  v <- do.call(paste, c(lapply(sample(strata), function(x) data[[x]]), sep="|"))
  u <- sample(unique(v))

  data_stratified <- sapply(u, function(x) idx_obs[v==x], simplify = FALSE)

  data_stratified_folds <-
    lapply(data_stratified, function(d){
      draw_folds_simple(data = d, n_folds=n_folds, pr_folds=pr_folds) %>%
        lapply(function(i){d[i]})
    })

  folds <- combine_folds(data_stratified_folds)

  return(folds)
}

combine_folds <- function(data_stratified_folds, folds=NULL){

  stopifnot(length(folds) > 0 | length(data_stratified_folds) > 0)

  ## recursion done when no entry left:
  if(length(data_stratified_folds) == 0){
    return(folds)
  }

  ## initial folds object:
  if(length(folds) == 0){
    folds <- replicate(length(data_stratified_folds[[1]]), integer(0))
  }

  ## recursion step:
  folds_add <- data_stratified_folds[[1]]
  folds_add_n <- sapply(folds_add, length)
  folds_add_sorted <- folds_add[order(folds_add_n)]

  folds_new <- mapply(c, folds, folds_add, SIMPLIFY =FALSE)
  folds_new_n <- sapply(folds_new, length)
  folds_new_sorted <- folds_new[order(folds_new_n, decreasing = TRUE)]
  names(folds_new_sorted) <- names(folds)

  combine_folds(data_stratified_folds[-1], folds_new_sorted)

}

draw_folds_simple <- function(data, n_folds, pr_folds=rep(1/n_folds, n_folds)){

  data <- as.data.frame(data)
  n_obs <- nrow(data)
  idx_obs_rdm <- sample(n_obs)
  idx_fold <- rep(1:(n_folds-1), times=round(pr_folds[-length(pr_folds)]*n_obs))
  idx_fold <- c(idx_fold, rep(n_folds, times=n_obs-length(idx_fold)))

  folds <- lapply(1:n_folds, function(i){
    idx_obs_rdm[idx_fold==i]
  })
  names(folds) = 1:n_folds

  return(folds)
}




