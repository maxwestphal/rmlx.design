build_splits <- function(data, constraints, obs_idx) {
  ## check input:
  if (ncol(data) == 1) {
    stop("[rmlx] no relevant variable to split on...")
  }

  ## decompose obs_idx, and relevant data for splitting
  obs_idx_vals <- data[[obs_idx]]
  data <- data[, names(data) != obs_idx, drop = FALSE]

  ## compute equivalence classes (ec) of all required variables based on unique values:
  values <- do.call(paste, c(data, sep = "|"))
  equicl <- base::split(seq_len(nrow(data)), match(values, unique(values)))

  ## select ec representatives (first element)
  idx_repr <- sapply(equicl, "[", 1)

  ## reduce data to representative observations (one per ec):
  data_repr <- cbind(idx_repr = idx_repr, data[idx_repr, , drop = FALSE])

  ## evaluate constraints sequentially:
  ## (resulting logical matrix L indicates if if i-th val obs should have j-th sample in trn data)
  k <- 0
  L <- T
  while (k < length(constraints)) {
    k <- k + 1
    L <- L & eval_constraint(constraint = constraints[[k]], data_repr = data_repr)
  }

  ## build and return output:
  L %>%
    construct_splits() %>%
    # setNames(nm = values[idx_repr]) %>%
    filter_splits() %>%
    expand_splits(equicl = equicl, obs_idx_vals = obs_idx_vals) %>%
    return()
}

construct_splits <- function(L) {
  ## list of val sets:
  l <- do.call(paste, c(as.data.frame(as.matrix(L) * 1L), sep = ""))
  idx_list <- base::split(seq_len(nrow(L)), match(l, unique(l)))

  ## reduced representation:
  idx1 <- sapply(idx_list, "[", 1)
  R <- L[idx1, ]

  ## resulting split in standard format:
  lapply(seq_along(idx1), function(k) {
    list(
      train = (R@j[R@i == k - 1]) + 1,
      test = idx_list[[k]]
    )
  }) %>%
    return()
}

filter_splits <- function(split) {
  split[sapply(split, function(x) length(x$train) > 0)]
}

expand_splits <- function(split, equicl, obs_idx_vals) {
  lapply(split, function(s) {
    lapply(s, function(idx_repr) {
      obs_idx_vals[do.call(c, equicl[idx_repr])]
    })
  })
}
