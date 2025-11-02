#' @name specify_design
#' @title Specify a simple or nested experimental design
#'
#' @param spec (rmlx_spec) \cr specify a simple (non-nested) design by providing
#' either an \code{rmlx_estimand} or \code{rmlx_splitting_rule}. To specify a nested design,
#' use \code{nest()}.
#'
#' @return (rmlx_design)
#'
#' @examples
#' e <- specify_estimand(specify_constraint(~ train$country != test$country))
#' s <- specify_splitting("cv", n_folds = 5)
#'
#' # simple, structured (estimand-based) design:
#' specify_design(e)
#'
#' # simple, unstructured (splitting-based) design:
#' specify_design(s)
#'
#' # nested, structured design (nested CV in this case):
#' specify_design(nest(s))
#'
#' # nested, hybrid design:
#' specify_design(nest(e, s))
#'
#' @importFrom checkmate assert_multi_class
#' @importFrom checkmate assert_character
#' @export

specify_design <- function(spec){

  checkmate::assert_class(spec, "rmlx_spec")

  ## case 1 - simple (non-nested) design:
  if(methods::is(spec, "rmlx_spec_simple")){
    return(define_design(name=get_name(spec), outer=spec, inner=NULL))
  }
  ## case 2 - nested design:
  if(methods::is(spec, "rmlx_spec_nested")){
    return(define_design(name=get_name(spec), outer=spec$outer, inner=spec$inner))
  }

}

get_name <- function(spec){
  spec$name
}

define_design <- function(name, outer, inner){
  checkmate::assert_character(name, min.chars = 2, null.ok = TRUE)
  checkmate::assert_class(outer, "rmlx_spec_simple", null.ok=FALSE)
  checkmate::assert_class(inner, "rmlx_spec_simple", null.ok=TRUE)

  list(name = name,
       spec = list(outer = outer,
                   inner = inner),
       structured = c(get_structured(outer),
                      get_structured(inner)),
       nested = !is.null(inner)) %>%
    add_class("rmlx_design")
}

get_structured <- function(spec){
  if(is.null(spec)){return(NA)}
  checkmate::test_class(spec, "rmlx_estimand")
}

#' @name nest
#' @title specify a nested data splitting scheme
#' @param outer (rmlx_spec_simple) \cr outer specification
#' @param inner (rmlx_spec_simple) \cr inner specification
#' @param name (character) \cr name of the nested specification
#'
#' @importFrom checkmate assert_multi_class
#' @importFrom checkmate assert_character
#' @return (rmlx_spec_nested)
#' @export
nest <- function(outer, inner=outer, name=get_default_name(outer, inner)){
  checkmate::assert_class(outer, "rmlx_spec_simple", null.ok=FALSE)
  checkmate::assert_class(inner, "rmlx_spec_simple", null.ok=FALSE)

  define_spec_nested(name=name, outer=outer, inner=inner)
}


# TESTING -------------------------------------------------------------------------------------
# data <- generate_data(100)
# spec <- nest(
#   specify_estimand("train$country != test$country"),
#   specify_splitting("cv", n_folds=3)
# )
# r <- derive_splits_nested(spec, data)
# r$info
# r$sets %>% length()
# r$sets %>% sapply(length)

# ---------------------------------------------------------------------------------------------

#' @importFrom data.table data.table
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom tidyr starts_with
#' @importFrom tidyr replace_na
derive_splits_nested <- function(nested, data) {
  spec <- nested # TODO: rename arg 'nested' to 'design' and replace all occurences...

  ## derive outer splits:
  splits_outer <- derive_splits(design = spec$outer, data = data)

  ## init algorithm:
  info <- list()
  sets <- list(train = list(), test = list())

  idx_split <- 1
  idx_train <- 1
  idx_test <- 1

  ## outer loop:
  for (idx_outer in 1:nrow(get_info(splits_outer))) {
    idx_outer_obs_train <- get_splits(splits_outer, idx_outer)$train
    idx_outer_obs_test <- get_splits(splits_outer, idx_outer)$test

    info_folds <- NULL

    # (1) add type='outer' split
    split_new <- list(train = idx_outer_obs_train, test = idx_outer_obs_test)
    sets$train[[idx_train]] <- split_new$train
    sets$test[[idx_test]] <- split_new$test
    info[[length(info) + 1]] <- get_split_info(
      split = split_new,
      idx_split, idx_outer, 0,
      type = "outer", type_test = "outer", type_train = "outer",
      idx_train, idx_test,
      info_folds = NULL
    )

    idx_split <- idx_split + 1
    idx_train <- idx_train + 1

    ## inner loop:

    ## derive inner splits:
    splits_inner <- derive_splits(spec$inner, data[idx_outer_obs_train, ])
    info_folds <- attr(splits_inner, "info_folds")

    delta_idx <- 1

    for (idx_inner in 1:nrow(get_info(splits_inner))) {
      idx_inner_obs_train_rel <- get_splits(splits_inner, idx_inner)$train
      idx_inner_obs_train <- idx_outer_obs_train[idx_inner_obs_train_rel]

      idx_inner_obs_test_rel <- get_splits(splits_inner, idx_inner)$test
      idx_inner_obs_test <- idx_outer_obs_train[idx_inner_obs_test_rel]

      # (2) add type='mixed' split:
      split_new <- list(train = idx_inner_obs_train, test = idx_outer_obs_test)
      sets$train[[idx_train]] <- split_new$train
      sets$test[[idx_test]] <- split_new$test
      info[[length(info) + 1]] <- get_split_info(
        split = split_new,
        idx_split, idx_outer, idx_inner,
        type = "mixed", type_test = "outer", type_train = "inner",
        idx_train, idx_test,
        info_folds = info_folds[idx_inner, -1]
      )
      idx_split <- idx_split + 1

      # (3) add type='inner' split:
      split_new <- list(train = idx_inner_obs_train, test = idx_inner_obs_test)
      sets$test[[idx_test + delta_idx]] <- split_new$test
      info[[length(info) + 1]] <- get_split_info(
        split = split_new,
        idx_split, idx_outer, idx_inner,
        type = "inner", type_test = "inner", type_train = "inner",
        idx_train, idx_test + delta_idx,
        info_folds = info_folds[idx_inner, -1]
      )

      idx_split <- idx_split + 1
      idx_train <- idx_train + 1
      delta_idx <- delta_idx + 1
    }

    idx_test <- idx_test + delta_idx
  }


  ## compile results:
  info <- data.table::rbindlist(info, fill = TRUE) %>%
    dplyr::mutate(dplyr::across(tidyr::starts_with("fold_"), \(x) tidyr::replace_na(x, replace = "train")))
  info$idx_test <- info$idx_test + length(sets$train)

  define_splits(info = info, sets = unlist(sets, FALSE, FALSE), splits = NULL) %>% return()
}


get_split_info <- function(split,
                           idx_split, idx_outer, idx_inner,
                           type, type_test, type_train,
                           idx_train, idx_test,
                           info_folds) {
  info <- data.frame(
    idx_split = idx_split,
    idx_outer = idx_outer,
    idx_inner = idx_inner,
    type = type,
    type_test = type_test,
    type_train = type_train,
    idx_train = idx_train,
    idx_test = idx_test,
    n_train = length(split$train),
    n_test = length(split$test)
  )

  if (!is.null(info_folds)) {
    stopifnot(nrow(info_folds) == 1)
    info <- cbind(info, info_folds)
  }

  return(info)
}
