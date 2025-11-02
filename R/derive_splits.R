#' Derive a data split for a given estimand.
#'
#' @param design (rmlx_design) \cr a \code{rmlx_design} object, i.e. a
#' \code{rmlx_estimand} object created via specify \code{\link{specify_estimand}} or a
#' \code{rmlx_splitting_rule} object created via specify \code{\link{specify_splitting_rule}}
#' @param data (data.frame) \cr the complete data set.
#' @return (rmlx_splits)
#' @export
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom methods is
#' @importFrom checkmate assert_class
derive_splits <- function(design, data) {

  design <- as_design(design)

  ## argument checks:
  checkmate::assert_class(design, "rmlx_design")
  checkmate::assert_class(data, "data.frame")

  if (!is_nested(design) & is_structured(design)[1]) {
    return(derive_splits_structured(estimand = design$spec$outer, data = data))
  }

  if (!is_nested(design) & !is_structured(design)[1]) {
    return(derive_splits_unstructured(method = design$spec$outer, data = data))
  }

  if (is_nested(design)) {
    return(derive_splits_nested(nested = design$spec, data = data))
  }
}



as_design <- function(spec){

  if(methods::is(spec, "rmlx_design")){
    return(spec)
  }

  specify_design(spec=spec)
}

is_nested <- function(design){
  design$nested
}
is_structured <- function(design){
  design$structured
}

derive_splits_structured <- function(estimand, data) {
  eligible_test <- NULL

  ## which variables are needed:
  vars_test <- get_vars(estimand, target = "test", obs_idx = "...idx")
  vars_relation <- get_vars(estimand, target = "relation", obs_idx = "...idx")
  vars_train <- get_vars(estimand, target = "train", obs_idx = "...idx")
  vars_comb <- unique(c(vars_test, vars_relation, vars_train))

  ## add idx variable:
  obs_idx <- "...idx"
  data <- data %>% dplyr::mutate(...idx = 1:dplyr::n())


  ## step 1 - find obs that are eligible for testing:
  data <- data %>% dplyr::mutate(
    eligible_test = verify_test(data = data, constraints = estimand$test, details = FALSE)
  )

  ## step 2 - for each test obs (equiv class), find training set via constraints:
  splits <- build_splits(
    data = data %>%
      dplyr::filter(eligible_test) %>%
      dplyr::select(dplyr::all_of(vars_relation)),
    constraints = estimand$relation,
    obs_idx = obs_idx
  )


  ## step 3 - for each training set, check training constraints:
  eligible_train <- verify_train(define_splits(info = NULL, sets = NULL, splits = splits),
    data = data,
    constraints = estimand$train
  )

  splits <- splits[eligible_train]

  ## step 4 - compile and return output:
  define_splits(info = NULL, sets = NULL, splits = splits) %>% return()
}








# Helper functions ----------------------------------------------------------------------------


verify_train <- function(splits,
                         data,
                         constraints,
                         details = FALSE) {
  eligible_train <- sapply(1:nrow(splits$info), function(idx) {
    verify_train_1(
      data_train = get_train_set(data = data, splits = splits, idx = idx),
      constraints = constraints,
      details = details
    )
  })

  return(eligible_train)
}

verify_train_1 <- function(data_train, constraints, details = FALSE) {
  verified <- sapply(constraints, function(x) eval_expr(x$expr, data = list(train = data_train)))

  if (!details) {
    verified <- all(verified)
  }

  return(verified)
}

verify_test <- function(data, constraints, details = FALSE) {
  sapply(1:nrow(data), function(i) {
    verify_test_1(
      instance = data[i, , drop = FALSE],
      constraints = constraints,
      details = details
    )
  })
}

verify_test_1 <- function(instance, constraints, details = FALSE) {
  verified <- sapply(constraints, function(x) eval_expr(x$expr, data = list(test = instance)))

  if (!details) {
    verified <- all(verified)
  }

  return(verified)
}




#' @importFrom purrr flatten_chr
#' @importFrom stringi stri_remove_empty
get_vars <- function(estimand, target = c("test", "relation", "train"), obs_idx = "") {
  target <- match.arg(target)

  vars <- estimand[[target]] %>%
    lapply("[[", "vars") %>%
    unname() %>%
    append(obs_idx, after = 0) %>%
    purrr::flatten_chr() %>%
    stringi::stri_remove_empty() %>%
    unique()

  return(vars)
}
