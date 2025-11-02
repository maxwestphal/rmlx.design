#' #' Split data randomly
#' #'
#' #' @param data (data.frame) \cr  input data
#' #' @param method (character) \cr  currently only "cv" is supported
#' #' @param ... (any) \cr further args for specified \code{method}
#' #' @param args (list) \cr further args for specified \code{method}
#' #' @param seed (numeric) \cr an integer, to specify the random seed
#' #'
#' #' @return rmlx_splits
#' #' @export
#' derive_splits_rdm <- function(data,
#'                                  method = "cv",
#'                                  ...,
#'                                  args = list(),
#'                                  seed = NULL){
#'
#'   set.seed(seed)
#'
#'   do.call(what = paste0("derive_splits_", method),
#'           args = c(list(data=data), args, list(...)) ) %>%
#'     define_splits() %>%
#'     return()
#'
#' }




