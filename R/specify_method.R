derive_splits_unstructured <- function(method, data) {
  splits <- do.call(method$method, args = c(list(data = data), method$args))

  define_splits(info = NULL, sets = NULL, splits = splits) %>% return()
}



#' Specify a data splitting method
#'
#' @param method (character) \cr
#' @param ... (any) \cr named arguments to function name
#' @param default (logical) \cr Should default arguments be used? (default: FALSE)
#' @param name (character) \cr name of the data splitting method, equal to \code{method} by default.
#'
#' @return an object of class \code{rmlx_splitting_rule}
#' @export
#'
#' @examples
#' \dontrun{
#' specify_splitting_rule()
#' }
#' specify_splitting_rule("cv")
#' specify_splitting_rule("cv", default = TRUE)
#' specify_splitting_rule("cv", n_folds = 10)
specify_splitting_rule <- function(method, ..., default = FALSE, name = NA) {
  if (!(method %in% valid_methods)) {
    stop(paste0(
      "[rmlx] method needs to be one of the following: ",
      paste0("'", valid_methods, "'", collapse = "")
    ))
  }

  checkmate::assert_character(name, len=1, min.chars = 1)
  checkmate::assert_logical(default, len=1)

  args <- list(...)
  args_default <- formals(method) %>%
    "["(-1) %>%
    as.list()


  if (length(args) == 0 & !default) {
    message("[rmlx] please supply the following arguments for selected splitting method '", method, "':")
    message(str(args_default, 1))
    return(invisible(NULL))
  }

  if (length(args) == 0 & default) {
    message("[rmlx] using default arguments of selected splitting method '", method, "':")
    message(str(args_default, 1))
    args_final <- args_default
  }

  if (length(args) > 0 & !default) {
    if (!all(names(args) %in% names(args_default))) {
      message(paste0("[rmlx] some args supplied via '...' do not match args of method ", method, ":"))
      message(paste(
        names(args)[sapply(names(args), \(x) {
          !(x %in% names(args_default))
        })],
        collapse = ", "
      ))
    }

    args_final <- args_default
    args_valid <- names(args)[names(args) %in% names(args_default)]
    args_final[args_valid] <- args[args_valid]

    args_missing <- setdiff(names(args_default), names(args))

    if (length(args_missing) > 0) {
      message("[rmlx] using defaults for the following arguments of splitting method '", method, "':")
      message(str(args_default[args_missing], 1))
    }
  }

  if (length(args) > 0 & default) {
    stop("[rmlx] please either use default=TRUE or supply arguments via '...' (not both!)")
  }

  ## return:
  define_splitting_rule(name = name, method = method, args = args_final)
}
