#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Pipe operator for concatenation
#'
#' @name %,%
#' @rdname pipe_concat
#' @export
#' @usage lhs \%,\% rhs
#' @param lhs (any) \cr an R object
#' @param rhs (any) \cr an R object such that \code{c(lhs, rhs)} is defined, usually of same class as \code{lhs}.
#' @return The result of calling `c(lhs, rhs)`.
`%,%` <- function(lhs, rhs) {
  return(c(lhs, rhs))
}
