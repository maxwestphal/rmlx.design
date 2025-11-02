#' @title Specify an Estimand
#'
#' @description define an estimand by a set of constraints.
#'
#' @param ... (character | rmlx_constraint | rmlx_constraints) \cr
#' Either an \code{mlguide_constraints} object or a list of \code{mlguide_constraint}s objects.
#' Alternatively, a character (vector) can be specified instead, which is passed to
#' \code{\link{parse_constraints}}.
#' @param constraints (rmlx_constraints) \cr a \code{mlguide_constraints} object (created with \code{\link{cc}}),
#' will be combined with \code{...} arguments (via\code{\link{cc}}).
#' @param name (character) \cr The label/name of the estimand (optional, default: NA).
#'
#' @return rmlx_estimand
#' @export
#' @examples
#' estimand <- specify_estimand(cc(
#'   "(test[['year']] - train[['year']]) %in% 1:2 #name=time_transferability #type=context",
#'   "test[['country']] != train[['country']] #name=region_transferability #type=context"
#' ))
specify_estimand <- function(...,
                             constraints = cc(),
                             name = NA) {
  stopifnot(is.character(name) | is.na(name))
  stopifnot(length(name) == 1)

  constraints <- c(list(...), constraints)

  ## parse constraints if needed:
  cl <- combine_constraints(constraints = constraints) %>% split_constraints(by = "target")

  if (length(cl$relation) == 0) {
    stop("[rmlx] no constraint with target 'relation' specified, see '?constrain'!")
  }

  ## compile and return output:
  define_estimand(name = name, test = cl$test, relation = cl$relation, train = cl$train)
}
