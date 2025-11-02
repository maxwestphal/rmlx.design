#' @export
#' @importFrom utils str
print.rmlx_constraint <- function(x, ...) {
  utils::str(x, 1)
}

#' @export
#' @importFrom utils str
print.rmlx_constraints <- function(x, ...) {
  utils::str(x, 2)
}

#' @export
#' @importFrom utils str
print.rmlx_estimand <- function(x, ...) {
  utils::str(x, 3)
}

#' @export
#' @importFrom utils str
print.rmlx_splitting <- function(x, ...) {
  utils::str(x, 1)
}

#' @export
#' @importFrom utils str
print.rmlx_design <- function(x, ...) {
  utils::str(x, 2)

  # TODO: suitable print versions for structured, unstructured, nested

}


#' @export
#' @importFrom utils str
print.rmlx_splits <- function(x, ...) {
  message("[rmlx] rmlx_splits object with the following properties:")
  print(x$info)
}
