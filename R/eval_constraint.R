#' @importFrom Matrix sparseMatrix
eval_constraint <- function(constraint, data_repr) {
  n <- nrow(data_repr)

  i <- rep(1:n, n)
  j <- rep(1:n, each = n)

  x <- eval_expr(
    expr = constraint$expr,
    data = list(test = data_repr[i, , drop = FALSE], train = data_repr[j, , drop = FALSE])
  )

  Matrix::sparseMatrix(
    i = i[x],
    j = j[x],
    dims = c(n, n),
    repr = "T"
  )
}
