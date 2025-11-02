
#' @rdname constrain
#' @export
constrain_test <- function(expr,
                           name = to_character(expr),
                           vars = detect_vars(expr),
                           type = c(NA, "context", "population", "admissibility")){

  specify_constraint(expr=expr, name=name, vars=vars, type=type, target="test")

}

#' @rdname constrain
#' @export
constrain_relation <- function(expr,
                               name = to_character(expr),
                               vars = detect_vars(expr),
                               type = c(NA, "context", "population", "admissibility")){

  specify_constraint(expr=expr, name=name, vars=vars, type=type, target="relation")

}

#' @rdname constrain
#' @export
constrain_train <- function(expr,
                            name = to_character(expr),
                            vars = detect_vars(expr),
                            type = c(NA, "context", "population", "admissibility")){

  specify_constraint(expr=expr, name=name, vars=vars, type=type, target="train")

}
