get_all <- function(x, what, simplify = TRUE) {
  sapply(x, "[[", what, simplify = simplify)
}

add_class <- function(object, class_new = NULL) {
  class(object) <- append(class(object), class_new)
  return(object)
}

fix_names <- function(x, from = "name") {
  names(x) <- sapply(x, function(y) y[[from]])
  return(x)
}
