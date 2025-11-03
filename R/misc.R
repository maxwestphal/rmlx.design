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

get_name <- function(spec){
  spec$name
}

get_name_nested <- function(outer, inner) {
  paste0("{outer:'", get_name(outer), "'}{", "inner:'", get_name(inner), "'}")
}

get_structured <- function(spec){
  if(is.null(spec)){return(NA)}
  checkmate::test_class(spec, "rmlx_estimand")
}

is_nested <- function(design){
  design$nested
}

is_structured <- function(design){
  design$structured
}
