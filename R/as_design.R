as_design <- function(spec){

  # TODO - needed at all?
  specify_design(spec=spec)

}



get_default_name <- function(outer, inner) {
  # what about case where one name is NA/NULL?
  paste0("[outer='", outer$name, "'][", "inner='", inner$name, "']")
}
