augment <- function(expr, ...){

  stopifnot(methods::is(expr, "character") | methods::is(expr, "formula") )

  expr <- to_character(expr)

  args <- list(...)

  ext <- sapply(1:length(args), function(i){
    paste0("#", names(args)[i], "=", args[[i]])
  }) %>% paste(collapse = " ")

  paste(expr, ext) %>% return()
}
