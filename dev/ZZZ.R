# summary -------------------------------------------------------------------------------------








define_ <- function(..., class, proc_names=c("none", "ignore", "fix")){

  stopifnot(methods::is(class, "character"))
  names <- match.arg(names)

  result <- list(...) %>% add_class(class)

  if(proc_names == "ignore"){
    names(result) <- NULL
  }

  if(proc_names == "fix"){
    result <- fix_names(result)
  }

  return(result)
}





# derive_splits_rdm ----------------------------------------------------------------------------







