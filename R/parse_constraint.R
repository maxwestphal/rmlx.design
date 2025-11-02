#' @title Parse constraints
#'
#' @description
#' ...
#'
#'
#' @param x (character) \cr a character expression, can be augmented by further "#" parameters, see examples
#'
#' @return (rmlx_constraints)
#' @export
#'
#' @examples
#' parse_constraints("test$age >= 18 # name=ic_age #type=population")
parse_constraints <- function(x) {
  stopifnot(is.character(x))

  do.call(define_constraints, lapply(x, parse_constraint))
}

parse_constraint <- function(x) {
  parts <- strsplit(x, split = "#", fixed = TRUE)[[1]]

  expr <- parts[[1]]

  vv <- list()

  if (length(parts) > 1) {
    x2 <- parts[2:length(parts)]
    x2 <- x2[grepl("(.+)=(.+)", x2)]
    x2 <- gsub(" ", "", x2)
    if (length(x2) >= 1) {
      parts2 <- strsplit(x2, "=", fixed = TRUE)
      nn <- sapply(parts2, "[", 1)
      vv <- lapply(parts2, "[", -1) %>% lapply(paste, collapse = "")
      names(vv) <- nn
    }
  }

  args2 <- vv[c("name", "vars", "type", "target")]
  args2 <- args2[!is.na(names(args2))]
  args <- c(list(expr = expr), args2)

  do.call(specify_constraint, args = c(list(), args))
}
