#' Describe the derived data splits
#'
#' @param splits (rmlx_splits) \cr obtained via \code{\link{derive_splits}}
#' @param data (data.frame) \cr data that was used to derive the \code{splits}
#' @param ... (quote) \cr named \code{\link{quote}}s that describe properties of training or test sets.
#'
#' @returns data.table, an extended version of get_info(splits)
#' @export
describe_splits <- function(splits, data, ...){
  vars_new <- list(...)

  checkmate::assert_class(splits, "rmlx_splits")
  checkmate::assert_class(data, "data.frame")
  checkmate::assert_named(vars_new)

  if(length(vars_new)==0){
    return(get_info(splits))
  }

  info_new <-
    lapply(1:nrow(get_info(splits)), function(idx){
      lapply(vars_new, function(f){
        eval(f, envir=split_data(data, splits, idx, set="both", simplify=TRUE))
      }) %>% data.table::as.data.table()
    }) %>% data.table::rbindlist()

  return(cbind(get_info(splits), info_new))

}

