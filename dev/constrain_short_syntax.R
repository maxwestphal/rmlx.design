# old short syntax ----------------------------------------------------------------------------
# adapt to new via prefix with "::"???


# estimand <- specify_estimand(
#   name = "test_estimand",
#   test =
#     "age(,>=,18) # inclusion_criterion_age [p]" %,%
#     "(complete.cases) # requires_complete_data [a]",
#   relation =
#     "clinic(==) # trained_in_same_clinic [c]" %,%
#     "country(==) # trained_in_same_country [c]" %,%
#     "year(-, %in%, 1:2) # trained_on_data_from_last_two_years [c]",
#   train =
#     "(nrow, >=, 100) # enough_train_obs [a]"  %,%
#     "response(mean, >=, 0.05) # enought_train_cases [a]" %,%
#     "response(mean, <=, 0.95) # enought_train_controls [a]"
# )


# parse_constraint ----------------------------------------------------------------------------

# TODO: refactor, only utilize if x has class "character"...
# two cases:
# (a) simple/long form: expr + hastag arguments as single character, e.g. "test$age >= 18 #target='test'"
# (b) complex/short form: see below

#' Parse constraints (OLD VERSION!!!)
#'
#' @param x (character) \cr ...
#'
#' @return mlguide_constraint
#' @export
#'
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split_1
#' @importFrom stringr str_extract
parse_constraint_OLD <- function(x){
  if(is.null(x)){
    return(NULL)
  }

  target <- match.arg(target)

  stopifnot(is.character(x))
  stopifnot(length(x) == 1)
  x <- stringr::str_replace_all(x, " ", "")

  ## decompose at '#'
  x_parts <- stringr::str_split_1(x, "#")
  stopifnot(length(x_parts) <= 2)
  x1 <- x_parts[1]
  x2 <- x_parts[2]

  ## parse part 1, format 'variable(fun, comp, value)':
  # x1_parts <- stringr::str_split_1(x1, "\\(")
  x1_parts <- stringr::str_split_fixed(x1, "\\(", 2)

  ## parse variable:
  variable <- x1_parts[1]

  ## parse fun, comp, value:
  # x12 <- stringr::str_extract(x1, "(?<=\\()(.*?)(?=\\))")
  x12 <-  substr(x1_parts[2], 1, nchar(x1_parts[2])-1)
  a <- parse_part12(x12)

  sep1 <- ifelse(variable=="", yes="", no="$")

  fun_args <- switch (target,
                      test = paste0("test", sep1, variable),
                      relation = paste0("test", sep1, variable, ",", paste0("train", sep1, variable)),
                      train = paste0("train", sep1, variable)
  )

  # build expression that can be evaluated later with eval_expr:
  sep2 <- ifelse(a$fun=="", yes="", no="'")

  expr <- paste0(sep2, a$fun, sep2, "(" , fun_args, ")", " %>% ", "'", a$comp, "'", "(", a$value, ")")

  ## return:
  do.call(
    what = paste0("constrain_", target),
    args = list(expr = expr,
                name = part2_to_name(x2, x1),
                vars = variable,
                type = part2_to_type(x2))
  ) %>%
    add_class("mldesign") %>%
    return()

}



# Helper functions ----------------------------------------------------------------------------
part2_to_name <- function(part2, part1){
  if(is.na(part2)){
    name <- as.character(part1)
  }
  if(!is.na(part2)){
    name <- stringr::str_split_1(part2, "\\[")[1]
  }
  return(name)
}

part2_to_type <- function(part2){
  type <- stringr::str_extract(part2, "(?<=\\[)(.*?)(?=\\])")

  if(length(type) > 1){
    stop("[rmlx] multiple types specifed via '[<type>]'?")
  }

  if(is.na(type)){
    type <- "c" # default type, if non is recognized
  }

  return(type)
}


parse_part12 <- function(part12){

  sep <- ifelse(grepl(";", part12, fixed=TRUE), ";", ",") # TODO: simplify/remove??

  parts <- stringr::str_split_1(part12, sep)

  stopifnot(length(parts) %in% 1:3)

  if(length(parts) == 1){
    fun <- parts[1]
    comp <- "=="
    value <- TRUE
  }

  if(length(parts) == 2){
    fun <- parts[1]
    comp <- "=="
    value <- parts[2]
  }

  if(length(parts) == 3){
    fun <- parts[1]
    comp <- parts[2]
    value <- parts[3]
  }

  return(list(fun = fun, comp=comp, value = value))

}
