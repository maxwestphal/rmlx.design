# TODO: old tests, need to refactor parse_constraint first:

# test_that("parse_constraint: test", {
#
#   xx <- c(
#     "age(,>=,18) # ic_age [p]",
#     "(complete.cases) # requires_complete_data [a]"
#   )
#
#   data <- list(test = data.frame(age = 27, feature1="b", feature2=NA))
#
#   for(x in xx){
#     e <- parse_constraint(x, target="test")
#     e %>% testthat::expect_s3_class("rmlx_constraint")
#
#     result <- eval_expr(e$expr, data=data)
#     result %>% testthat::expect_type("logical")
#     result %>% testthat::expect_length(1)
#   }
#
# })
#
#
#
# test_that("parse_constraint: relation", {
#
#   xx <- c(
#     "clinic(!=)",
#     "clinic(==)",
#     "clinic(==)#clinic_same",
#     "clinic(==)#clinic_same[c]",
#     "year(-, ==, 1)",
#     "year(-, >=, 1)",
#     "year(-, <=, 2)",
#     "year(-, %in%, 1:2)",
#     "year(-; %in%; c(1, 3))"
#   )
#
#   data <- list(test = data.frame(clinic = letters[3],
#                                  year = 3),
#                train = data.frame(clinic = letters[1:5],
#                                   year = 1:5))
#
#   for(x in xx){
#     e <- parse_constraint(x, target="relation")
#     e %>% testthat::expect_s3_class("rmlx_constraint")
#
#     result <- eval_expr(e$expr, data=data)
#     result %>% testthat::expect_type("logical")
#     result %>% testthat::expect_length(nrow(data$train))
#   }
#
# })
#
#
#
# test_that("parse_constraint: train", {
#
#   xx <- c(
#     "(nrow, >=, 100) # enough_train_obs [a]",
#     "response(mean, >=, 0.1) # enought_train_cases [a])",
#     "response(mean, <=, 0.9) # enought_train_controls [a])"
#   )
#
#   data <- list(train = data.frame(response = c(rep(1, 10), rep(0, 90))))
#
#   for(x in xx){
#     e <- parse_constraint(x, target="train")
#     e %>% testthat::expect_s3_class("rmlx_constraint")
#
#     result <- eval_expr(e$expr, data=data)
#     result %>% testthat::expect_type("logical")
#     result %>% testthat::expect_length(1)
#   }
#
# })
