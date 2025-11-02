test_that("misc functions work", {
  # concat pipe:
  x <- 1 %,% 2 %,% 3
  expect_equal(x, 1:3)

  # print functions:
  print(sc("test$county != train$country"))
  print(cc(sc("test$county != train$country"), sc("test$county - train$year == 1")))
  print(specify_estimand(sc("test$county != train$country")))
  print(specify_splitting_rule("cv", n_folds = 5))
  print(specify_design(nest(specify_splitting_rule("cv", n_folds = 5))))
})
