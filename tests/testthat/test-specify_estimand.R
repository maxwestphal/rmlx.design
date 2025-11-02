test_that("specify_estimand", {
  expect_error(
    specify_estimand("null_estimand")
  )

  specify_estimand(
    # test:
    "test$age >= 18",
    "complete.cases(test)",
    # relation:
    specify_constraint(~ test$year - train$year %in% 1:2, type = "context"),
    # train:
    cc("train$age >= 18"),
    name = "example_estimand"
  ) %>%
    expect_s3_class("rmlx_estimand")
})
