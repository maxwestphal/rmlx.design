test_that("specify_constraint tests", {
  c1 <- specify_constraint("test$age >= 18", name = "ic_age")
  expect_s3_class(c1, "rmlx_constraint")
})
