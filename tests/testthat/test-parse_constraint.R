test_that("parse_constraint: test", {
  c1 <- parse_constraint("test$age >= 18 #name=ic_age, type=p")
  expect_s3_class(c1, "rmlx_constraint")
})


