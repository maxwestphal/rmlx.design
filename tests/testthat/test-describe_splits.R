test_that("describe_splits works", {
  set.seed(123)
  data <- generate_data(n_obs = 2500)

  ## basic CV version:
  splits <- derive_splits(specify_splitting_rule("cv", n_folds=5), data)

  desc_splits <- describe_splits(splits, data,
                                 n_test_2 = quote(nrow(test)),
                                 prev_outcome_test = quote(mean(test$outcome==1)))


  expect_s3_class(desc_splits, "data.table")
  expect_true(all(c("n_test_2", "prev_outcome_test") %in% colnames(desc_splits)))
  expect_true(all(desc_splits$n_test == desc_splits$n_test_2))
  expect_true(var(desc_splits$prev_outcome_test) > 1e-5)


  ## stratified CV:
  splits <- derive_splits(specify_splitting_rule("cv", n_folds=5, strata="outcome"), data)

  desc_splits <- describe_splits(splits, data,
                                 n_test_2 = quote(nrow(test)),
                                 prev_outcome_test = quote(mean(test$outcome==1)))

  expect_s3_class(desc_splits, "data.table")
  expect_true(all(c("n_test_2", "prev_outcome_test") %in% colnames(desc_splits)))
  expect_true(all(desc_splits$n_test == desc_splits$n_test_2))
  expect_true(all(desc_splits$n_test == 500))
  expect_true(var(desc_splits$prev_outcome_test) < 1e-5)

})

