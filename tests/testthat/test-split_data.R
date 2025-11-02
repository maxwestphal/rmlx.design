test_that("data splitting tests", {
  set.seed(123)
  data <- generate_data(2000)
  spec <- specify_splitting_rule("cv", n_folds = 5)

  splits <- derive_splits(spec, data)

  split_data(data, splits, idx = 1)
  split_data(data, splits, idx = 1:2)
  split_data(data, splits, idx = 1)

  split_data(data, splits, idx = 1, set = "train")
  split_data(data, splits, idx = 1, set = "test")

  get_data_split(data, splits, 1)
  get_data_splits(data, splits, 1:2)

  get_train_set(data, splits, 1)
  get_train_sets(data, splits, 1:2)

  get_test_set(data, splits, 1)
  get_test_sets(data, splits, 1:2)
})
