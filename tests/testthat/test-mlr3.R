test_that("mlr3", {
  set.seed(123)
  data <- generate_data(n_obs = 1000)


  spec <- specify_splitting_rule("cv", n_folds = 3) %>% nest()

  splits <-
    derive_splits(spec, data) %>%
    add_mlr3_vars()

  n_splits <- nrow(splits$info)
  n_models <- 5
  n_obs <- 50
  predictions <- replicate(
    n_obs,
    expand.grid(
      idx_model = 1:n_models,
      idx_split = 1:n_splits
    ),
    simplify = FALSE
  ) %>%
    data.table::rbindlist() %>%
    dplyr::mutate(
      pred = runif(dplyr::n()),
      label = sample(0:1, size = dplyr::n(), replace = TRUE)
    )

  data_bm <-
    derive_metrics(predictions, splits, acc = mean(round(pred) == label)) %>%
    select_models("acc")

  testthat::expect_s3_class(data_bm, "data.table")
  testthat::expect_equal(nrow(data_bm), 3 * (3 * 2 + 1))
})
