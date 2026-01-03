test_that("testing derive_splits()", {
  set.seed(123)
  data <- generate_data(n_obs = 10000)


  # data splitting based on estimand ------------------------------------------------------------
  estimand <- specify_estimand(
    ## test:
    "test$age >= 18   #name=inclusion_criterion_age #type=p",
    "complete.cases(test)   #name=requires_complete_data #type=a",
    ## relation:
    "test$clinic == train$clinic   #name=trained_in_same_clinic #type=c",
    "test$country == train$country   #name=trained_in_same_country #type=c",
    "(test[['year']] - train[['year']]) %in% 1:2   #name=trained_on_data_from_last_two_years #type=c",
    ## train:
    "nrow(train) >= 10   #name=enough_train_obs #type=a",
    "mean(train[['outcome']]) >= 0.05   #name=enought_train_cases #type=a",
    "mean(train[['outcome']]) <= 0.95   #name=enought_train_controls #type=a"
  )
  splits_e <- derive_splits(estimand, data)

  expect_s3_class(estimand, "rmlx_estimand")
  expect_s3_class(splits_e, "rmlx_splits")
  expect_equal(nrow(get_info(splits_e)), 100)


  # data splitting based on (conventional) rule -----------------------------------------------
  rule <- specify_splitting_rule("holdout", pr_test=0.2)
  splits_r <- derive_splits(rule, data)

  expect_s3_class(rule, "rmlx_splitting_rule")
  expect_s3_class(splits_r, "rmlx_splits")
  expect_equal(nrow(get_info(splits_r)), 1)
  expect_equal(get_info(splits_r)$n_test, 2000)

  # data splitting based on (conventional) rule -----------------------------------------------
  rule <- specify_splitting_rule("cv", default = TRUE)
  splits_r <- derive_splits(rule, data)

  expect_s3_class(rule, "rmlx_splitting_rule")
  expect_s3_class(splits_r, "rmlx_splits")
  expect_equal(nrow(get_info(splits_r)), 5)

  # data splitting based on (conventional) rule -----------------------------------------------
  rule <- specify_splitting_rule("extensive_cv", default = TRUE)
  splits_r <- derive_splits(rule, data)

  expect_s3_class(rule, "rmlx_splitting_rule")
  expect_s3_class(splits_r, "rmlx_splits")
  expect_equal(nrow(get_info(splits_r)), 6)


  # data splitting based on (conventional) rule (stratified) ------------------------------------
  rule <- specify_splitting_rule("holdout", pr_test=0.2, strata=c("comorbidity"))
  splits_r <- derive_splits(rule, data)

  expect_s3_class(rule, "rmlx_splitting_rule")
  expect_s3_class(splits_r, "rmlx_splits")
  expect_equal(nrow(get_info(splits_r)), 1)

  # data splitting based on (conventional) rule (stratified) ------------------------------------
  rule <- specify_splitting_rule("cv", strata=c("comorbidity", "outcome"))
  splits_r <- derive_splits(rule, data)

  expect_s3_class(rule, "rmlx_splitting_rule")
  expect_s3_class(splits_r, "rmlx_splits")
  expect_equal(nrow(get_info(splits_r)), 5)

  # describe_splits(splits_r, data,
  #                 prev_como = quote(mean(test$comorbidity)),
  #                 prev_outc = quote(mean(test$outcome)) )

  # data splitting based on nested design -------------------------------------------------------
  # case 1: outer = inner (estimand)
  nested <- nest(specify_estimand("test$country != train$country"))
  splits_n <- derive_splits(nested, data)

  expect_s3_class(nested, "rmlx_spec_nested")

  expect_s3_class(splits_n, "rmlx_splits")
  expect_equal(nrow(get_info(splits_n)), 28)

  # data splitting based on nested design -------------------------------------------------------
  # case 2: outer = inner (rule)
  nested <- nest(specify_splitting_rule("cv", n_folds = 5))
  splits_n <- derive_splits(nested, data)

  expect_s3_class(nested, "rmlx_spec_nested")
  expect_s3_class(splits_n, "rmlx_splits")
  expect_equal(nrow(get_info(splits_n)), 55)

  # data splitting based on nested design -------------------------------------------------------
  # case 3: outer (estimand) != inner (estimand)
  nested <- nest(specify_estimand("test$country != train$country"),
                 specify_estimand("test$year - train$year == 1"))
  splits_n <- derive_splits(nested, data)

  expect_s3_class(nested, "rmlx_spec_nested")
  expect_s3_class(splits_n, "rmlx_splits")
  expect_equal(nrow(get_info(splits_n)), 44)

  # data splitting based on nested design -------------------------------------------------------
  # case 4: outer (estimand) != inner (rule)
  nested <- nest(specify_estimand("test$country != train$country"),
                 specify_splitting_rule("cv", n_folds = 5))
  splits_n <- derive_splits(nested, data)

  expect_s3_class(nested, "rmlx_spec_nested")
  expect_s3_class(splits_n, "rmlx_splits")
  expect_length(get_splits(splits_n), 44)
})
