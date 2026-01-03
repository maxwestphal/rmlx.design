test_that("specify_splitting_rule", {
  specify_splitting_rule("cv", default = TRUE) %>%
    expect_s3_class("rmlx_splitting_rule")

  specify_splitting_rule("cv", n_folds = 5, strata = "RESPONSE") %>%
    expect_s3_class("rmlx_splitting_rule")

  specify_splitting_rule("extensive_cv", default = TRUE) %>%
    expect_s3_class("rmlx_splitting_rule")

  specify_splitting_rule("extensive_cv", n_folds = 5, strata = "RESPONSE") %>%
    expect_s3_class("rmlx_splitting_rule")
})




