
# ISSUES (0.2.1) ------------------------------------------------------------------------------

# TODO: remove "TODO", remove commented out
# TODO: revise file names, function distribution across files

# TODO: check Rmd
# TODO: check examples
# TODO: check vignettes








# ISSUES (0.3.0) ------------------------------------------------------------------------------

# TODO: revise dictionary (include synonym), use data.table

# TODO: INCREASE VERSION

# TODO: compatibility with rsample, tidymodels, external data ... (besides mlr3)

# TODO: simplify (print of) splits$info for non-nested splits

# TODO: fewer issues, move some to 0.4.0

# TODO: pkgdown site

# TODO: HP TUNING
# TODO: STRATA FOR TEST SET (already working via strata = country (=estimand-defining vars))
#       -> better would be to allow explicit definition a la "strata='idx_outer'"
# https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html
# MLR3 feature

# TODO: specify_estimand: warning vs. error on EMPTY "relation" arg (empty rel = reproducibility),
#       -> we need random splitting then...
# TODO: better parsing of variable names, e.g. in case of "train$year)"
# TODO: splitting functions cv, holdout, etc. should they be in dictonary instead of "global" funs?

# TODO: generate_data: should be more flexible, i.e. more args (n_clinic, n_country, ...)
# TODO: generate_data: allow for between year, clinic, region etc. heterogeneity (SIMPLE model with couple of features)

# TODO: filter_splits rename old fun -> new fun: allow filtering by arbitrary constraints

# TODO: sort out filter_split, expand_split, subset_split (which should be exported)
# TODO: check that train set should be based on all points not only test_eligible



# TODO: add attributes to derive_split (like n_test_obs (included/excluded), n_train_sets)

# TODO: Double check default args for 'methods' e.g. specify_method("cv") throws error- expeccted?

# TODO: use data.table everywhere?!?

# TODO: re-organise/cleanup R folder
# TODO: re-organise/cleanup unit tests

# TODO: overhaul/optimize all function names

# TODO: train
# TODO: score/eval (grouped)
# -> see: https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html



# TODO: NEWS.md
# TODO: styler


# ISSUES (0.4.0) ------------------------------------------------------------------------------
# -> FOR CRAN release: documentation, tests

# TODO: besides

# TODO: vignette for data augmentation (idx_obs, idx_origin)

# TODO: refactor get_vars() to avoid purrr and stringi dependence
# TODO: special case: no vars at all
# case a: no constraints defined: expect length 0
# case b: only constraint(s) operating on complete data defined, want to continue computations...
#   -> return( data[,"obs_id", drop=FALSE] ) ???

# TODO: ??? plan for parse_constraint_old -> implement short form again later? -> old version when "::" prefix

# TODO: nested data splitting optimization: calc outer and inner splits independently, then build intersection

# TODO: construct_splits: optimize "which" expression -> train= (R@j[R@i == k-1]) +1
# TODO: data.table refactoring
# TODO: refactor with checkmate, styler

# TODO: examples for all exported functions
# TODO: more unit tests

# TODO:

# TODO: revision of "overview" vignette / UML diagram + clean up
# TODO: 2nd vignette: more complex example, including (synthetic) heterogeneous dataset and ML,
# different estimands
# - simple ML: glmnet::cv.glmnet() (as inner data splitting is not yet implemented)



# TODO eval_constraint
## TODO: check if index set (i,j) can be optimized,
## e.g. for SYMMETRIC, e.g. equal/unequal, <, >: only need to check i<=j

## TODO:
## (1) for "equal":
## ---> clear that i <- j <- 1:n; x=TRUE
## (2) for !=, <, > (in general: conditions known to be anti-symmetric, i.e. cond(i,j) => !cond(j,i))
## ---> i,j such that only comb i<j (or <= ???) are evaluated
## (3) for other (e.g. <=, >=)
## ---> similar to (2)
## (4) for unknown: do as already implemented


# ISSUES 0.5.0 (INITIAL CRAN RELEASE) ---------------------------------------------------------





# ISSUES (LATER) ------------------------------------------------------------------------------

# TODO: efficiency tests + main algo more efficient (Rcpp)
# - in build_splits while loop:
#   - at start of while loop: potentially abort, if all entries of L are FALSE at any step
#     (is this expensive to eval? control via arg to apply_design)
# TODO: derive_splits: implement equivalence classes, before relation step?!?

# TODO: random data splitting cv/holdout/bootstrap (common args, stratify, grouped...)
# TODO: nested cv, inner/outer splits
t
# TODO: splits[[3, 4]] index for nested cv
# TODO: summary and/or describe_split or similar
# --> get some description of the datasets, i.e. train_sets, test_sets and relations
#' @export
#' @importFrom utils str
summary.rmlx_estimand <- function(object, ...){
  utils::str(object, 1) # TODO: overview of number of constraints
}

#' @export
#' @importFrom utils str
summary.rmlx_splits <- function(object, ...){
  utils::str(object, 2) # TODO: overview of number of n_train/n_test
}
# TODO: time splicing, i.e. derive discrete time steps (per overall, per splits (defined so far))
# TODO: get_inner(_idx)/outer(_idx)






