estimand <- specify_estimand(
  name = "test_estimand",
  test =
    "age(,>=,18) # inclusion_criterion_age [p]" %,%
    "(complete.cases) # requires_complete_data [a]",
  relation =
    # "obs_id(!=) # inference_for_unseen_patient [c]" %,%
    "clinic(==) # trained_in_same_clinic [c]" %,%
    "country(==) # trained_in_same_country [c]" %,%
    "year(-, %in%, 1:2) # trained_on_data_from_last_two_years [c]",
  train =
    "(nrow, >=, 100) # enough_train_obs [a]"  %,%
    "response(mean, >=, 0.1) # enought_train_cases [a]" %,%
    "response(mean, <=, 0.9) # enought_train_controls [a]"
)

estimand




n_obs <- 20000
n_country <- 4
n_clinic_per_country <- 5
n_clinic <- n_country*n_clinic_per_country
n_year <- 5
prev <- 0.25

set.seed(123)

clinics <- letters[1:n_clinic]
clinic <- sample(clinics, n_obs, replace=TRUE)
table(clinic)

countries <- LETTERS[1:n_country]
clinic_to_country <- rep(1:n_country, times=n_clinic_per_country)
country <- sapply(clinic, function(x) countries[clinic_to_country[which(clinics == x)]]  )
table(country)


data <- data.frame(
  obs_id = sample(1:n_obs, replace=FALSE),
  clinic = clinic %>% as.factor(),
  country = country %>% as.factor(),
  year = sample(2000 + (1:n_year), n_obs, replace=TRUE),
  response = rbinom(n_obs, 1, prev),
  age = rnorm(n_obs, 60, 5)
)

dim(data)

summary(data)


