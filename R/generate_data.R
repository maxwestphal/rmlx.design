#' Generate synthetic data for testing purposes
#'
#' @param n_obs (numeric) \cr number of observations
#'
#' @return (data.frame)
#' @export
#'
#' @examples
#' generate_data(200)
#' @importFrom stats rbinom
#' @importFrom stats rnorm
generate_data <- function(n_obs = 2000) {
  age <- comorbidity <- NULL

  n_country <- 4
  n_clinic_per_country <- 5
  n_clinic <- n_country * n_clinic_per_country
  n_year <- 6
  # prev <- 0.25

  clinics <- letters[1:n_clinic]
  clinic <- sample(clinics, n_obs, replace = TRUE)

  countries <- LETTERS[1:n_country]
  clinic_to_country <- rep(1:n_country, times = n_clinic_per_country)
  country <- sapply(clinic, function(x) countries[clinic_to_country[which(clinics == x)]])

  data.table(
    obs_id = sample(1:n_obs, replace = FALSE),
    clinic = clinic %>% as.factor(),
    country = country %>% as.factor(),
    year = sample(2000 + (1:n_year), n_obs, replace = TRUE)
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(age = stats::rnorm(1, 50, 5) %>% round(2)) %>%
    dplyr::mutate(comorbidity = stats::rbinom(1, 1, which(country == countries) / (n_country + 1))) %>%
    dplyr::mutate(outcome = stats::rbinom(1, 1, 1 / (1 + exp(5 - 0.1 * age - 1 * comorbidity - 0.1 * age * comorbidity)))) %>%
    dplyr::ungroup()
}
