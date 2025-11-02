
times <- 10L

nn_clinic <- 2^(1:8)
n_obs_per_clinic <- 100


spec <- mldesign::specify_estimand(
  sc(~ test$idx_clinic != train$idx_clinic)
)


results_raw <- lapply(nn_clinic[1:6], function(n_clinic){

  message(n_clinic)
  data <- expand.grid(idx_clinic=paste(1:n_clinic), idx_obs=1:n_obs_per_clinic) %>%
    mutate(idx_clinic = paste0(idx_clinic, "-", 1:100))

  microbenchmark::microbenchmark(
    derive_splits(spec, data), times=times, unit="seconds"
  )

})



results_rev <- lapply(nn_clinic, function(n_clinic){

  message(n_clinic)
  data <-expand.grid(idx_clinic=paste(1:n_clinic), idx_obs=1:n_obs_per_clinic)

  microbenchmark::microbenchmark(
    derive_splits(spec, data), times=times, unit="seconds"
  )

})


sapply(results_raw, \(x) summary(x) %>% '[['("median"))
sapply(results_rev, \(x) summary(x) %>% '[['("median"))


I1 <- 6
I2 <- 9

runtime_results_df <- data.frame(
  method = c(rep("naive", length(results_raw)),
             rep("improved", length(results_rev[1:I2]))),
  n_clinic = c(nn_clinic[1:I1], nn_clinic[1:I2]),
  n_obs = c(nn_clinic[1:I1], nn_clinic[1:I2])*n_obs_per_clinic,
  runtime_seconds = c(sapply(results_raw, \(x) summary(x) %>% '[['("median")),
                      sapply(results_rev[1:I2], \(x) summary(x) %>% '[['("median")))
)

runtime_results_df %>%
  ggplot(aes(n_obs, runtime_seconds, col=method)) +
  geom_point(size=4, shape=18) +
  geom_line(lwd=1.05) +
  coord_cartesian(ylim=c(0, 20)) +
  xlab("N  (100 observations per estimand-defining level)") +
  ylab("Elapsed time (seconds)") +
  labs(color = "Algorithm") +
  theme(axis.title = element_text(face="bold", size=18),
        axis.text = element_text(face="bold"),
        legend.title = element_text(face="bold", size=18),
        legend.text = element_text(face="bold"),
        legend.position = c(0.9, 0.9))


readr::write_csv(runtime_results_df,
                 file.path(out_dir, "2024-07-27_estiml_rwd_results_runtime.csv"))




# ---------------------------------------------------------------------------------------------
runtime_results_df <- readr::read_csv(file.path(out_dir, "2024-07-27_estiml_rwd_results_runtime.csv"))

library(dplyr)
library(ggplot2)

runtime_results_df %>%
  ggplot(aes(n_obs, runtime_seconds, col=method)) +
  geom_point(size=5, shape=18) +
  geom_line(lwd=1.1) +
  coord_cartesian(ylim=c(0, 20)) +
  xlab("N  (100 observations per estimand-defining level)") +
  ylab("Elapsed time (seconds)") +
  labs(color = "Algorithm") +
  theme_grey(base_size = 24) +
  theme(
    axis.title = element_text(face="bold"),
    axis.text = element_text(face="bold"),
    legend.title = element_text(face="bold"),
    legend.text = element_text(face="bold"),
    legend.position = c(0.9, 0.9))




