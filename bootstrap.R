library(distributional)
library(tidymodels)

# Take samples of size n
n <- seq(2, 50, 1)
sim_data <- NULL
sim_dist <- dist_normal(5, 10)

# generate samples of size 1, 2, 3,... upto 50
# generate bootstrap (sample with replacement) from one drawn sample
data_n <- n %>% map_df(function(i) {
  set.seed(1000 + i)
  sim_data <- distributional::generate(sim_dist, i)
  data_orig <- bind_cols(n = i, unlist(sim_data))
  boot_data <- bootstraps(data_orig, times = 20000, apparent = TRUE)
  data_bootstrap <- bind_cols(n = i, boot_data)
  data_bootstrap
})

# mean and sd of extracted data from bootstrap data

mean_bootstrap <- function(split) {
  data = analysis(split)
  mean(data$`...2`, na.rm = TRUE)
}

sd_bootstrap <- function(split) {
  data = analysis(split)
  sd(data$`...2`, na.rm = TRUE)
}

max_bootstrap <- function(split) {
  data = analysis(split)
  max(data$`...2`, na.rm = TRUE)
}

# compute mean and sd of the bootstrap samples
boot_max <-
  data_n %>%
  group_by(n) %>% 
  mutate(
    sim_max = map(splits, max_bootstrap)
  ) %>% unnest(cols = sim_max)



boot_max %>%
  filter(n %in% 2:10) %>% 
  group_by(sim_max) %>% 
  ggplot() +
  geom_histogram(aes(x = sim_max)) +
  facet_wrap(~n)