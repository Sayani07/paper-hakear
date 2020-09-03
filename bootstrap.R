library(distributional)
library(tidymodels)

# Take samples of size n
n <- seq(2, 50, 1)
sim_data <- NULL
sim_dist <- dist_normal(5, 10)

# generate samples of size 1, 2, 3,... upto 50
# generate bootstrap (sample with replacement) from one drawn sample
set.seed(150)
data_n <- n %>% map_df(function(i) {
  sim_data <- generate(sim_dist, i)
  data_orig <- bind_cols(n = i, sim_data)
  boot_data <- bootstraps(data_orig, times = 2000, apparent = TRUE)
  data_bootstrap <- bind_cols(n = i, boot_data)
  data_bootstrap
})

# compute mean and sd of the bootstrap samples

mean_bootstrap <- function(split) {
  mean(analysis(split), na.rm = TRUE)
}



boot_models <-
  data_n %>%
  filter(n==2) %>% 
  mutate(
    mean = map(splits, mean_bootstrap)
    #sd = map(splits, sd)
  )
