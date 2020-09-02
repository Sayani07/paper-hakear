##----load_lib
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(distributional)

##----shape_rate
shape1 <- 3
rate1 <- 0.5



##----create_simdata
# create data for each levels

create_simdata <- function(nx = 2,
                           # number of rvs to generate
                           nsim = 500,
                           # number of simulations
                           sim_dist = distributional::dist_gamma(
                             shape = shape1,
                             rate = rate1
                           ),
                           # distribution of each rv
                           create_fun = mean
                           # function used on the rvs
) {
  mean_dist <- (1:nsim) %>%
    purrr::map(function(i) {
      x <- sim_dist %>%
        distributional::generate(nx) %>%
        unlist()
      x %>% create_fun()
    })
}

##----create_nlevels
create_nlevels <- function(nlevels = seq(2, 10, 2),
                           nsim = 500,
                           sim_dist = distributional::dist_gamma(
                             shape = shape1,
                             rate = rate1
                           ),
                           # distr = "norm",
                           create_fun = mean) {
  data_orig <- nlevels %>%
    purrr::map_df(function(i) {
      create_datai <- create_simdata(nx = i, nsim, sim_dist, create_fun)
      tibble::tibble(ind = i, sim_data = create_datai)
    })
  data_orig %>% unnest(sim_data)
}

##----norm_mean
norm_mean <- function(x = NULL,
                      shape = shape1,
                      rate = rate1) {
  meanx <- shape / rate
  sdx <- shape / (rate^2)
  (mean(x) - meanx) / sdx
}



##----norm_max
# normalised maximum of a vector
norm_max <- function(x = NULL,
                     shape = shape1,
                     rate = rate1) {
  nx <- length(x)
  p <- 1 - (1 / nx)
  meanx <- shape / rate
  sdx <- shape / (rate^2)
  # meanx = 5
  # sdx = 10
  #b <- stats::qnorm(p, meanx, sdx)
  #a <- 1 / (nx * stats::dnorm(b, meanx, sdx))
  k = stats::quantile(x, p, type = 8, na.rm = TRUE)
  #(max(x) - a) / b
  max(x)/k
}


##----create_scenarios
set.seed(12345)
data_all_mean <- create_nlevels(nlevels = seq(2, 50, 1), create_fun = mean)

set.seed(12345)
data_all_max <- create_nlevels(
  nlevels = seq(2, 50, 1),
  create_fun = max
)

set.seed(12345)
data_all_norm_mean <- create_nlevels(
  nlevels = seq(2, 50, 1),
  create_fun = norm_mean
)

set.seed(12345)
data_all_norm_max <- create_nlevels(
  nlevels = seq(2, 50, 1),
  create_fun = norm_max
)


##----mean_small
data_all_mean %>%
  filter(ind %in% seq(2, 10, 1)) %>%
  ggplot(aes(x = sim_data)) +
  geom_histogram() +
  facet_wrap(~ind)


##----mean_large
data_all_mean %>%
  filter(ind %in% seq(20, 45, 5)) %>%
  ggplot(aes(x = sim_data)) +
  geom_histogram() +
  facet_wrap(~ind)

##----norm_mean_small
data_all_norm_mean %>%
  filter(ind %in% seq(2, 10, 1)) %>%
  ggplot(aes(x = sim_data)) +
  geom_histogram() +
  facet_wrap(~ind)

##----norm_mean_large
data_all_norm_mean %>%
  filter(ind %in% seq(20, 45, 5)) %>%
  ggplot(aes(x = sim_data)) +
  geom_histogram() +
  facet_wrap(~ind)


##----max_small
data_all_max %>%
  filter(ind %in% seq(2, 10, 1)) %>%
  ggplot(aes(x = sim_data)) +
  geom_histogram() +
  facet_wrap(~ind)


##----max_large
data_all_max %>%
  filter(ind %in% seq(20, 45, 5)) %>%
  ggplot(aes(x = sim_data)) +
  geom_histogram() +
  facet_wrap(~ind)


##----norm_max_small
data_all_norm_max %>%
  filter(ind %in% seq(2, 10, 1)) %>%
  ggplot(aes(x = sim_data)) +
  geom_histogram() +
  facet_wrap(~ind)



##----norm_max_large
data_all_norm_max %>%
  filter(ind %in% seq(20, 45, 5)) %>%
  ggplot(aes(x = sim_data)) +
  geom_histogram() +
  facet_wrap(~ind)
