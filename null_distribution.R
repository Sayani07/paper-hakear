## ----load
library(tsibbledata)
library(ggplot2)
library(tsibble)
library(lvplot)
library(dplyr)
library(gravitas)
library(purrr)
library(magrittr)
library(distributional)
# devtools::install_github("vigou3/actuar")
library(actuar)
quantile_prob <- seq(0.01, 0.99, 0.01)
make_sim_1var <- function(sim_dist,
                          # distribution
                          nx = 7,
                          # number of levels/categories,
                          nobs_sample = 500,
                          # number of observations in each sample
                          nsim = 200
                          # number of simulations to get histogram
) {
  gen_index <- rep(1:nx, each = nobs_sample)
  gen_dist <- generate(sim_dist, nobs_sample) %>% unlist()
  gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from = `...1`, values_from = `...2`)
}

plot_normmax_1var <- function(sim_dist,
                              nx = 7,
                              nobs_sample = 500,
                              nsim = 200) {
  suppressMessages(
    plot_data <- (1:nsim) %>%
      purrr::map(function(i) {
        data <- make_sim_1var(
          sim_dist,
          nx,
          nobs_sample,
          nsim
        )
        gravitas::norm_jsd_maxpair(data)
      })
  )

  MMPD_obs <- unlist(plot_data) %>% tibble(.name_repair = "universal")
  names(MMPD_obs) <- "normalised_max"
  MMPD_obs %>% ggplot() +
    geom_histogram(aes(x = normalised_max))
}

hist_dist <- function(sim_dist,
                      nx = 7,
                      nobs_sample = 500,
                      nsim = 50, 
                      dist_ordered = TRUE) {
  suppressMessages(
    plot_data <- (1:nsim) %>%
      purrr::map(function(i) {
        data <- make_sim_1var(
          sim_dist,
          nx,
          nobs_sample,
          nsim
        )
        gravitas::dist_npair(data, quantile_prob, dist_ordered)
      })
  )
  
  MMPD_obs <- unlist(plot_data) %>% tibble(.name_repair = "universal")
  names(MMPD_obs) <- "pairwise_jsd"
  
  hist_dist <- MMPD_obs %>% ggplot(aes(x = pairwise_jsd, y = ..density..)) +
    geom_histogram() + 
    ggplot2::geom_histogram()+
    ggplot2::geom_density(colour = "red")
  
  p <- MMPD_obs %>% filter(!is.na(pairwise_jsd)) %>% ggplot(aes(sample = pairwise_jsd))
  p
  qq_dist <- p + stat_qq() + stat_qq_line()
  
  qq_dist
  #ggarrange(hist_dist, qq_dist, ncol = 1, nrow = 2)
}


nx_range <- seq(10, 60, by = 10)

## ----normalv11
set.seed(nx_range[1])
nx <- nx_range[1]
sim_dist <- rep(distributional::dist_normal(5, 10), nx)

p1 <- plot_normmax_1var(sim_dist,
  nx,
  nobs_sample = 50
) +
  ggtitle(paste(nx, "levels"))


## ----normalv12
set.seed(nx_range[2])
nx <- nx_range[2]
sim_dist <- rep(distributional::dist_normal(5, 10), nx)

p2 <- plot_normmax_1var(sim_dist,
  nx,
  nobs_sample = 50
) +
  ggtitle(paste(nx, "levels"))



## ----normalv13
set.seed(nx_range[3])
nx <- nx_range[3]
sim_dist <- rep(distributional::dist_normal(5, 10), nx)

p3 <- plot_normmax_1var(sim_dist,
  nx,
  nobs_sample = 50
) +
  ggtitle(paste(nx, "levels"))

## ----normalv14
set.seed(nx_range[4])
nx <- nx_range[4]
sim_dist <- rep(distributional::dist_normal(5, 10), nx)

p4 <- plot_normmax_1var(sim_dist,
  nx,
  nobs_sample = 50
) +
  ggtitle(paste(nx, "levels"))



## ---normalall

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)


## ----exp20
nx <- 20
sim_dist <- rep(distributional::dist_exponential(0.5), nx)
q1 <- plot_normmax_1var(
  sim_dist,
  nx
) +
  ggtitle("Exp(0.5)")

## ----chisq20
nx <- 20
sim_dist <- rep(distributional::dist_chisq(5), nx)
q2 <- plot_normmax_1var(
  sim_dist,
  nx
) +
  ggtitle("Chisq(5)")



## ----gumbel20
nx <- 20
sim_dist <- rep(distributional::dist_gumbel(0.5, 2), nx)
q3 <- plot_normmax_1var(sim_dist, nx) +
  ggtitle("Gumbel(0.5, 2)")


## ----cauchy20



## ----alldist
p2_1 <- p2 + ggtitle("N(5, 10)")
ggarrange(p2_1, q1, q2, q3, nrow = 2, ncol = 2)



make_sim_2var <- function(sim_dist,
                          # distribution
                          nx = 7,
                          # number of levels/categories of x-axis,
                          nfacet = 10,
                          # number of levels/categories of facet,
                          nobs_sample = 500,
                          # number of observations in each sample
                          nsim = 200
                          # number of simulations to get histogram
) {
  data_str <- tibble::tibble(facet_variable = "A", x_variable = "B", facet_levels = nfacet, x_levels = nx)

  sim_distharmony1(data_str,
    sim_dist = sim_dist
  ) %>%
    dplyr::select(-dist) %>%
    tidyr::pivot_wider(names_from = "Var2", values_from = sim_dist) %>%
    ungroup()
}

plot_normmax_2var <- function(sim_dist,
                              nx = 7,
                              nfacet = 10,
                              nobs_sample = 500,
                              nsim = 200) {
  suppressMessages(
    plot_data <- (1:nsim) %>%
      purrr::map(function(i) {
        data <- make_sim_2var(
          sim_dist,
          nx,
          nfacet,
          nobs_sample,
          nsim
        )
        gravitas::MMPD1(data)
      })
  )

  MMPD_obs <- unlist(plot_data) %>% tibble(.name_repair = "universal")
  names(MMPD_obs) <- "normalised_max"
  
  MMPD_obs %>% ggplot() +
    geom_histogram(aes(x = normalised_max)) +
    ggtitle(paste("x: ", nx, "levels", " facet: ", nfacet, "levels"))
}

nfacet_range <-  seq(10, 40, by = 10)
nsim = 200

## ----normalv21
set.seed(nfacet_range[1])
nx <- nx_range[2]
nfacet <- nfacet_range[1]

sim_dist <- rep(distributional::dist_exponential(rate = 2 / 3), nfacet * nx)


r1 <- plot_normmax_2var(sim_dist,
                        nx,
                        nfacet,
                        nobs_sample,
                        nsim) 

## ----normalv22
set.seed(nfacet_range[2])
nx <- nx_range[2]
nfacet <- nfacet_range[2]

sim_dist <- rep(distributional::dist_exponential(rate = 2 / 3), nfacet * nx)

r2 <- plot_normmax_2var(sim_dist,
                        nx,
                        nfacet,
                        nobs_sample,
                        nsim) 

## ----normalv23
set.seed(nfacet_range[3])
nx <- nx_range[2]
nfacet <- nfacet_range[3]

sim_dist <- rep(distributional::dist_exponential(rate = 2 / 3), nfacet * nx)


r3 <- plot_normmax_2var(sim_dist,
                        nx,
                        nfacet,
                        nobs_sample,
                        nsim) 

## ----normalv24
set.seed(nfacet_range[4])
nx <- nx_range[2]
nfacet <- nfacet_range[4]

sim_dist <- rep(distributional::dist_exponential(rate = 2 / 3), nfacet * nx)

r4 <- plot_normmax_2var(sim_dist,
                        nx,
                        nfacet,
                        nobs_sample,
                        nsim) 


## ---normalallvar2

ggarrange(r1, r2, r3, r4, nrow = 2, ncol = 2)

## ----distv11
set.seed(nx_range[1])
nx <- nx_range[1]
sim_dist <- rep(distributional::dist_normal(5, 10), nx)

s1 <-hist_dist(sim_dist,
               nx,
               nobs_sample = 50,
               dist_ordered = FALSE) + ggtitle("N(5, 10)")

## ----distv12
set.seed(nx_range[1])
nx <- nx_range[1]
sim_dist <- rep(distributional::dist_exponential(0.5), nx)

s2 <-hist_dist(sim_dist,
               nx,
               nobs_sample = 50,
               dist_ordered = FALSE) + ggtitle("Exp(0.5)")

## ----distv13
set.seed(nx_range[1])
nx <- nx_range[1]
sim_dist <- rep(distributional::dist_chisq(5), nx)

s3 <-hist_dist(sim_dist,
               nx,
               nobs_sample = 50,
               dist_ordered = FALSE) + ggtitle("Chi(5)")

## ----distv14
set.seed(nx_range[1])
nx <- nx_range[1]
sim_dist <- rep(distributional::dist_gumbel(0.5, 2), nx)
s4 <-hist_dist(sim_dist,
               nx,
               nobs_sample = 50,
               dist_ordered = FALSE) + ggtitle("Gumbel (0.5, 2)")


##alldist_distances

ggarrange(s1, s2, s3, s4, nrow = 2, ncol = 2) + coord_fixed(ratio=1)