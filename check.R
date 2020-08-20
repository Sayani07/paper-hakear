##----load
library(tsibbledata)
library(ggplot2)
library(tsibble)
library(lvplot)
library(dplyr)
library(gravitas)
library(purrr)
library(magrittr)
library(distributional)
quantile_prob = seq(0.01, 0.99, 0.01)
make_sim_1var <- function(sim_dist,
                           #distribution
                          nx = 7,
                          #number of levels/categories,
                          nobs_sample = 500,
                          # number of observations in each sample                
                          nsim = 200
                          # number of simulations to get histogram
){
  gen_index = rep(1:nx, each = nobs_sample)
  gen_dist = generate(sim_dist, nobs_sample) %>% unlist()
  gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from =  `...1`, values_from = `...2`)
  
}

plot_normmax_1var <- function(sim_dist,
                              nx = 7,
                              nobs_sample = 500,
                              nsim = 200)
{
  suppressMessages(
  plot_data <- (1:nsim) %>%
    purrr::map(function(i){
    data <- make_sim_1var(sim_dist,
                          nx,
                          nobs_sample,
                          nsim)
    gravitas::norm_jsd_maxpair(data)
    })
  )
    
   MMPD_obs <- unlist(plot_data) %>% tibble(.name_repair = "universal")
    names(MMPD_obs) <- "normalised_max"
    MMPD_obs %>% ggplot() + geom_histogram(aes(x = normalised_max)) 
}

nx_range = seq(10, 60, by = 10)
    
##----normalv11
set.seed(nx_range[1])
nx = nx_range[1]
sim_dist = rep(distributional::dist_normal(5, 10), nx)

p1 <- plot_normmax_1var(sim_dist,
                  nx, nobs_sample = 50) + 
  ggtitle(paste(nx, "levels"))


##----normalv12
set.seed(nx_range[2])
nx = nx_range[2]
sim_dist = rep(distributional::dist_normal(5, 10), nx)

p2 <- plot_normmax_1var(sim_dist,
                  nx, nobs_sample = 50) + 
  ggtitle(paste(nx, "levels"))



##----normalv13
set.seed(nx_range[3])
nx = nx_range[3]
sim_dist = rep(distributional::dist_normal(5, 10), nx)

p3 <- plot_normmax_1var(sim_dist,
                  nx, nobs_sample = 50) + 
  ggtitle(paste(nx, "levels"))

##----normalv14
set.seed(nx_range[4])
nx = nx_range[4]
sim_dist = rep(distributional::dist_normal(5, 10), nx)

p4 <- plot_normmax_1var(sim_dist,
                  nx, nobs_sample = 50) + 
  ggtitle(paste(nx, "levels"))



##---normalall

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)


##----exp20
nx = 20
sim_dist = rep(distributional::dist_exponential(0.5), nx)
q1 <- plot_normmax_1var(sim_dist,
                  nx) + 
  ggtitle("Exp(0.5)")

##----chisq20
nx = 20
sim_dist = rep(distributional::dist_chisq(5), nx)
q2 <- plot_normmax_1var(sim_dist,
                                 nx) + 
                 ggtitle("Chisq(5)")
                 
               

##----gumbel20
nx = 20
sim_dist = rep(distributional::dist_gumbel(c(0.5,2)), nx)
q3 <-  plot_normmax_1var(sim_dist,
                                 nx) + 
                 ggtitle("Gumbel(0.5, 2)")


##----weibull20
nx = 20
sim_dist = rep(distributional::dist_weibull(c(1.5,1)), nx)
q4 <-  plot_normmax_1var(sim_dist,
                  nx) +
  + 
  ggtitle("Weibull(1.5, 1)")


##---alldist

ggarrange(q1, q2, q3, q4, nrow = 2, ncol = 2)


##----expv1-30
set.seed(124)
nx = 30
sim_dist = rep(distributional::dist_exponential(5), nx)
plot_normmax_1var(sim_dist,
                  nx)

##----expv1-50
set.seed(124)
nx = 30
sim_dist = rep(distributional::dist_exponential(5), nx)
plot_normmax_1var(sim_dist,
                  nx)

##----normalv1-7
##----normal-ndist8
library(distributional)
ndist = 8
ntimes = 500

sim_dist1 = rep(distributional::dist_normal(5, 10), ndist)

gen_index = rep(1:ndist, each = ntimes)
gen_dist = generate(sim_dist1, ntimes) %>% unlist()
gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from =  `...1`, values_from = `...2`)

x = gen_data
y = quantile_extractx_n(x)
prob = seq(0.01, 0.99, 0.01)
z = JSdist_pair_matrix(y, dist_ordered = FALSE)
MMPD_obs <- unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD))

##----normal-ndist20
library(distributional)
ndist = 20
ntimes = 500

sim_dist1 = rep(distributional::dist_normal(5, 10), ndist)

gen_index = rep(1:ndist, each = ntimes)
gen_dist = generate(sim_dist1, ntimes) %>% unlist()
gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from =  `...1`, values_from = `...2`)

x = gen_data
y = quantile_extractx_n(x)
prob = seq(0.01, 0.99, 0.01)
z = JSdist_pair_matrix(y, dist_ordered = FALSE)
MMPD_obs <- unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD))


##----normal-ndist31
library(distributional)
ndist = 31
ntimes = 500

sim_dist1 = rep(distributional::dist_normal(5, 10), ndist)

gen_index = rep(1:ndist, each = ntimes)
gen_dist = generate(sim_dist1, ntimes) %>% unlist()
gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from =  `...1`, values_from = `...2`)

x = gen_data
y = quantile_extractx_n(x)
prob = seq(0.01, 0.99, 0.01)
z = JSdist_pair_matrix(y, dist_ordered = FALSE)
MMPD_obs <- unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: normal", " levels:", ndist, " sample-size:", ntimes))

##----normal-ndist100
library(distributional)
ndist = 100
ntimes = 5000

sim_dist1 = rep(distributional::dist_normal(5, 10), ndist)

gen_index = rep(1:ndist, each = ntimes)
gen_dist = generate(sim_dist1, ntimes) %>% unlist()
gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from =  `...1`, values_from = `...2`)

x = gen_data
y = quantile_extractx_n(x)
prob = seq(0.01, 0.99, 0.01)
z = JSdist_pair_matrix(y, dist_ordered = FALSE)
MMPD_obs <- unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: normal", " levels:", ndist, " sample-size:", ntimes))
####---- normal ends here


##----exponential-ndist8
library(distributional)
ndist = 8
ntimes = 500

sim_dist1 = rep(distributional::dist_exponential(rate = 2/3), ndist)

gen_index = rep(1:ndist, each = ntimes)
gen_dist = generate(sim_dist1, ntimes) %>% unlist()
gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from =  `...1`, values_from = `...2`)

x = gen_data
y = quantile_extractx_n(x)
prob = seq(0.01, 0.99, 0.01)
z = JSdist_pair_matrix(y, dist_ordered = FALSE)
MMPD_obs <- unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"

MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: exp(2/3)", " levels:", ndist, " sample-size:", ntimes))

##----exponential-ndist20
library(distributional)
ndist = 20
ntimes = 500

sim_dist1 = rep(distributional::dist_exponential(rate = 2/3), ndist)

gen_index = rep(1:ndist, each = ntimes)
gen_dist = generate(sim_dist1, ntimes) %>% unlist()
gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from =  `...1`, values_from = `...2`)

x = gen_data
y = quantile_extractx_n(x)
prob = seq(0.01, 0.99, 0.01)
z = JSdist_pair_matrix(y, dist_ordered = FALSE)
MMPD_obs <- unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"

MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: exp(2/3)", " levels:", ndist, " sample-size:", ntimes))

##----exponential-ndist31
library(distributional)
ndist = 31
ntimes = 500

sim_dist1 = rep(distributional::dist_exponential(rate = 2/3), ndist)

gen_index = rep(1:ndist, each = ntimes)
gen_dist = generate(sim_dist1, ntimes) %>% unlist()
gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from =  `...1`, values_from = `...2`)

x = gen_data
y = quantile_extractx_n(x)
prob = seq(0.01, 0.99, 0.01)
z = JSdist_pair_matrix(y, dist_ordered = FALSE)
MMPD_obs <- unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: exp(2/3)", " levels:", ndist, " sample-size:", ntimes))

##----exponential-ndist100
library(distributional)
ndist = 100
ntimes = 5000

sim_dist1 = rep(distributional::dist_exponential(rate = 2/3), ndist)

gen_index = rep(1:ndist, each = ntimes)
gen_dist = generate(sim_dist1, ntimes) %>% unlist()
gen_data <- bind_cols(gen_index, gen_dist) %>% tidyr::pivot_wider(names_from =  `...1`, values_from = `...2`)

x = gen_data
y = quantile_extractx_n(x)
prob = seq(0.01, 0.99, 0.01)
z = JSdist_pair_matrix(y, dist_ordered = FALSE)
MMPD_obs <- unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"

MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: exp(2/3)", " levels:", ndist, " sample-size:", ntimes))

## check1 done


# check2 start

# make data that can be fed to median_by_log

nfacet = 2
nx = 4
# number of samples generated
nsamp = 200
# ntimes: number of observations drawn from each sample

MMPD_obs <- (1:nsamp) %>% map(function(i){

  data_str <- tibble::tibble(facet_variable = "A",x_variable  = "B", facet_levels = nfacet, x_levels = nx)

  sim_dist1 <- rep(distributional::dist_exponential(rate = 2/3), nfacet*nx)

  data1 <- sim_distharmony1(data_str, sim_dist = sim_dist1) %>%
    select(-dist) %>%
    tidyr::pivot_wider(names_from = "Var2", values_from = sim_dist) %>% ungroup()

  MMPD1(data1)

})


MMPD_obs <- MMPD_obs %>% unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: exp(2/3)", " levels:", ndist, " sample-size:", ntimes))




# check2 start

# make data that can be fed to median_by_log

nfacet = 2
nx = 4
# number of samples generated
nsamp = 200
# ntimes: number of observations drawn from each sample

MMPD_obs <- (1:nsamp) %>% map(function(i){

  data_str <- tibble::tibble(facet_variable = "A",x_variable  = "B", facet_levels = nfacet, x_levels = nx)

  sim_dist1 <- rep(distributional::dist_exponential(rate = 2/3), nfacet*nx)

  data1 <- sim_distharmony1(data_str, sim_dist = sim_dist1) %>%
    select(-dist) %>%
    tidyr::pivot_wider(names_from = "Var2", values_from = sim_dist) %>% ungroup()

  MMPD1(data1)

})


MMPD_obs <- MMPD_obs %>% unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: exp(2/3)", " levels:", ndist, " sample-size:", ntimes))

## Exponential level up

# check2 start

# make data that can be fed to median_by_log

nfacet = 7
nx = 11
# number of samples generated
nsamp = 200
# ntimes: number of observations drawn from each sample

MMPD_obs <- (1:nsamp) %>% map(function(i){

  data_str <- tibble::tibble(facet_variable = "A",x_variable  = "B", facet_levels = nfacet, x_levels = nx)

  sim_dist1 <- rep(distributional::dist_exponential(rate = 2/3), nfacet*nx)

  data1 <- sim_distharmony1(data_str, sim_dist = sim_dist1) %>%
    select(-dist) %>%
    tidyr::pivot_wider(names_from = "Var2", values_from = sim_dist) %>% ungroup()

  MMPD1(data1)

})


MMPD_obs <- MMPD_obs %>% unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: exp(2/3)", " levels:", ndist, "iterarion:", nsamp, "nfacet:", nfacet, "nx:", nx))



# make data that can be fed to median_by_log

nfacet = 11
nx = 7
# number of samples generated
nsamp = 200
# ntimes: number of observations drawn from each sample

MMPD_obs <- (1:nsamp) %>% map(function(i){

  data_str <- tibble::tibble(facet_variable = "A",x_variable  = "B", facet_levels = nfacet, x_levels = nx)

  sim_dist1 <- rep(distributional::dist_exponential(rate = 2/3), nfacet*nx)

  data1 <- sim_distharmony1(data_str, sim_dist = sim_dist1) %>%
    select(-dist) %>%
    tidyr::pivot_wider(names_from = "Var2", values_from = sim_dist) %>% ungroup()

  MMPD1(data1)

})


MMPD_obs <- MMPD_obs %>% unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD_obs)) + ggtitle(paste("distribution: exp(2/3)", " levels:", ndist, "iterarion:", nsamp, "nfacet:", nfacet, "nx:", nx))

### more level up


nfacet = 31
nx = 24
# number of samples generated
nsamp = 200
# ntimes: number of observations drawn from each sample

MMPD_obs <- (1:nsamp) %>% map(function(i){

  data_str <- tibble::tibble(facet_variable = "A",x_variable  = "B", facet_levels = nfacet, x_levels = nx)

  sim_dist1 <- rep(distributional::dist_exponential(rate = 2/3), nfacet*nx)

  data1 <- sim_distharmony1(data_str, sim_dist = sim_dist1) %>%
    select(-dist) %>%
    tidyr::pivot_wider(names_from = "Var2", values_from = sim_dist) %>% ungroup()

  MMPD1(data1)

})


MMPD_obs <- MMPD_obs %>% unlist(z) %>% tibble(.name_repair = "universal")
names(MMPD_obs) <- "MMPD"
MMPD_obs %>% ggplot() + geom_histogram(aes(x = MMPD)) + ggtitle(paste("distribution: exp(2/3)", " levels:", ndist, "iterarion:", nsamp, "nfacet:", nfacet, "nx:", nx))

