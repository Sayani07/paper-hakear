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
#devtools::install_github("vigou3/actuar")
library(actuar)
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
sim_dist = rep(distributional::dist_gumbel(0.5,2), nx)
q3 <-  plot_normmax_1var(sim_dist, nx) + 
                 ggtitle("Gumbel(0.5, 2)")


##----cauchy20
nx = 20
sim_dist = rep(distributional::dist_cauchy(0,0.5), nx)
q4 <-  plot_normmax_1var(sim_dist, nx) +
  ggtitle("Cauchy(0, 0.5)")


##----cauchy20



##----alldist
p2_20 <- p2 + ggtitle("N(5, 10)")
ggarrange(p2_20, q1, q2, q3, nrow = 2, ncol = 2)

