### This code creates a table of data for different simulations scenarios ###
library(tidyverse)
# nperm and nsim can run parallelly
sim_table <- expand.grid(#data = c(99, 9999), # different data with same specifications
  #nperm = c(20, 100), # different permutations to compute mean and sd
  #nobs_each_comb  = c(10, 40, 500), # different number of observations for each combinations for each panel
  #nobs_diff_comb  = c("comb1", "comb2"), # different number of observations for different combinations for each panel
  #nsim = c(100, 500), # number of simulations of the entire exercise to compute distribution and CI of MMPD 
  #dist = c("norm", "non-norm"), # distributions from where data is generated
  nfacet = c(2, 3, 5, 7, 14, 20, 31, 50), # range of facet levels 
   #nfacet = c(2, 3),
   #nx = c(2, 3),
   nx = c(2, 3, 5, 7, 14, 20, 31, 50) # range of x levels
) %>% tibble()


#Export as .csv

write_csv(sim_table,'simulations/null/sim_table.csv')
