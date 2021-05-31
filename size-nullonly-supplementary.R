# This script computes size of the test in our paper and shows it is close to 95%

library(readr)
library(dplyr)
library(distributional)
library(gravitas)
library(here)
library(purrr)
library(tidyverse)
library(purrr)
library(hakear)

#simtable<-read_csv(here::here('simulations/sim_table/sim_table.csv'))
simtable <-  expand.grid(x_levels = c(3, 7, 14),
                         facet_levels = c(2, 9, 10)) %>%
  mutate(design = c("null", "null","null", "null", "null", "null", "null", "null", "null"))

sample_seed = seq(1000, 10999, by = 10)

# since it already run till 25 yesterday! Delete this line later
# sample_seed = sample_seed[26:1000]
npermj <- 200
ntimesj <- 500
nsampj <- 100
wj <- 2
# distj <- simj$dist
# seedj <- simj$seed
# typej <- simj$type

# different designs

sim_null <- function(nxj, nfacetj, mean, sd, w = 0){
  rep(distributional::dist_normal(mu = mean, sigma = sd),
      times = nxj*nfacetj)}


sim_harmony_sim <- map((30: length(sample_seed)),
                         function(seedj){
                           set.seed(sample_seed[seedj]) 
                           ## code for one harmony table starts
                           sim_data_all <- map(seq_len(nrow(simtable)),
                                               function(x){
                                                 y = simtable %>% magrittr::extract(x,)
                                                 
                                                 # assigning different rows for different design
                                                 
                                                 # setting value of parameters
                                                 designj = y$design
                                                 nfacetj<- y$facet_levels # Which nfacet level
                                                 nxj<- y$x_levels
                                                 
                                                 
                                                 if(designj=="null")
                                                   sim_function = sim_null
                                                 else if(designj=="varx")
                                                   sim_function =  sim_varx
                                                 else if(designj=="varf")
                                                   sim_function =  sim_varf
                                                 else
                                                   sim_function =  sim_varall
                                                 
                                                 # making data for each row using customized sim_function
                                                 
                                                 sim_panel_data =
                                                   hakear::sim_panel(nx = nxj,
                                                                     nfacet = nfacetj,
                                                                     ntimes = ntimesj,
                                                                     sim_dist = sim_function(nx = nxj,
                                                                                             nfacet = nfacetj,
                                                                                             mean = 0,
                                                                                             sd = 1,
                                                                                             w = wj)) %>%
                                                   unnest(c(data)) %>%
                                                   ungroup()
                                                 
                                                 
                                               })
                           
                           names(simtable) = c("x_levels", "facet_levels", "design")
                           
                           null_harmony_oneseed <- wpd_threshold(sim_data_all, harmony_tbl = simtable, create_harmony_data = FALSE, response = sim_data, nsamp = nsampj, nperm = npermj)
                           
                           ## code for one harmony table ends
                           
                           write_csv(null_harmony_oneseed, paste0("simulations/supplementary/test-hpc/data-ind/", "null-oneseed-id-", seedj, ".csv"))                        
                         }) %>% bind_rows(.id = "seed_id")


write_csv(sim_harmony_sim, "simulations/supplementary/test-hpc/data-agg/null-allseed.csv")

