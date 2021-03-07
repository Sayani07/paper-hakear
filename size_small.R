# This constitutes the simulation for checking if harmonies are being ranked properly
# 
# Observations are generated from a N(0,1) distribution for each combination of $nx$ and $nfacet$ from the following sets: $nx = \{3, 7, 14\}$ and $nfacet = \{2, 9, 10\}$. The panel $(3, 2), (7,9), (14, 10)$ are considered to have design $D_{null}$. The panels $(7, 2), (14, 9)$ have design of the form $D_{var_f}$. $(14, 2), (3, 10)$ have design of the form $D_{var_x}$ and the rest are under $D_{var_{null}}$. We generate only one data set for which all these designs were simulated and consider this as the original data set. We generate $200$ repetitions of this experiment with different seeds and compute the proportion of times a panel is rejected when it is under $D_{null}$. We also compute the proportion of times a panel is rejected when it actually belongs to a non-null design. The first proportion is desired to be as small as possible and a higher value of the later is expected. Also, these would constitute to be the estimated size and power of the test.

library(readr)
library(dplyr)
library(hakear)
library(distributional)
library(gravitas)
library(here)
library(tidyr)
library(tidyverse)
library(parallel)

# set values of parameters
sample_seed = seq(1000, 2999, by = 10) # number of  sample to compute p-value
nperm = 200 # number of permutations to compute wpd threshold
nx_range <- c(3, 7, 14)
nfacet_range <- c(2, 9, 10)
design_range <- c("null", "var_f", "var_x","var_all", "null", "var_f", "var_x","var_all", "null")

# simulation table for which threshold is computed

sim_table <- expand.grid(nx = nx_range,
                         nfacet = nfacet_range
) %>%
  tibble()

simtable <- sim_table %>%
  bind_cols(design = design_range)


# define functions for different designs
sim_varx_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w), sd), nfacet)
}

sim_varf_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w), sd), each = nx)
}

sim_varall_normal <- function(nx, nfacet, mean, sd, w) {
  dist_normal((mean + seq(0,
                          (nx *
                             nfacet - 1),
                          by = 1
  ) * w), sd)
}

all_seed <- mclapply(sample_seed, function(i){
  
  set.seed(i)

# sim_orig is being generated properly
  
sim_orig <- lapply(seq_len(nrow(simtable)), function(x){
  
  simx <- simtable[x, ]
  

if(simx$design =="null")
{
  data <- sim_panel(
    nx = simx$nx,
    nfacet = simx$nfacet,
    ntimes = 500,
    sim_dist = distributional
    ::dist_normal(0, 1)
  ) %>% unnest(c(data))
}

else if (simx$design=="var_f")
{
  data <-  sim_panel(
    nx = simx$nx, 
    nfacet = simx$nfacet,
    ntimes = 500,
    sim_dist = sim_varf_normal(simx$nx,
                               simx$nfacet, 0, 1, 10)
  ) %>% unnest(data)
}

else if (simx$design=="var_x")
{
  data <- sim_panel(
    nx = simx$nx, 
    nfacet = simx$nfacet,
    ntimes = 500,
    sim_dist = sim_varx_normal(simx$nx, 
                               simx$nfacet, 0, 1, 10)
  ) %>% unnest(data)
  
}

else(simx$design == "var_all")
{
  data <- sim_panel(
    nx = simx$nx, nfacet = simx$nfacet,
    ntimes = 500,
    sim_dist = sim_varall_normal(simx$nx,
                                 simx$nfacet,
                                 0, 1, 10)
  ) %>% unnest(data)
}
}) %>%
  bind_rows(.id = "harmony_id") 

# <sim_orig ends>


sim_split <- sim_orig %>% 
  ungroup() %>% 
  group_split(harmony_id)



wpd_perm1 <- lapply(sim_split, function(x){
  if(x$nx<=5 & x$nfacet<=5){
    compute_pairwise_norm(x, 
                          gran_x = "id_x",
                          gran_facet = "id_facet",
                          response = sim_data)
  }
  else{
    compute_pairwise_norm_scalar(x, 
                                 gran_x = "id_x",
                                 gran_facet = "id_facet",
                                 response = sim_data)
  }
  }
  )

simtable_wpd_orig <- simtable %>% bind_cols(wpd = unlist(wpd_perm1))

#how many permutations for computing threshold: nperm
wpd_sample <- lapply(seq_len(nperm), function(x){
  
  sample_data <- sample(sim_orig$sim_data, size =  nrow(sim_orig))
  
  sample_data <- sim_orig %>% 
    select(-sim_data) %>% 
    ungroup() %>% 
    mutate(sim_data = sample_data)
  
  sample_split <- sample_data %>% 
    ungroup() %>% 
    group_split(harmony_id)


  wpd_perm <- lapply(sim_split, function(x){
    if(x$nx<=5 & x$nfacet<=5){
      compute_pairwise_norm(x, 
                                   gran_x = "id_x",
                                   gran_facet = "id_facet",
                                   response = sim_data)
    }
    else{
    compute_pairwise_norm_scalar(x, 
                                 gran_x = "id_x",
                                 gran_facet = "id_facet",
                                 response = sim_data)
    }
    }
  )
})

  # find threshold
each_seed <- wpd_sample %>% unlist()
  

    threshold99 = quantile(each_seed, probs = 0.99)
    threshold95 = quantile(each_seed, probs = 0.95)
    threshold90 = quantile(each_seed, probs = 0.90)
  
  # conduct test
  
    simtable_wpd_orig %>% 
    mutate(select99 = if_else(wpd>threshold99, "yes", "no"),
           select95 = if_else(wpd>threshold95, "yes", "no"),
           select90 = if_else(wpd>threshold90, "yes", "no"))
}) %>%
  bind_rows(.id = "seed_id")


saveRDS(all_seed,
        paste0('simulations/supplementary/test/wpd_N01/many_harmony_threshold.rds'))


# lensim <- length(sim_orig)
# 
# wpd_orig = compute_pairwise_norm_scalar(sim_orig, 
#                                         gran_x = "id_x",
#                                         gran_facet = "id_facet",
#                                         response = sim_data)
# 
# wpd_sample <- lapply(seq_len(nperm), function(x){
#   
#   sample_data <- sample(sim_orig, size =  lensim)
#   
#   sim_panel_sample <- bind_cols(id_x = id_x, 
#                                 id_facet = id_facet, 
#                                 sim_data = sample_data)
#   
#   wpd =  compute_pairwise_norm_scalar(sim_panel_sample, 
#                                       gran_x = "id_x",
#                                       gran_facet = "id_facet",
#                                       response = sim_data) %>% 
#     as_tibble() %>% mutate(perm_id = x)
#   
# }) %>% bind_rows()
# 
# wpd_all <- wpd_orig %>% as_tibble() %>% 
#   mutate(perm_id = 0) %>% 
#   bind_rows(wpd_sample) %>% 
#   bind_cols(nx = nxj, nfacet = nfacetj)
# 
# wpd_all
# }) %>%
#   bind_rows()
