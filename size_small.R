library(readr)
library(dplyr)
library(hakear)
library(distributional)
library(gravitas)
library(here)
library(tidyr)
sample_seed = seq(1000,2999, by = 10)
nperm = 200
simtable <- read_csv(here::here('simulations/sim_table/sim_table.csv'))

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

library(tidyverse)
# nperm and nsim can run parallelly
sim_table <- expand.grid(nx = c(3, 7, 14),
                         nfacet = c(2, 9, 10)
                         ) %>%
  tibble()

simtable <- sim_table %>% bind_cols(design = c("null", "var_f", "var_x","var_all", "null", "var_f", "var_x","var_all", "null"))

all_seed <- lapply(sample_seed, function(i){
  
  set.seed(i)


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

else(simx$design=="var_all")
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

lensim <- length(sim_orig)
                        
                        wpd_orig = compute_pairwise_norm_scalar(sim_panel_orig, 
                                                                gran_x = "id_x",
                                                                gran_facet = "id_facet",
                                                                response = sim_data)
                        
                        wpd_sample <- lapply(seq_len(nperm), function(x){
                          
                          sample_data <- sample(sim_orig, size =  lensim)
                          
                          sim_panel_sample <- bind_cols(id_x = id_x, 
                                                        id_facet = id_facet, 
                                                        sim_data = sample_data)
                          
                          wpd =  compute_pairwise_norm_scalar(sim_panel_sample, 
                                                              gran_x = "id_x",
                                                              gran_facet = "id_facet",
                                                              response = sim_data) %>% 
                            as_tibble() %>% mutate(perm_id = x)
                          
                        }) %>% bind_rows()
                        
                        wpd_all <- wpd_orig %>% as_tibble() %>% 
                          mutate(perm_id = 0) %>% 
                          bind_rows(wpd_sample) %>% 
                          bind_cols(nx = nxj, nfacet = nfacetj)
                        
                        wpd_all
                      }) %>%
    bind_rows()
  
  # find threshold
  
  threshold <- each_seed %>% mutate(
    threshold99 = quantile(each_seed$value, probs = 0.99),
    threshold95 = quantile(each_seed$value, probs = 0.95),
    threshold90 = quantile(each_seed$value, probs = 0.90))
  
  # conduct test
  
  threshold %>% 
    dplyr::filter(perm_id == 0) %>% 
    mutate(select99 = if_else(value>threshold99, "yes", "no"),
           select95 = if_else(value>threshold95, "yes", "no"),
           select90 = if_else(value>threshold90, "yes", "no"))
  
}) %>% bind_rows(.id = "seed_id")

saveRDS(all_seed,
        paste0('simulations/supplementary/test/wpd_N01/null_all_seed_threshold.rds'))