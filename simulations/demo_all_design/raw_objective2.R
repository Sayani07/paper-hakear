#This script calculates raw mmpd for each simulation scenario
##Read Simulation Table
  .libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(readr)
library(tidyverse)
library(parallel)
library(hakear)

simtable<-read_csv('sim_table.csv')
# lambda <- seq(0.1, 0.9, 0.05)
### Extract flags from simulation scenario

scen<- 3 #If running within R uncomment this.  This will only run first scenario
#scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this

simj<-simtable[scen,] #Extract row of table
nfacetj<-simj$nfacet # Which nfacet level
nxj<-simj$nx #Which nx level
# designj <- simj$design

#create max pairwise data for each row : additive inverse lambda

set.seed(9999)
mean = 5
sd = 10

sim_function = function(nx, 
                        nfacet, 
                        #dist_type = 
                          #sim_dist = distributional::dist_normal(0,1), #standard normal
                          design = "null"){
  if(design == "null")
  {
    sim_dist = rep(distributional::dist_normal(mean, sd), 
        times = nx*nfacet)
  }
  if(design == "varf")
    
  {
    sim_dist = rep(dist_normal(seq(mean,mean*nfacet, 
                        by = mean), sd),
        each = nx)
    # par1 = distributional::dist_gamma(0,1)[[1]][1]
    # par2 =  distributional::dist_gamma(0,1)[[1]][2]
    
  }
  
  if(design == "varx"){
    sim_dist = rep(dist_normal(seq(mean,mean*nx, by = mean), sd), nfacet)
}
  if(design == "varall")
  {
    sim_dist = dist_normal(seq(mean,mean*nfacet*nx, by = mean), sd)
  }
  
  return(sim_dist)
}

sim_null_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = 500, 
                    sim_dist = sim_function(nxj, nfacetj, design = "null")) %>% 
  unnest(c(data)) %>% ungroup()


sim_varx_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = 500, 
                    sim_dist = sim_function(nxj, nfacetj, design = "varx")) %>% 
  unnest(c(data)) %>% ungroup()


sim_varf_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = 500, 
                    sim_dist = sim_function(nxj, nfacetj, design = "varf")) %>% 
  unnest(c(data)) %>% ungroup()


sim_varall_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = 500, 
                    sim_dist = sim_function(nxj, nfacetj, design = "varall")) %>% 
  unnest(c(data)) %>% ungroup()


  data_all <- bind_rows(sim_null_data, sim_varf_data, sim_varx_data, sim_varall_data, .id = 
                          "design") %>% mutate(design = paste0("D", design)) %>% ungroup()

  
lambda_grid <- mclapply(seq(0.1, 0.9, 0.05), function(x){
  
    D1 <- compute_pairwise_max(data_all %>% filter(design == "D1"), 
                               gran_x = "id_x",
                               gran_facet = "id_facet",
                               response = sim_data, lambda = x)
    
    D2 <- compute_pairwise_max(data_all %>% filter(design == "D2"), 
                               gran_x = "id_x",
                               gran_facet = "id_facet",
                               response = sim_data, lambda = x)
    
    D3 <- compute_pairwise_max(data_all %>% filter(design == "D3"), 
                               gran_x = "id_x",
                               gran_facet = "id_facet",
                               response = sim_data, lambda = x)
    
    D4 <- compute_pairwise_max(data_all %>% filter(design == "D4"), 
                               gran_x = "id_x",
                               gran_facet = "id_facet",
                               response = sim_data, lambda = x)
    tibble(lambda = x, D1 = D1, D2 = D2, D3 = D3, D4 = D4)
    
  }) %>% bind_rows() %>% pivot_longer(c(2:5),
                                      names_to = "design", 
                                      values_to = "raw_mpd")
  
saveRDS(lambda_grid, paste0('../results/demo_all_design/',
                         nxj,'_',
                         nfacetj,'_dist.rds'))

