#This script calculates raw mmpd for each simulation scenario
##Read Simulation Table

.libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(readr)
library(drake)
library(tidyverse)
library(hakear)

set.seed(9999)

nsim = 200
# change path while running it on HPC
# simtable<-read_csv(here::here('simulations/null/sim_table.csv'))

simtable<-read_csv('../sim_table.csv')

### Extract flags from simulation scenario

#scen<- 2 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this

simj<-simtable[scen,] #Extract row of table
nfacetj<-simj$nfacet # Which nfacet level
nxj<-simj$nx #Which nx level
wj <- simj$w

#create data for each row for null normal

sim_null_normal = function(nxj, nfacetj){
  rep(distributional::dist_normal(0, 1), 
      times = nxj*nfacetj)
}

sim_varx_normal = function(nx, 
                           nfacet, 
                           mean = 0, 
                           sd = 5,
                           w)
{
  rep(dist_normal((mean + seq(0, nx-1, by  = 1)*w), sd), nfacet)
}

sim_panel_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = 500, 
                    sim_dist = sim_varx_normal(nx = nxj, nfacet = nfacetj, w = wj)) %>% 
  unnest(c(data)) %>% ungroup() %>% 
  bind_cols(w = wj)

set.seed(1111)

raw_dist <- map(seq_len(nsim), function(i)
{
  new_sim_data = sample(sim_panel_data$sim_data, 
                        size = nrow(sim_panel_data))
  new_data = sim_panel_data %>% 
    select(-sim_data) %>% 
    mutate(sim_data = new_sim_data)
  
  # for creating one raw mmpd
  raw_mmpd = compute_pairwise_max(new_data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data) %>% 
    as_tibble() %>% mutate(perm_id = i)
  
}) %>% bind_rows()


if(!dir.exists("simulations/vary_x/null_design_quantrans/data-ind/wpd_N05"))
{
  dir.create("simulations/vary_x/null_design_quantrans/data-ind/wpd_N05")
}
saveRDS(raw_dist,
        paste0('../data-ind/wpd_N05/',
               nxj,'_',
               nfacetj,"_",
               wj, '_wpd.rds'))
#This script calculates raw mmpd for each simulation scenario
##Read Simulation Table

.libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(readr)
library(drake)
library(tidyverse)
library(hakear)

set.seed(9999)

nsim = 200
# change path while running it on HPC
# simtable<-read_csv(here::here('simulations/null/sim_table.csv'))

simtable<-read_csv('../sim_table.csv')

### Extract flags from simulation scenario

#scen<- 2 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this

simj<-simtable[scen,] #Extract row of table
nfacetj<-simj$nfacet # Which nfacet level
nxj<-simj$nx #Which nx level
wj <- simj$w

#create data for each row for null normal

sim_null_normal = function(nxj, nfacetj){
  rep(distributional::dist_normal(0, 1), 
      times = nxj*nfacetj)
}

sim_varx_normal = function(nx, 
                           nfacet, 
                           mean = 0, 
                           sd = 1,
                           w)
{
  rep(dist_normal((mean + seq(0, nx-1, by  = 1)*w), sd), nfacet)
}

sim_panel_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = 500, 
                    sim_dist = sim_varx_normal(nx = nxj, nfacet = nfacetj, w = wj)) %>% 
  unnest(c(data)) %>% ungroup() %>% 
  bind_cols(w = wj)

set.seed(1111)

raw_dist <- map(seq_len(nsim), function(i)
{
  new_sim_data = sample(sim_panel_data$sim_data, 
                        size = nrow(sim_panel_data))
  new_data = sim_panel_data %>% 
    select(-sim_data) %>% 
    mutate(sim_data = new_sim_data)
  
  # for creating one raw mmpd
  raw_mmpd = compute_pairwise_max(new_data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data) %>% 
    as_tibble() %>% mutate(perm_id = i)
  
}) %>% bind_rows()


if(!dir.exists("simulations/vary_x/null_design_quantrans/data-ind/wpd_N05"))
{
  dir.create("simulations/vary_x/null_design_quantrans/data-ind/wpd_N05")
}
saveRDS(raw_dist,
        paste0('../data-ind/wpd_N05/',
               nxj,'_',
               nfacetj,"_",
               wj, '_wpd.rds'))
