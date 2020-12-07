#This script calculates raw and normalised mmpd for each simulation scenario nsim 100, nperm 100
##Read Simulation Table
.libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(parallel)
library(readr)
library(drake)
library(tidyverse)

nsim = 200
simtable<-read_csv('sim_table.csv')

### Extract flags from simulation scenario

#scen<- 3 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this

simj<-simtable[scen,] #Extract row of table
nfacetj<-simj$nfacet # Which nfacet level
nxj<-simj$nx #Which nx level

#create data for each row for null normal
set.seed(9999)
sim_null_normal = function(nxj, nfacetj){
  rep(distributional::dist_normal(5, 10), 
      times = nxj*nfacetj)
}

sim_panel_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = 500, 
                    sim_dist = sim_null_normal) %>% 
  unnest(c(data)) %>% ungroup()

#shuffling this data
set.seed(1111)

mmpd_dist <- map(seq_len(nsim), function(i)
{
  new_sim_data = sample(sim_panel_data$sim_data, 
                        size = nrow(sim_panel_data))
  new_data = sim_panel_data %>% 
    select(-sim_data) %>% 
    mutate(sim_data = new_sim_data)
  
  # for creating one norm mmpd

  norm_mmpd = hakear::compute_mmpd_norm(new_data, 
                                        gran_x = "id_x",
                                        gran_facet = "id_facet",
                                        response = sim_data,
                                        nperm = 100) %>% 
    as_tibble() %>% mutate(perm_id = i)
  
}) %>% bind_rows()

saveRDS(mmpd_dist, 
          paste0('../results/norm/',
                        nxj,'_',
                        nfacetj,'_dist.rds'))


