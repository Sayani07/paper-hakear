#This script calculates norm mmpd for each simulation scenario
##Read Simulation Table

.libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(readr)
library(drake)
library(tidyverse)
library(hakear)
library(parallel)


nsim = 200
# change path while running it on HPC
simtable<-read_csv(here::here('simulations/null/sim_table.csv'))
#simtable<-read_csv('sim_table.csv')

### Extract flags from simulation scenario
mclapply(seq_len(nrow(simtable)),
         function(j){
           scen<- j #If running within R uncomment this.  This will only run first scenario
           #scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this
           
           simj<-simtable[scen,] #Extract row of table
           nfacetj<-simj$nfacet # Which nfacet level
           nxj<-simj$nx #Which nx level
           
           #create data for each row for null normal
           set.seed(8899)
           sim_null_normal = function(nxj, nfacetj){
             rep(distributional::dist_normal(5, 1), 
                 times = nxj*nfacetj)
           }
           
           sim_panel_data = 
             hakear::sim_panel(nx = nxj,
                               nfacet = nfacetj, 
                               ntimes = 500, 
                               sim_dist = sim_null_normal) %>% 
             unnest(c(data)) %>% ungroup()
           
           set.seed(9988)
           
           norm_dist <- map(seq_len(nsim), function(i)
           {
             new_sim_data = sample(sim_panel_data$sim_data, 
                                   size = nrow(sim_panel_data))
             new_data = sim_panel_data %>% 
               select(-sim_data) %>% 
               mutate(sim_data = new_sim_data)
             
             # for creating one norm mmpd
             norm_mmpd = hakear::compute_wpd_norm(new_data, 
                                             gran_x = "id_x",
                                             gran_facet = "id_facet",
                                             response = sim_data) %>% 
               as_tibble() %>% mutate(perm_id = i)
             
           }) %>% bind_rows()
           
           saveRDS(norm_dist, paste0('simulations/results/norm/wpd_N51/',
                                    nxj,'_',
                                    nfacetj,'_wpd_N51.rds'))
         })

