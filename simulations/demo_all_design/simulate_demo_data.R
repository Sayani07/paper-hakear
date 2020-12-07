#This script calculates raw mmpd for each simulation scenario
##Read Simulation Table

.libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(readr)
library(drake)
library(tidyverse)
library(hakear)
library(tidyverse)
library(parallel)

# demo design data

set.seed(9999)

data_null <- hakear::sim_panel(nx = 2, nfacet = 3, ntimes = 500,
                sim_dist = rep(dist_normal(5, 5), 6)) %>% unnest(c(data))

data_varf <- sim_panel(nx = 2,
                            nfacet = 3,
                            ntimes = 500,
                            sim_dist = rep(dist_normal(seq(5, 15, 5), 5), each = 2))%>% unnest(c(data))

data_varx <- sim_panel(nx = 2,
                   nfacet = 3,
                   ntimes = 500,
                   sim_dist = rep(dist_normal(seq(5, 10, 5), 5), 3)) %>% unnest(c(data))

data_varall <- sim_panel(nx = 2,
                   nfacet = 3,
                   ntimes = 500,
                   sim_dist = dist_normal(seq(5,30, 5), 5))%>% unnest(c(data))


# visualize design data


p1 <- data_null  %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet, labeller = label_both) + geom_boxplot() + xlab("id_x")

p2 <- data_varf%>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~id_facet, labeller = label_both) + geom_boxplot() + xlab("id_x")

p3 <-  data_varx %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + 
  facet_wrap(~id_facet, labeller = label_both) + geom_boxplot() + xlab("id_x")

p4 <-  data_varall  %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet, labeller = label_both) + geom_boxplot() + xlab("id_x")


ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2,
                  common.legend = TRUE,
                  labels = c("a", "b", "c", "d")) 


# raw distance measure for demo design data

data_all <- bind_rows(data_null, data_varf, data_varx, data_varall, .id = 
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


lambda_grid %>% 
  mutate(design = factor(design, levels = c("D1", "D2", "D3", "D4"))) %>% 
  ggplot(aes(x=lambda, y = raw_mpd, colour = design)) +
  geom_line() + scale_x_continuous(breaks = seq(0.1, 0.9, 0.1))



