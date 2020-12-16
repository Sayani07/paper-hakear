# starting with standard normal
# mu = 0, sd = 1, nx = 4, nfacet = 7
library(hakear)
library(tidyverse)
library(parallel)
library(distributional)

mean = 0
sd = 1
range_w =  c(1, 3, 5, 7, 9)
range_lambda = seq(0.1, 0.9, 0.05)

sim_null_normal= function(nx, nfacet, mean, sd, w = 0){
  rep(distributional::dist_normal(mean, sd), nx*nfacet)
}

sim_varx_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal((mean + seq(0, nx-1, by  = 1)*w), sd), nfacet)
}

# functions to generate distribution vector across combinations so that they are same across x and different across facet

sim_varf_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal((mean + seq(0, nfacet-1, by  = 1)*w), sd), each = nx)
}

sim_varall_normal = function(nx, nfacet, mean, sd, w)
{
  dist_normal((mean + seq(0, (nx*nfacet - 1), by  = 1)*w), sd)
}

### changing mean with constant sd

set.seed(9999)
data_null <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    # data_y <- mclapply(range_mean, function(y){   
    # generate the data
    data <- sim_panel(nx = 3,
                      nfacet = 2,
                      ntimes = 500,
                      sim_dist = sim_null_normal(nx = 3, nfacet = 2, mean = 0, sd = 1, w = w)) %>%
      unnest(c(data))
    
    # compute wpd
    wpd <- compute_pairwise_max(data, 
                                gran_x = "id_x",
                                gran_facet = "id_facet",
                                response = sim_data, lambda = x)
    bind_cols(omega = w, wpd = wpd)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>%
  bind_rows() 



set.seed(9999)
data_varx <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    # data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varx_normal(nx = 3, nfacet = 2, mean = 0, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data, lambda = x)
    bind_cols(omega = w, wpd = wpd)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>%
  bind_rows() 



set.seed(9999)
data_varf <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    # data_y <- mclapply(range_mean, function(y){   
    # generate the data
    data <- sim_panel(nx = 3,
                      nfacet = 2,
                      ntimes = 500,
                      sim_dist = sim_varf_normal(nx = 3, nfacet = 2, mean = 0, sd = 1, w = w)) %>%
      unnest(c(data))
    
    # compute wpd
    wpd <- compute_pairwise_max(data, 
                                gran_x = "id_x",
                                gran_facet = "id_facet",
                                response = sim_data, lambda = x)
    bind_cols(omega = w, wpd = wpd)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>%
  bind_rows() 

# varall

set.seed(9999)
data_varall <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    # data_y <- mclapply(range_mean, function(y){   
    # generate the data
    data <- sim_panel(nx = 3,
                      nfacet = 2,
                      ntimes = 500,
                      sim_dist = sim_varall_normal(nx = 3, nfacet = 2, mean = 0, sd = 1, w = w)) %>%
      unnest(c(data))
    
    # compute wpd
    wpd <- compute_pairwise_max(data, 
                                gran_x = "id_x",
                                gran_facet = "id_facet",
                                response = sim_data, lambda = x)
    bind_cols(omega = w, wpd = wpd)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>%
  bind_rows() 


# data_all_varf <- data_varf %>% rename("wpd" = "value",
#                                  "omega" = "name")
# 

data_all <- bind_rows(data_null,
                      data_varf, 
                      data_varx,
                      data_varall,
                      .id = "design") 

data_all %>% 
  ggplot(aes(x = lambda, y = wpd, color = design)) +
  geom_line() + facet_grid(~omega, 
                           labeller = "label_both") + scale_color_brewer(palette = "Dark2") + 
  geom_jitter(alpha = 0.5) + 
  theme(legend.position = "bottom") + scale_colour_manual(values = c("#CC79A7",  "#E69F00", "#56B4E9", "#0072B2"))


ggsave(here::here("simulations/result_report/wpd_alterate_designs.png"))
