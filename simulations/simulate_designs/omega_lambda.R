# starting with standard normal
# mu = 0, sd = 1, nx = 4, nfacet = 7

library(hakear)
library(tidyverse)
library(parallel)
library(distributional)

mean = 0
range_mean = c(0, 1, 3,  5, 7, 15, 20)
sd = 1
range_sd = c(1, 3,  5, 7, 15, 20)
range_w =  c(1, 2, 3, 4, 5)
range_lambda = seq(0.1, 0.9, 0.05)

# functions to generate distribution vector across combinations so that they are same across facet and different across x


# functions to generate distribution vector across combinations so that they are same across facet and different across x


# functions to generate distribution vector across combinations so that they are same across facet and different across x

sim_varx_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal((mean + seq(0, nx-1, by  = 1)*w), sd), nfacet)
}

# functions to generate distribution vector across combinations so that they are same across x and different across facet

sim_varf_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal((mean + seq(0, nfacet-1, by  = 1)*w), sd), each = nx)
}

### changing mean with constant sd

set.seed(9999)
data_varx <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 4,
                        ntimes = 500,
                        sim_dist = sim_varx_normal(nx = 3, nfacet = 4, mean = y, sd = 2, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data, lambda = x)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>% bind_rows() 
# 
# data_all_varx <- data_varx %>% rename("wpd" = "value",
#                                       "omega" = "name")


set.seed(9999)
data_varf <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 4,
                        ntimes = 500,
                        sim_dist = sim_varf_normal(nx = 3, nfacet = 4, mean = y, sd = 2, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data, lambda = x)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>% bind_rows() 

# data_all_varf <- data_varf %>% rename("wpd" = "value",
#                                  "omega" = "name")
# 


data_all <- bind_rows(data_varx, data_varf, .id = "design") %>% mutate(design = if_else(design == "1", "vary_x", "vary_facet"))

data_all %>% 
  ggplot(aes(x = lambda, y = wpd, color = design)) +
  geom_line() + facet_grid(mean~omega, 
                           labeller = "label_both") + scale_color_brewer(palette = "Dark2") + 
  geom_point(alpha = 0.5) + 
theme(legend.position = "bottom")

ggsave(here::here("simulations/result_report/lambda_omega_mean.png"))

### changing sd with constant mean


# functions to generate distribution vector across combinations so that they are same across x and different across facet

sim_varx_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal(mean, (sd + seq(0, nx-1, by  = 1)*w)), nfacet)
}



sim_varf_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal(mean, (sd + seq(0, nfacet-1, by  = 1)*w)), each = nx)
}



set.seed(9999)
data_varx <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_sd, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 4,
                        ntimes = 500,
                        sim_dist = sim_varx_normal(nx = 3, nfacet = 4, mean = 15, sd = y, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data, 
                                  lambda = x)
      tibble(sd = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>% bind_rows() 
# 
# data_all_varx <- data_varx %>% rename("wpd" = "value",
#                                       "omega" = "name")

set.seed(9999)
data_varf <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_sd, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 4,
                        ntimes = 500,
                        sim_dist = sim_varf_normal(nx = 3, nfacet = 4, mean = 15, sd = y, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data, lambda = x)
      tibble(sd = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 
  
  bind_cols(lambda = x, data_w)
}) %>% bind_rows() 

# data_all_varf <- data_varf %>% rename("wpd" = "value",
#                                  "omega" = "name")
# 


data_all <- bind_rows(data_varx, data_varf, .id = "design") %>% mutate(design = if_else(design == "1", "vary_x", "vary_facet"))

data_all %>% 
  ggplot(aes(x = lambda, y = wpd, color = design)) +
  geom_line() + facet_grid(sd~omega, 
                           labeller = "label_both") + scale_color_brewer(palette = "Dark2") + 
  geom_point(alpha = 0.5) + 
  theme(legend.position = "bottom")


ggsave(here::here("simulations/result_report/lambda_omega_sd.png"))
