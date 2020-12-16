# starting with standard normal
# mu = 0, sd = 1, nx = 4, nfacet = 7

library(hakear)
library(tidyverse)
library(parallel)
library(distributional)

mean = 0
range_mean = c(0, 1, 3,  5, 7, 15)
sd = 1
range_sd = c(1, 3,  5, 7, 15, 20)
range_w =  seq(1, 20, 1)

# functions to generate distribution vector across combinations so that they are same across facet and different across x


# functions to generate distribution vector across combinations so that they are same across facet and different across x


# functions to generate distribution vector across combinations so that they are same across facet and different across x


sim_null_normal = function(nx, nfacet, mean, sd, w){
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
  data_null <- 
    data_w <- mclapply(range_w, function(w){
      data_y <- mclapply(range_mean, function(y){   
        # generate the data
        data <- sim_panel(nx = 3,
                          nfacet = 2,
                          ntimes = 500,
                          sim_dist = sim_null_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
          unnest(c(data))
        
        # compute wpd
        wpd <- compute_pairwise_max(data, 
                                    gran_x = "id_x",
                                    gran_facet = "id_facet",
                                    response = sim_data,
                                    lambda = 0.67)
        tibble(mean = y, wpd)
      }) %>% bind_rows() 
      bind_cols(omega = w, data_y)
    })  %>% bind_rows() 
  
  

set.seed(9999)
data_varf <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varf_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 


set.seed(9999)
data_varx <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varx_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 



set.seed(9999)
data_varall <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varall_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 


data_all <- bind_rows(data_null, data_varf, data_varx, data_varall, .id = "design")

data_all %>% 
  ggplot(aes(x = omega, y = wpd, color = design)) +
  geom_line() + facet_wrap(~mean, 
                           labeller = "label_both") + scale_color_brewer(palette = "Dark2") + 
  geom_point(alpha = 0.5) + 
  theme(legend.position = "bottom")


ggsave(here::here("simulations/result_report/data_varymean.png"))

######-----------------------------####


# for fixed mean and variable sd

mean = 0
range_mean = c(0, 1, 3,  5, 7, 15)
sd = 1
range_sd = c(1, 3,  5, 7, 15, 20)
range_w =  seq(1, 20, 1)

# functions to generate distribution vector across combinations so that they are same across facet and different across x


# functions to generate distribution vector across combinations so that they are same across facet and different across x


# functions to generate distribution vector across combinations so that they are same across facet and different across x


sim_null_normal = function(nx, nfacet, mean, sd, w){
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
data_null <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_sd, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_null_normal(nx = 3, nfacet = 2, mean = 0, sd = y, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 


mean = 0
range_mean = c(0, 1, 3,  5, 7, 15)
sd = 1
range_sd = c(1, 3,  5, 7, 15, 20)
range_w =  seq(1, 20, 1)

# functions to generate distribution vector across combinations so that they are same across facet and different across x


# functions to generate distribution vector across combinations so that they are same across facet and different across x


# functions to generate distribution vector across combinations so that they are same across facet and different across x


sim_null_normal = function(nx, nfacet, mean, sd, w){
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
data_null <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_null_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 



set.seed(9999)
data_varf <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varf_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 


set.seed(9999)
data_varx <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varx_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 



set.seed(9999)
data_varall <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varall_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 


data_all <- bind_rows(data_null, data_varf, data_varx, data_varall, .id = "design")

data_all %>% 
  ggplot(aes(x = omega, y = wpd, color = design)) +
  geom_line() + facet_wrap(~mean, 
                           labeller = "label_both") + scale_color_brewer(palette = "Dark2") + 
  geom_point(alpha = 0.5) + 
  theme(legend.position = "bottom")


ggsave(here::here("simulations/result_report/data_varymean.png"))



set.seed(9999)
data_varx <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varx_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 



set.seed(9999)
data_varall <- 
  data_w <- mclapply(range_w, function(w){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varall_normal(nx = 3, nfacet = 2, mean = y, sd = 1, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data,
                                  lambda = 0.67)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(omega = w, data_y)
  })  %>% bind_rows() 


data_all <- bind_rows(data_null, data_varf, data_varx, data_varall, .id = "design")

data_all %>% 
  ggplot(aes(x = omega, y = wpd, color = design)) +
  geom_line() + facet_wrap(~mean, 
                           labeller = "label_both") + scale_color_brewer(palette = "Dark2") + 
  geom_point(alpha = 0.5) + 
  theme(legend.position = "bottom")


ggsave(here::here("simulations/result_report/data_varymean.png"))
