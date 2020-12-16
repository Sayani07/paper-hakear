# starting with standard normal
# mu = 0, sd = 1, nx = 4, nfacet = 7

library(hakear)
library(tidyverse)
library(parallel)
library(distributional)

mean = 0
range_mean = c(0, 1, 3,  5, 7, 15, 20)
sd = 1
range_sd = c(1, 2, 3, 5, 7, 10)
w = 3
range_w =  c(1, 2, 3, 4, 5)
range_lambda = seq(0.1, 0.9, 0.05)
range_seed = seq(1000, 10900, by = 100) 

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

##---- constant omega, changing mean and sd

##---varyx design

data_all_null <- mclapply(range_seed, function(s){
  set.seed(s)
  data_null <- mclapply(range_lambda, function(x){
    data_w <- mclapply(range_sd, function(z){
      data_y <- mclapply(range_mean, function(y){   
        # generate the data
        data <- sim_panel(nx = 3,
                          nfacet = 2,
                          ntimes = 500,
                          sim_dist = sim_null_normal(nx = 3, nfacet = 2, mean = y, sd = z, w = w)) %>%
          unnest(c(data))
        
        # compute wpd
        wpd <- compute_pairwise_max(data, 
                                    gran_x = "id_x",
                                    gran_facet = "id_facet",
                                    response = sim_data, lambda = x)
        tibble(mean = y, wpd)
      }) %>% bind_rows() 
      bind_cols(sd = z, data_y)
    })  %>% bind_rows() 
  }) %>% bind_rows() 
  bind_cols(seed = s, data_null)
})




##---varyx design

data_all_varx <- mclapply(range_seed, function(s){
set.seed(s)
data_varx <- mclapply(range_lambda, function(x){
  data_w <- mclapply(range_sd, function(z){
    data_y <- mclapply(range_mean, function(y){   
      # generate the data
      data <- sim_panel(nx = 3,
                        nfacet = 2,
                        ntimes = 500,
                        sim_dist = sim_varx_normal(nx = 3, nfacet = 2, mean = y, sd = z, w = w)) %>%
        unnest(c(data))
      
      # compute wpd
      wpd <- compute_pairwise_max(data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data, lambda = x)
      tibble(mean = y, wpd)
    }) %>% bind_rows() 
    bind_cols(sd = z, data_y)
  })  %>% bind_rows() 
}) %>% bind_rows() 
 bind_cols(seed = s, data_varx)
 })


##---varyf design


data_all_varf <- mclapply(range_seed, function(s){
  set.seed(s)
  data_varf <- mclapply(range_lambda, function(x){
    data_w <- mclapply(range_sd, function(z){
      data_y <- mclapply(range_mean, function(y){   
        # generate the data
        data <- sim_panel(nx = 3,
                          nfacet = 2,
                          ntimes = 500,
                          sim_dist = sim_varf_normal(nx = 3, nfacet = 2, mean = y, sd = z, w = w)) %>%
          unnest(c(data))
        
        # compute wpd
        wpd <- compute_pairwise_max(data, 
                                    gran_x = "id_x",
                                    gran_facet = "id_facet",
                                    response = sim_data, lambda = x)
        tibble(mean = y, wpd)
      }) %>% bind_rows() 
      bind_cols(sd = z, data_y)
    })  %>% bind_rows() 
  }) %>% bind_rows() 
  bind_cols(seed = s, data_varf)
})

##---varyall design

data_all_varall <- mclapply(range_seed, function(s){
  set.seed(s)
  data_varall <- mclapply(range_lambda, function(x){
    data_w <- mclapply(range_sd, function(z){
      data_y <- mclapply(range_mean, function(y){   
        # generate the data
        data <- sim_panel(nx = 3,
                          nfacet = 2,
                          ntimes = 500,
                          sim_dist = sim_varall_normal(nx = 3, nfacet = 2, mean = y, sd = z, w = w)) %>%
          unnest(c(data))
        
        # compute wpd
        wpd <- compute_pairwise_max(data, 
                                    gran_x = "id_x",
                                    gran_facet = "id_facet",
                                    response = sim_data, lambda = x)
        tibble(mean = y, wpd)
      }) %>% bind_rows() 
      bind_cols(sd = z, data_y)
    })  %>% bind_rows() 
  }) %>% bind_rows() 
  bind_cols(seed = s, data_varall)
})


data_all <- bind_rows(data_all_null %>% bind_rows(),
                      data_all_varx %>% bind_rows() , 
                      data_all_varf%>% bind_rows(), 
                      data_all_varall %>% bind_rows(),.id = "design")

data_all %>% 
  ggplot(aes(x = lambda, y = wpd, color = design)) +
  geom_line() + facet_grid(sd~mean, 
                           labeller = "label_both") + scale_color_brewer(palette = "Dark2") + 
  geom_point(alpha = 0.5) + 
  theme(legend.position = "bottom")

ggsave(file = here::here("simulations/result_report/omega_lambda_mean_sd.png"))


data_all %>% ggplot(aes(x=wpd)) + geom_density() + facet_grid(mean~sd)
