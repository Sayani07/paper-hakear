set.seed(9999)

mean = 5
range_mean = c(0, 1, 3,  5, 7, 15, 20)
sd = 20
range_sd = c(1, 3,  5, 7, 15, 20)
range_w =  c(1, 2, 3, 4, 5)
range_lambda = seq(0.1, 0.9, 0.05)
nx = 3
nfacet = 2
w = 5
ntimes = 500
      

# sd change 
sim_varf_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal(mean, (sd + seq(0, nfacet-1, by  = 1)*w)), each = nx)
}


# mean change
sim_varf_normal = function(nx, nfacet, mean, sd, w)
{
  rep(dist_normal((mean + seq(0, nfacet-1, by  = 1)*w), sd), each = nx)
}

# check if sim_panel is doing what it is supposed to
data_all <- lapply(range_w, function(w){
sim_dist_data <-  sim_varf_normal(nx, nfacet, mean, sd, w)
id_x <- rep(seq_len(nx), nfacet)
id_facet <- rep(seq_len(nfacet), each = nx)
sim_data2 <- tibble(id_x, id_facet, sim_dist_data) 

sim_panel_data <- sim_data2 %>% group_by(id_facet, 
             id_x) %>%
  summarise(sim_data = generate(sim_dist_data,                                         times = ntimes)) %>% 
  unnest(sim_data) %>% ungroup()

bind_cols(omega = w, sim_panel_data)
}) %>% bind_rows()

data_all %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_grid(omega~id_facet, labeller = "label_both") + geom_boxplot() +
  scale_y_continuous(breaks = seq(-3, 5, 2))


# check if compute_pairwise_max is doing
library(hakear)
library(gravitas)
wpd <- compute_pairwise_max(sim_panel_data, 
                            gran_x = "id_x",
                            gran_facet = "id_facet",
                            response = sim_data, 
                            lambda = 0.8)


ggplot(data_all, aes(x=wpd, y=design, fill = design)) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles") +  facet_grid(sd~mean, labeller = "label_both")
