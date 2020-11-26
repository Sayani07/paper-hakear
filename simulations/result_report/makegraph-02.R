# run for raw MMPD files aggregation
#all_data <- read_rds("simulations/result_report/all_data_raw.rds")
# run for raw max pairwise distances files aggregation
all_data <- read_rds("simulations/result_report/all_data_raw_pairwise_max.rds")
# 
# add_pairwise_max while writing if plots are for max pairwise distances

hist_nxbyfacet <- all_data %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_grid(nx~nfacet, labeller = "label_both") + 
  xlab("raw mmpd")

ggsave(hist_nxbyfacet, filename = here("simulations/result_report", "hist_nxbyfacet.png"))


raw_nxbyfacet <- all_data %>% 
  ggplot(aes(x = value, y = as.factor(nx))) +
           ggridges::geom_density_ridges() +
           facet_wrap(~nfacet, labeller = "label_both", nrow = 2) + 
  xlab("raw mmpd") +
  ylab("nx")

ggsave(raw_nxbyfacet, filename = here("simulations/result_report", "nx_by_facet_raw.png"))

  

raw_nfacetbynx <- all_data %>% 
  ggplot(aes(x = value, y = as.factor(nfacet))) +
  ggridges::geom_density_ridges() +
  facet_wrap(~nx, labeller = "label_both", nrow = 2) + 
  xlab("raw mmpd") +
  ylab("nfacet")


ggsave(raw_nfacetbynx, filename = here("simulations/result_report", "nfacet_by_nx_raw.png"))