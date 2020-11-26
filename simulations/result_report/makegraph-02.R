
all_data <- read_rds("simulations/result_report/all_data_raw.rds")

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


ggsave(raw_nxbyfacet, filename = here("simulations/result_report", "nfacet_by_nx_raw.png"))