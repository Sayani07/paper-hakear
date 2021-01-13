library(readr)
library(tidyverse)

vary_x <- read_rds("simulations/vary_x/null_design_quantrans/data-agg/all_data_wpd_N01.rds")

vary_all <- read_rds("simulations/vary_all/raw/data-agg/all_data_wpd_N01.rds")

vary_facet <- read_rds("simulations/vary_facet/raw/data-agg/all_data_wpd_N01.rds")

all_design <- bind_rows(vary_facet, vary_x, vary_all, .id = "design") %>% 
  mutate(design = case_when(
    design=="1" ~ "vary_facet",
    design=="2" ~ "vary_x",
    design=="3" ~ "vary_all",
    TRUE ~ as.character(design)                                                                 ))

all_design %>% 
  ggplot() +
  geom_density(aes(x=value), 
                 alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2")  +
  facet_wrap(~w, labeller = "label_both")
#+ facet_grid(nx~nfacet)
  




  
