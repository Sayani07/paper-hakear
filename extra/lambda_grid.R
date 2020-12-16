# contains aggregated data from the file simulations/results/raw/ and tidy them up

library(tidyverse)
library(here)
library(readr)
library(rlang)


# run for raw MMPD files aggregation
#all_files = list.files(path = "simulations/results/raw/",                        pattern = "_dist.rds")

# run for raw max pairwise distances files aggregation
# all_files = list.files(path = "simulations/results/raw/", 
#                             pattern = "pairwise_max_dist.rds")
# 
# all_files = list.files(path = "simulations/results/norm/", 
#                        pattern = "_dist.rds")

# objective 2
all_files = list.files(path = "simulations/results/demo_all_design/", 
                       pattern = "_dist.rds")


names_levels <- map_dfr(all_files, 
                        function(x){
                          z = str_split(str_remove(x, "_dist.rds"), "_") %>% 
                            unlist()
                          bind_cols(nx = as.numeric(z[1]),
                                    nfacet = as.numeric(z[2]))
                        })

# nsim = 200 types repeated
names_rep <- names_levels %>% slice(rep(1:n(), each = 17*4))


all_data <- paste0("simulations/results/demo_all_design/", 
                   all_files)%>%
  map(readRDS) %>% 
  bind_rows() %>%
  bind_cols(names_rep) %>%
  arrange(nfacet, nx)


all_data %>% 
  mutate(design = factor(design, levels = c("D1", "D2", "D3", "D4"))) %>% 
  ggplot(aes(x=lambda, y = raw_mpd, colour = design)) +
  geom_line() + scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) +
  facet_grid(nx~nfacet, )


# run for raw MMPD files aggregation
#write_rds(all_data, "simulations/result_report/all_data_raw.rds")

# run for raw max pairwise distances files aggregation
write_rds(all_data, "simulations/result_report/all_data_raw_pairwise_max.rds")
# 
# write_rds(all_data, "simulations/result_report/all_data_norm_mmpd.rds")
