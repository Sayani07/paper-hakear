# contains aggregated data from the file simulations/results/raw/ and tidy them up

library(tidyverse)
library(here)
library(readr)
library(rlang)
here::here()

all_files = list.files(path = "simulations/results/raw/", 
                       pattern = ".rds")


names_levels <- map_dfr(all_files, 
                        function(x){
                          z = str_split(str_remove(x, "_dist.rds"), "_") %>% 
                            unlist()
                          bind_cols(nx = as.numeric(z[1]),
                                    nfacet = as.numeric(z[2]))
                        })

# nsim = 200 types repeated
names_rep <- names_levels %>% slice(rep(1:n(), each = 200))


all_data <- paste0("simulations/results/raw/", 
                   all_files)%>%
  map(readRDS) %>% 
  bind_rows() %>%
  bind_cols(names_rep) %>%
  arrange(nfacet, nx)

write_rds(all_data, "simulations/result_report/all_data_raw.rds")
