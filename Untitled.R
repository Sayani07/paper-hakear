library(tidyverse)
library(tsibbledata)
data = vic_elec %>% as_tibble() %>% slice(1:10)

(1:4) %>% 
  purrr::map_dfc(function(i){
    dplyr::lag(data$Demand, n = i)
  })