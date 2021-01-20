library(readr)
library(tidyverse)
N01 <- read_rds("simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N01.rds")

raw <- N01 %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))


log <- N01 %>% 
  ggplot(aes(x = value/(log(nx*nfacet)))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))


inverse <- N01 %>% 
  ggplot(aes(x = log(log(nx*nfacet))/value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))


invboxcox <- N01 %>% 
  ggplot(aes(x = forecast::BoxCox(sqrt(nx)*sqrt(nfacet)/value, lambda = "auto"))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))


# shape similar but location different
boxcox <- N01 %>% 
  ggplot(aes(x = (forecast::BoxCox(value/(sqrt(nx)*sqrt(nfacet)), lambda = "auto")))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))
