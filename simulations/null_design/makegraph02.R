library(here)
library(readr)
library(tidyverse)
# run for raw MMPD files aggregation
#all_data <- read_rds("simulations/result_report/all_data_raw.rds")
# run for raw max pairwise distances files aggregation
all_data <- read_rds(here::here("simulations/tuning_param/all_data.rds")) %>% 
  filter(nx<30)
  
# for omega 8
nxbyfacet <- all_data %>% 
  filter(omega == 8) %>% 
  ggplot(aes(x = lambda, y = wpd)) +
  geom_line(aes(colour = design)) +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("raw mmpd")

ggsave(nxbyfacet, filename = here("simulations/tuning_param/figs/", "nxbyfacet_omega8.png"))

# for omega 1
nxbyfacet <- all_data %>% 
  filter(omega == 1) %>% 
  ggplot(aes(x = lambda, y = wpd)) +
  geom_line(aes(colour = design)) +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("raw mmpd")

ggsave(nxbyfacet, filename = here("simulations/tuning_param/figs/", "nxbyfacet_omega1.png"))

# graph of intersection

intersection_data <- all_data %>% 
  pivot_wider(id_cols = -c(design), names_from = design, values_from = wpd) %>% 
  group_by(nx, nfacet, omega) %>% 
  mutate(intersection_point = abs(vary_x - vary_facet)) %>% filter(intersection_point == min(intersection_point)) %>% 
  mutate(factor = as.factor(omega))

intersection_plot <- ggplot(intersection_data %>% ungroup(), aes(x = omega, y = lambda)) +
  geom_line() +
  geom_point() + 
  facet_grid(nx~nfacet, labeller = "label_both")


ggsave(intersection_plot, filename = here("simulations/tuning_param/figs/", "intersection_plot.png"))


