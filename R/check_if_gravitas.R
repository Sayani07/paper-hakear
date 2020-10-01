library(readr)
library(tidyverse)
library(gravitas)
library(tsibble)
library(sugrrants)
elec <- read_rds("data/elec.rds")

id3 <- elec %>% 
  filter(id == 3)

p_cal_elec <- id3 %>% 
  frame_calendar(x = time, y = kwh, date = date, nrow = 1) %>% 
  ggplot(aes(x = .time, y = .kwh, group = date)) +
  geom_line(aes(colour = as.factor(id)), size = 0.5) +
  scale_colour_brewer(name = "", palette = "PiYG") +
  facet_grid(id ~ ., labeller = label_both) +
  theme(legend.position = "bottom") +
  geom_smooth(
    aes(.time, .kwh, group = date), 
    se = FALSE, method = "loess"
  )

id3_tsibble <- id3 %>% 
  as_tsibble(index = date_time)


harmonies <- id3_tsibble %>%
  harmony(
    ugran = "month",
    filter_in = "wknd_wday",
    filter_out = c("hhour", "fortnight")
  )

smart_harmony <- id3_tsibble %>% 
  gravitas::rank_harmony(
    harmony_tbl = harmonies,
    response = "kwh", dist_ordered = TRUE)

 id3 %>%
  as_tsibble(index = date_time) %>%
  create_gran("week_month") %>%
  create_gran("hour_day") %>%
  prob_plot("hour_day",
            "week_month",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = c(0.25, 0.5, 0.75))

 ###id1


 id1 <- elec %>% 
   filter(id == 1)
 
 p_cal_elec <- id1 %>% 
   frame_calendar(x = time, y = kwh, date = date, nrow = 1) %>% 
   ggplot(aes(x = .time, y = .kwh, group = date)) +
   geom_line(aes(colour = as.factor(id)), size = 0.5) +
   scale_colour_brewer(name = "", palette = "PiYG") +
   facet_grid(id ~ ., labeller = label_both) +
   theme(legend.position = "bottom") +
   geom_smooth(
     aes(.time, .kwh, group = date), 
     se = FALSE, method = "loess"
   )
 
 id1_tsibble <- id1 %>% 
   as_tsibble(index = date_time)
 
 
 harmonies <- id1_tsibble %>%
   harmony(
     ugran = "month",
     filter_in = "wknd_wday",
     filter_out = c("hhour", "fortnight")
   )
 
 smart_harmony1 <- id1_tsibble %>% 
   gravitas::rank_harmony(
     harmony_tbl = harmonies,
     response = "kwh", dist_ordered = TRUE)
 
 id1_tsibble %>%
   create_gran("week_month") %>%
   create_gran("hour_day") %>%
   prob_plot("week_month",
             "hour_day",
             response = "kwh",
             plot_type = "quantile",
             symmetric = TRUE,
             quantile_prob = c(0.1,0.25,0.5,0.75, 0.9))
 

 ###id2 and #id4
 
 id2_tsibble <- elec %>% 
   filter(id == 2) %>% 
   as_tsibble(index = date_time)
 
 
 harmonies2 <- id2_tsibble %>%
   harmony(
     ugran = "month",
     filter_in = "wknd_wday",
     filter_out = c("hhour", "fortnight")
   )
 
 smart_harmony2 <- id2_tsibble %>% 
   gravitas::rank_harmony(
     harmony_tbl = harmonies2,
     response = "kwh", dist_ordered = TRUE)
 
 id4_tsibble <- elec %>% 
   filter(id == 4) %>% 
   as_tsibble(index = date_time)
 
 harmonies4 <- id4_tsibble %>%
   harmony(
     ugran = "month",
     filter_in = "wknd_wday",
     filter_out = c("hhour", "fortnight")
   )
 
 smart_harmony4 <- id4_tsibble %>% 
   gravitas::rank_harmony(
     harmony_tbl = harmonies4,
     response = "kwh", dist_ordered = TRUE)
 
 
p1 <-   id2_tsibble %>%
   prob_plot("wknd_wday",
             "week_month",
             response = "kwh",
             plot_type = "boxplot",
             symmetric = FALSE,
             quantile_prob = c(0.25,0.5,0.75))
 
 
p2 <-   id2_tsibble %>%
  prob_plot("week_month",
            "day_week",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75))


p3 <-   id4_tsibble %>%
  prob_plot("wknd_wday",
            "week_month",
            response = "kwh",
            plot_type = "boxplot",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75))


p4 <-   id4_tsibble %>%
  prob_plot("week_month",
            "day_week",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75))


ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)