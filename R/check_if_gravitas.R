library(readr)
library(tidyverse)
library(sugrrants)
library(tsibble)
library(gravitas)

elec <- read_rds("data/elec.rds")

id3 <- elec %>% 
  filter(id == 3)

# p_cal_elec <- id3 %>% 
#   frame_calendar(x = time, y = kwh, date = date, nrow = 1) %>% 
#   ggplot(aes(x = .time, y = .kwh, group = date)) +
#   geom_line(aes(colour = as.factor(id)), size = 0.5) +
#   scale_colour_brewer(name = "", palette = "PiYG") +
#   facet_grid(id ~ ., labeller = label_both) +
#   theme(legend.position = "bottom") +
#   geom_smooth(
#     aes(.time, .kwh, group = date), 
#     se = FALSE, method = "loess"
#   )

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
   create_gran("week_month") %>% 
   create_gran("day_week") %>% 
   create_gran("month_year") %>% 
   create_gran("hhour_day") %>% 
   as_tibble() %>% 
   ggplot(aes(x = week_month, y = kwh)) +
   #geom_jitter(alpha = 0.5) +
   facet_wrap(~day_week) +
   geom_line(aes(group = interaction(hhour_day, month_year))) +
   geom_smooth(method = "loess",
                  colour = "red")


  prob_plot("week_month",
            "day_week",
            response = "kwh",
            plot_type = "violin",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75))


p3 <-   id4_tsibble %>%
  prob_plot("wknd_wday",
            "week_month",
            response = "kwh",
            plot_type = "boxplot",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75))




p4 <- id4_tsibble %>%
   create_gran("week_month") %>% 
   create_gran("day_week") %>% 
   ggplot(aes(x = day_week, y = kwh)) +
   geom_jitter(alpha = 0.1) + facet_wrap(~week_month) +
   geom_violin(fill = "blue")

p4 <-   id4_tsibble %>%
  prob_plot("week_month",
            "day_week",
            response = "kwh",
            plot_type = "violin",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75))

   
   ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

id4_tsibble %>%
      prob_plot("month_year",
                "week_month",
                response = "kwh",
                plot_type = "quantile",
                symmetric = FALSE,
                quantile_prob = c(0.25,0.5,0.75))


id1_tsibble %>%
   prob_plot("month_year",
             "day_week",
             response = "kwh",
             plot_type = "quantile",
             symmetric = TRUE,
             quantile_prob = c(0.25,0.5,0.75))



id3_tsibble %>%
   prob_plot("month_year",
             "day_week",
             response = "kwh",
             plot_type = "boxplot",
             symmetric = TRUE,
             quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9))



id3_tsibble %>%
   prob_plot("month_year",
             "day_week",
             response = "kwh",
             plot_type = "quantile",
             symmetric = TRUE,
             quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9))



id1_tsibble %>%
   prob_plot("day_week",
             "hour_day",
             response = "kwh",
             plot_type = "quantile",
             symmetric = TRUE,
             quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9))


# hour-of-day and month-of-year (important pair) id2's behavior across different hours of the day very different across months, but for id4 beahvior across different hours of the day is not likely a function of month.
id2_tsibble %>%
   prob_plot("month_year",
             "hour_day",
             response = "kwh",
             plot_type = "quantile",
             symmetric = TRUE,
             quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9))

id4_tsibble %>%
   prob_plot("month_year",
             "hour_day",
             response = "kwh",
             plot_type = "quantile",
             symmetric = TRUE,
             quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9))

# wknd_wday and week_month (important pair) id2's behavior across different hours of the day very different across months, but for id4 beahvior across different hours of the day is not likely a function of month.

id2_tsibble %>%
   prob_plot("week_month",
             "hour_day",
             response = "kwh",
             plot_type = "quantile",
             symmetric = FALSE,
             quantile_prob = seq(0.01,0.99,0.01))

id4_tsibble %>%
   prob_plot("week_month",
             "hour_day",
             response = "kwh",
             plot_type = "quantile",
             symmetric = FALSE,
             quantile_prob = seq(0.01,0.99,0.01))
