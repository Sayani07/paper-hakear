## ---- load 
library(knitr)
library(tidyverse)
library(lubridate)
library(lvplot)
#library(ggridges)
library(viridis)
library(tsibble)
library(gravitas)
library(ggpubr)
library(readr)
library(kableExtra)
library(distributional)
library(ggplot2)
library(sugrrants)
library(here)
library(ggplot2)
library(patchwork)

## ---- calendar-elec
elec <- read_rds(here("paper/data/elec.rds")) %>% 
  filter(date >= ymd("20180101"), date < ymd("20180701"))
rdbl <- c("Weekday" = "#d7191c", "Weekend" = "#2c7bb6")

elec <- elec %>% 
  mutate(
    wday = wday(date, label = TRUE, week_start = 1),
    weekday = if_else(wday %in% c("Sat", "Sun"), "Weekend", "Weekday")
  )
p_cal_elec <- elec %>% 
  filter(id %in% c(2, 4)) %>% 
  frame_calendar(x = time, y = kwh, date = date, nrow = 1) %>% 
  ggplot(aes(x = .time, y = .kwh, group = date)) +
  geom_line(aes(colour = as.factor(id)), size = 0.5) +
  scale_colour_brewer(name = "", palette = "Dark2", direction = 1) +
  facet_grid(id ~ ., labeller = label_both) +
  theme(legend.position = "bottom")
prettify(p_cal_elec, size = 2.5, label.padding = unit(0.1, "lines"))

## ---- intro_all
id2_tsibble <- elec %>% 
  filter(id == 2) %>% 
  as_tsibble(index = date_time)

id4_tsibble <- elec %>% 
  filter(id == 4) %>% 
  as_tsibble(index = date_time)

# hour-of-day and month-of-year (important pair) id2's behavior across different hours of the day very different across months, but for id4 behavior across different hours of the day is not likely a function of month.
p1 <- id2_tsibble %>%
  prob_plot("month_year",
            "hour_day",
            response = "kwh",
            plot_type = "quantile",
            symmetric = TRUE,
            quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9)) +
  ggtitle("") + theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + 
  scale_colour_brewer(name = "", palette = "PiYG")

p2 <- id4_tsibble %>%
  prob_plot("month_year",
            "hour_day",
            response = "kwh",
            plot_type = "quantile",
            symmetric = TRUE,
            quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9)) + 
  ggtitle("a") + theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)))


p3 <- id2_tsibble %>%
  create_gran("week_month") %>% 
  filter(week_month != 5) %>% 
  prob_plot("wknd_wday",
            "week_month",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75)) +
  ggtitle("") +
  
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)))

p4 <- id4_tsibble %>%
  create_gran("week_month") %>% 
  filter(week_month != 5) %>% 
  prob_plot("wknd_wday",
            "week_month",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75)) +
  ggtitle("b") +  #+ scale_x_discrete(breaks = seq(0, 23, 4)) + 
  theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)))

## ---- id2
ggpubr::ggarrange(p1, p2, ncol = 2)

## ---- id4
ggpubr::ggarrange(p3, p4, ncol = 2)

## ---- null4by2
sim_varall_normal <- function(nx, nfacet, mean, sd, w) {
  dist_normal((mean + seq(0,
                          (nx *
                             nfacet - 1),
                          by = 1
  ) * w), sd)
}
sim_panel_varall <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varall_normal(2, 3, 5, 10, 5)
) %>% unnest(data)

p_varall <- sim_panel_varall %>%
  rename("facet level" = "id_facet" ) %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + 
  facet_wrap(~`facet level`,labeller = "label_both") + 
  geom_boxplot() +
  ggtitle("") +
  xlab("x level") +
  ylab("")


sim_varx_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w), sd), nfacet)
}

sim_panel_varx <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varx_normal(2, 3, 5, 10, 5)
) %>% unnest(data)


p_varx <- sim_panel_varx %>%
  rename("facet level" = "id_facet" ) %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + 
  facet_wrap(~`facet level`,labeller = "label_both") + 
  ggtitle("") +
  geom_boxplot() +
  xlab("x level") +
  ylab("simulated response")



sim_varf_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w), sd), each = nx)
}

sim_panel_varf <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varf_normal(2, 3, 5, 10, 5)
) %>% unnest(data)


p_varf <- sim_panel_varf %>%
  rename("facet level" = "id_facet" ) %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~`facet level`,labeller = "label_both") + 
  geom_boxplot() +
  ggtitle("") +
  xlab("x level") +
  ylab("")



sim_panel_null <- sim_panel(
  nx = 2,
  nfacet = 3,
  ntimes = 500,
  sim_dist = distributional
  ::dist_normal(5, 10)
) %>% unnest(c(data))

set.seed(9999)


p_null <- sim_panel_null %>%
  rename("facet level" = "id_facet" ) %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~`facet level`,labeller = "label_both") +
  geom_boxplot() +
  ggtitle("") +
  xlab("x level") +
  ylab("simulated response")


ggpubr::ggarrange(p_null, p_varf,  p_varx, p_varall, nrow = 2, ncol = 2,
                  common.legend = TRUE,
                  labels = c("a", "b", "c", "d"))


## ---- notations
d1 <- tibble (variable = c("nx", "nfacet", "$N_C$", "$H_{N_C}$", "$lambda$", "$omega$", "$wpd$","$nperm$", "$nsim$"),  description = c("number of x categories", "number of facet categories","number of cyclic granularities", "set of harmonies", "tuning parameter", "mean increment", "raw distance measure", "number of permutations for threshold/normalization", "number of simulations"))


d2 <- tibble(variable =  c("$wpd_{norm}$", 
                           "$wpd_{threshold}$",
                           "$D_{null}$",
                           "$D_{var_f}$", 
                           "$D_{var_x}$",
                           "$D_{var_{all}}$"), 
             description = c("normalized distance measure",
                             "threshold for significance",
                             "null design",
                             "design with varying facets only", 
                             "design with varying x only", 
                             "design with varying both facets and x"))


d <- bind_rows(d1, d2)
knitr::kable(d, format = "markdown", escape = FALSE, caption = "Nomenclature table")

##----distance-explain
knitr::include_graphics(here::here("paper/Figs/dist_explain.png"))


##----raw
G21 <- read_rds("simulations/raw/null_design_quantrans/data-agg/all_data_wpd_Gamma21.rds")

G21 %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  scale_x_continuous(breaks = scales::breaks_extended(3)) + 
  xlab("wpd")

##----quadratic
G21 %>% 
  ggplot(aes(x=nx*nfacet, y = value)) +
  geom_point(alpha = 0.5, size = 0.5) + stat_summary(fun=median, geom="line", aes(group=1), color = "blue") + xlab("nx*nfacet") + ylab("wpd")

## ---- norm
G21_norm <- read_rds(here("simulations/norm/null_design_quantrans_nperm/data-agg/all_data_wpd_Gamma21.rds"))

G21_norm %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  scale_x_continuous(breaks = scales::breaks_extended(3)) + 
  xlab("wpd normalised using permutation approach")


## ---- linear-model
G21 <- read_rds(here("simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N01.rds"))

G21_median <- G21 %>% 
  group_by(nx*nfacet) %>% 
  summarise(actual = median(value))


# fit model median to log(nx*nfacet)
fit_lm2 <- lm(actual ~ log(`nx * nfacet`) , data = G21_median)

broom::tidy(fit_lm2) %>% kable(caption = "Results of linear model to capture the relationship between wpd and number of comparisons.")

## ---- resi-linear
intercept <- fit_lm2$coefficients[1]
slope <- fit_lm2$coefficients[2]

G21 %>% 
  ggplot(aes(x=log(nx*nfacet), y = (value - slope*log(nx*nfacet)))) +
  geom_point(alpha = 0.5, size = 0.5) + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") + 
  ylab("wpd normalised using linear modelling approach")


## ---- glm-tab
broom::tidy(glm_fit) %>% kable(caption = "Results of generalised linear model to capture the relationship between wpd and number of comparisons.")


## ---- wpd-glm-dist
G21_glm <- G21 %>% 
  mutate(wpd_glm =  (value - (1/(intercept + slope*log(nx*nfacet)
  )
  )
  ),
  wpd_glm_scaled = ((wpd_glm*320)))

#G21_glm$wpd_glm_scaled %>% sd()


G21_glm %>% 
  ggplot() +
  geom_density(aes(x = wpd_glm), 
               fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") +
  theme(legend.position = "bottom") 

##----hist-qq
hist <- ggplot(G21_glm) +
  geom_histogram(aes(x = wpd_glm_scaled)) +
  ggtitle("a") + xlab("transformed wpd using glm approach")


p <- ggplot(G21_glm, aes(sample = wpd_glm_scaled))
qqplot <-  p + geom_qq() + geom_qq_line() + coord_fixed(ratio = 4/5) +
  ggtitle("b")

# G21_glm <- G21 %>% 
#     mutate(wpd_glm =  (value - (1/(intercept + slope*log(nx*nfacet)
#     )
#     )
#     ),
#     wpd_glm_scaled = 300*(wpd_glm))

# G21_glm$wpd_glm_scaled %>% 
#   range()
hist + qqplot

##----dist-new-same-scale-link
G21_permutation <- read_csv(here("simulations/norm/null_design_quantrans_nperm/data-agg/all_data_wpd_N01.csv")) %>%
  rename("wpd_permutation" = "value")


# G21_model_data <- G21 %>%
#   mutate(model =
#            ((1/value)
#                   - intercept -
#                     slope*log(nx*nfacet))/slope) %>%
#   mutate(model_trans =
#            (model - mean(model))/sd(model))

# G21_model_data$model %>% summary()

G21_all_data <- G21_permutation %>% 
  # left_join(G21_lm, by = c("nx", "nfacet", "perm_id")) %>% 
  left_join(G21_glm, by = c("nx", "nfacet", "perm_id")) %>% 
  pivot_longer(cols = c(3, 7),
               names_to = "type_estimate",
               values_to = "value_estimate")
G21_all_data$type_estimate = factor(G21_all_data$type_estimate , levels = c( "wpd_permutation", "wpd_glm_scaled"))


G21_all_data %>% 
  filter(type_estimate %in% c("wpd_glm_scaled", "wpd_permutation")) %>% 
  ggplot() +
  geom_density(aes(x = value_estimate, 
                   fill = type_estimate), alpha = 0.5) +
  facet_grid(nx~nfacet,
             labeller = "label_both") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c( "#D55E00", "#0072B2")) +
  xlab("wpd_norm2") +
  scale_x_continuous(breaks = c(-5, -3, 0, 3, 5))

# G21_all_data %>% 
#   filter(type_estimate %in% c("wpd_permutation", "wpd_glm")) %>% 
#   ggplot() +
#   geom_density(aes(x = value_estimate, 
#                    fill = type_estimate), 
#                alpha = 0.7) +
#   facet_grid(nx~nfacet,
#              labeller = "label_both") +
#   theme(legend.position = "bottom") +
#   scale_fill_manual(values = c("#CC79A7", "#0072B2")) +
#   xlab("wpd_norm2")

# are scales same
# 
# G21_model_data %>%
#   group_by(nx, nfacet) %>% 
#   summarize(sd_model = sd(model))
#   
# G21_all_data %>%
#   filter(type_estimate == c("perm_trans")) %>% 
#   select(value_estimate) %>% 
#   range()
# 
# G21_all_data %>%
#   filter(type_estimate == c("wpd_glm_scaled")) %>% 
#   select(value_estimate) %>% 
#   range()


## ---- varall-new
sim_varall_normal <- function(nx, nfacet, mean, sd, w) {
  dist_normal((mean + seq(0,
                          (nx *
                             nfacet - 1),
                          by = 1
  ) * w), sd)
}
sim_panel_varall <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varall_normal(2, 3, 0, 1, 10)
) %>% unnest(data)

set.seed(9999)
varall <- compute_pairwise_norm(sim_panel_varall, 
                                gran_x = "id_x",
                                gran_facet = "id_facet",
                                response = sim_data, 
                                nperm = 200
)

# plot
p_varall <- sim_panel_varall %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot() +
  ggtitle(paste("d", round(varall, 2))) + xlab("x level")

## ---- varx-new
sim_varx_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w), sd), nfacet)
}

sim_panel_varx <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varx_normal(2, 3, 0, 1, 10)
) %>% unnest(data)


set.seed(9999)
varx <- compute_pairwise_norm(sim_panel_varx, 
                              gran_x = "id_x",
                              gran_facet = "id_facet",
                              response = sim_data, 
                              nperm = 200
)

# plot
p_varx <- sim_panel_varx %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot() +
  ggtitle(paste("c", round(varx, 2))) + xlab("x level")

##----varf-new
sim_varf_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w), sd), each = nx)
}
sim_panel_varf <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varf_normal(2, 3, 0, 1, 10)
) %>% unnest(data)


set.seed(9999)
varf <- compute_pairwise_norm(sim_panel_varf, 
                              gran_x = "id_x",
                              gran_facet = "id_facet",
                              response = sim_data, 
                              nperm = 200
)

p_varf <- sim_panel_varf %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot() +
  ggtitle(paste("b", round(varf, 2))) + xlab("x level")

##----null-new
sim_panel_null <- sim_panel(
  nx = 2,
  nfacet = 3,
  ntimes = 500,
  sim_dist = distributional
  ::dist_normal(0, 1)
) %>% unnest(c(data))

set.seed(9999)

null <- compute_pairwise_norm(sim_panel_null, 
                              gran_x = "id_x",
                              gran_facet = "id_facet",
                              response = sim_data, 
                              nperm = 200
)

p_null <- sim_panel_null %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot() +
  ggtitle(paste("a", round(null, 2))) + xlab("x level") 

## ---- plot-all-new3
(p_null + p_varf)/(p_varx + p_varall)