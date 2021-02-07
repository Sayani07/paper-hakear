library(readr)
library(tidyverse)

## --- read
# N01 <- read_rds("simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N01.rds")

N01 <- read_csv("simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N01.csv")


#write_csv(N01, "simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N01.csv")

## ---raw

N01 %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))


## ----log
log <- N01 %>% 
  ggplot(aes(x = value/(log(nx*nfacet)))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))

##----inverse
inverse <- N01 %>% 
  ggplot(aes(x = log(log(nx*nfacet))/value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))

##----invboxcox
invboxcox <- N01 %>% 
  ggplot(aes(x = forecast::BoxCox(sqrt(nx)*sqrt(nfacet)/value, lambda = "auto"))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))


##----invboxcox
invboxcox <- N01 %>% 
  ggplot(aes(x = sqrt(nx*nfacet)/value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))

##----boxcox
# shape similar but location different

boxcox <- N01 %>% 
  ggplot(aes(x = (forecast::BoxCox(value/(sqrt(nx)*sqrt(nfacet)), lambda = "auto")))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))
x

##----raw-nx_nfacet_product
# raw: Di asked you to see this relationship

N01 %>% 
  ggplot(aes(x=log(nx*nfacet), y = (value - mean(value))/log(nx*nfacet))) +
  geom_point() + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 


##----raw-nx_nfacet_product_cat
# seeing if the relationship differs for nx<nfacet or nx>nfacet
N01 %>% 
  mutate(comp_lev = if_else(nx<nfacet, "nx<nfacet",
                            if_else(nx>nfacet,
                               "nx>nfacet", "nx=nfacet"))) %>% 
  ggplot(aes(x=as.factor(nx*nfacet), y = value)) +
  geom_boxplot() + facet_wrap(~comp_lev) + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 

# looks like the relationship is quadratic (y <- sqrt(x) or y^2 ~ x) for all comp_lev

##----linearlize1
# to linearlize it from quadratic, we can do a transformation

N01 %>% 
  mutate(comp_lev = if_else(nx<nfacet, "nx<nfacet",
                            if_else(nx>nfacet,
                                    "nx>nfacet", "nx=nfacet"))) %>% 
  ggplot(aes(x=sqrt(nx*nfacet), y = value)) +
  geom_point() + facet_wrap(~comp_lev) + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 


##----linearlize2

N01 %>% 
  mutate(comp_lev = if_else(nx<nfacet, "nx<nfacet",
                            if_else(nx>nfacet,
                                    "nx>nfacet", "nx=nfacet"))) %>% 
  ggplot(aes(x=sqrt(sqrt(nx*nfacet)), y = value)) +
  geom_point() + facet_wrap(~comp_lev) + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 

##----linearlize3

N01 %>% 
  mutate(comp_lev = if_else(nx<nfacet, "nx<nfacet",
                            if_else(nx>nfacet,
                                    "nx>nfacet", "nx=nfacet"))) %>% 
  ggplot(aes(x=log(sqrt(sqrt(nx*nfacet))), y = value)) +
  geom_point() + facet_wrap(~comp_lev) + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 

##----dist_after_linearize
# looks like we have made the relationship from quadratic to linear

 N01 %>% 
  ggplot(aes(x = forecast::BoxCox(log(sqrt(sqrt(nx*nfacet)))/value,
             lambda = "auto"))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))





N01_median <- N01 %>% 
  group_by(nx*nfacet) %>% 
  summarise(actual = median(value))

# fit  <- lm(median_value ~`nx * nfacet` , data = N01_median)
# #second degree
# fit2 <- lm(median_value~poly(`nx * nfacet` ,2, raw=TRUE), data = N01_median)
# #third degree
# fit3 <- lm(median_value~poly(`nx * nfacet` ,3, raw=TRUE), data = N01_median)
# #fourth degree
# fit4 <- lm(median_value~poly(`nx * nfacet` ,4, raw=TRUE), data = N01_median)
# 

fit <- N01_median %>% 
  mutate(lm  = predict(lm(actual ~`nx * nfacet` , data = N01_median)),
         lm_order2 = predict(lm(actual ~ poly(`nx * nfacet` ,2, raw=TRUE), data = N01_median)),
         fit3 = predict(lm(actual ~ poly(`nx * nfacet` ,3, raw=TRUE), data = N01_median)),
         fit4 = predict(lm(actual ~ poly(`nx * nfacet` ,4, raw=TRUE), data = N01_median)),
         glm_order1 = predict(glm(actual ~ `nx * nfacet`), N01_median, family = Gamma(link = "identity")), 
         glm_order2 = predict(glm(actual ~ poly(`nx * nfacet` ,2), N01_median, family = Gamma(link = "identity"))))



long_data <- fit%>% 
  
  pivot_longer(cols = 2:7, names_to = "model", values_to = "fitted_values") %>% 
  filter(model %in% c("glm_order2", "lm_order2", "actual")) %>% 
  mutate(`nx * nfacet` = as.factor(`nx * nfacet`)) 



long_data %>% 
  ggplot() + 
  geom_line(aes(x=`nx * nfacet`,
             y = fitted_values, 
             color = model,
             group = model)) +
  geom_point(data = N01, aes(x = as.factor(nx * nfacet) , y = value), alpha = 0.02, color = "grey")
         

fit_lm2 <- lm(actual ~ poly(`nx * nfacet` ,2, raw=TRUE), data = N01_median)



actual -  4.908e-02
# raw <- function(nx*nfacet)
# with one unit increase in nx*nfacet, raw increases by this much (\delta)?
#  wpd/delta to eliminate impact of nx*"nfacet
# y = a + bx+ cx^2
# dy/dx = b + 2cx
# for one unit increase in nx*nfacet, value of median(value) increases by b + 2cx


N01 %>% 
  ggplot(aes(x = -2.424e-05 + value/2*(-1.235e-08))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))


# N01 %>% 
#   ggplot(aes(x = value/((2.424e-05 + 2*(-1.235e-08)*(nx*nfacet))))) + 
#   geom_density(fill = "blue") +
#   facet_grid(nx~nfacet,
#              labeller = "label_both") + 
#   xlab("wpd") +
#   scale_x_continuous(breaks = scales::breaks_extended(3))

xx <- N01_median$`nx * nfacet`
plot(N01_median$`nx * nfacet`, N01_median$median_value)
lines(xx, predict(fit), col="red")
lines(xx, predict(fit2), col="green")
lines(xx, predict(fit3), col="blue")
lines(xx, predict(fit4), col="purple")
model1 <- lm(median_value ~`nx * nfacet` , data = N01_median)
model2 <- lm(value^2 ~ I(nx*nfacet), data = N01)



N01 %>% 
  mutate(value = if_else(nx*nfacet<=49, 0.3*sqrt(value), 1.3*value)) 



# fit a log-linear relationship as suggested by Rob and Di

fit_lm2 <- lm(actual ~ poly(log(`nx * nfacet`) ,1, raw=TRUE), data = N01_median)

summary(fit_lm2)

intercept <- fit_lm2$coefficients[1]
slope <- fit_lm2$coefficients[2]


## attempt 1
# relationship of mean and nx*nfacet

N01 %>% 
  ggplot(aes(x=log(nx*nfacet), y = (value - intercept)/log(nx*nfacet))) +
  geom_point() + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 

# distribution across nx and nfacet
N01 %>% 
  ggplot(aes(x = (value - intercept)/(log(nx*nfacet)))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))



## attempt 2
# suggested by Rob again

N01 %>% 
  ggplot(aes(x=log(nx*nfacet), y = (value - intercept - slope*log(nx*nfacet)))) +
  geom_point() + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 


N01 %>% 
  ggplot(aes(x = ((value - intercept - slope*log(nx*nfacet))))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))


## attempt 3 (check if other distribution also leads to this)
## for this you need to change the input files and check if the coefficients are kind of same

G21 <- read_rds("simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N05.rds")

G21$value %>% summary()

G21_median <- G21 %>% 
  group_by(nx*nfacet) %>% 
  summarise(actual = median(value))

G21_median %>% summary(actual)

fit_lm2 <- lm(actual ~ poly(log(`nx * nfacet`) ,1, raw=TRUE), data = G21_median)

summary(fit_lm2)

intercept <- fit_lm2$coefficients[1]
slope <- fit_lm2$coefficients[2]

G21 %>% 
  ggplot(aes(x=log(nx*nfacet), y = (value - intercept - slope*log(nx*nfacet)))) +
  geom_point() + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 


G21 %>% 
  ggplot(aes(x = ((value - intercept - slope*log(nx*nfacet))))) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  xlab("wpd") +
  scale_x_continuous(breaks = scales::breaks_extended(3))

