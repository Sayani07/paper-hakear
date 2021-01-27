library(readr)
library(tidyverse)

## --- read
N01 <- read_rds("simulations/raw/null_design_quantrans/data-agg/all_data_wpd_N01.rds")

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


##----raw-nx_nfacet_product
# raw: Di asked you to see this relationship

N01 %>% 
  ggplot(aes(x=as.factor(nx*nfacet), y = value)) +
  geom_boxplot() + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 

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







model1 <- lm(value ~ sqrt(nx*nfacet), data = N01)
model2 <- lm(value^2 ~ I(nx*nfacet), data = N01)





N01 %>% 
  mutate(value = if_else(nx*nfacet<=49, 0.3*sqrt(value), 1.3*value)) 



# 
# %>%
#   ggplot(aes(x=as.factor(nx*nfacet), y = value)) +
#   geom_boxplot() + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") 

