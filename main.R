##----load
library(ggplot2)
library(gravitas)
library(purrr)
library(distributional)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggpubr)

##--sim_plot
sim_plot <- function(.data, sim_dist, nsamp = 20)
{
  data1 <- sim_distharmony1(.data, sim_dist = sim_dist)
  #data2 <- data1
  #names(data2) =  c("Var2", "Var1","dist","sim_dist")
  
  data_l = bind_cols(pairn = 1L, data1) %>% select(-dist) %>% unnest(sim_dist)
  #data_m = bind_cols(pairn = 2L, data2) %>% select(pairn, Var1, Var2,sim_dist, -dist) %>% unnest(sim_dist)
  #data_mlist =  list(data_l, data_m)
  data_mlist = list(data_l)
  harmonies = .data
  
  global_harmony <-  map(data_mlist, ~ (.x %>% select(-1)))%>%
    global_threshold(harmony_tbl = harmonies,
                     response = "sim_dist",
                     dist_distribution = "normal",
                     dist_ordered = TRUE,
                     create_gran_data = FALSE, nsamp = nsamp)
  
  g = global_harmony[[2]] %>% as_tibble()
  h1 = global_harmony[[1]]$MMPD[1]
  h2 = global_harmony[[1]]$MMPD[2]
  h3 = global_harmony[[3]]
  
  sim_hist <- ggplot(g, aes(x = value)) + geom_histogram()  + geom_vline(xintercept =  h1, colour = "red", show.legend = TRUE) +
    geom_vline(xintercept =  h2, colour = "blue", show.legend = TRUE) +  geom_vline(xintercept =  h3, colour = "black", show.legend = TRUE) 
  
  
  
  plot_harmony_row1 <- data1 %>% select(-dist) %>% unnest(sim_dist) %>%
    ggplot(aes(x = Var2, y = sim_dist)) +
    facet_wrap(~Var1) + geom_boxplot()
  
  plot_harmony_row2 <- data2 %>% select(-dist) %>% unnest(sim_dist) %>%
    ggplot(aes(x = Var2, y = sim_dist)) +
    facet_wrap(~Var1) + geom_boxplot()
  
  return(list(sim_hist, plot_harmony_row1, plot_harmony_row2, global_harmony[[1]], data1))
}

##----rank_harmony
rank_harmony <- function(.data = NULL,
                         harmony_tbl = NULL,
                         response = NULL,
                         prob = seq(0.01, 0.99, 0.01),
                         dist_distribution = "normal",
                         hierarchy_tbl = NULL,
                         dist_ordered = TRUE,
                         alpha = 0.05,
                         create_gran_data = TRUE)
{
  # <- _data <- <- <- step1(.data, harmony_tbl, response)
  
  dist_harmony_data <- dist_harmony_tbl(.data, harmony_tbl, response, prob, dist_distribution, hierarchy_tbl, dist_ordered, create_gran_data)
  
  comp_dist <- dist_harmony_data %>%
    unlist %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    tibble::as_tibble(.name_repair = "unique")
  
  # all for n = 100
  # taken from Tests for the Exponential, Weibull and Gumbel Distributions Based on the Stabilized Probability Plot
  if(alpha == 0.05){
    galpa <- 0.073
  }
  else if (alpha == 0.1){
    galpa <- 0.066
  }
  else if (alpha == 0.01){
    galpa <- 0.089
  }
  
  mean_max <- comp_dist$...1
  max_distance <- comp_dist$...2
  
  harmony_sort <- harmony_tbl %>%
    dplyr::mutate(MMPD = round(mean_max,2)) %>%
    dplyr::arrange(dplyr::desc(MMPD)) %>%
    #dplyr::mutate(r = rank(-max_pd)) %>%
    #dplyr::filter(max_norm_s>=galpa) %>%
    #dplyr::select(-max_norm_s) %>%
    dplyr::filter(!is.na(MMPD)) %>% 
    rename("facet" = "facet_variable",
           "x" = "x_variable",
           "facet_l" = "facet_levels",
           "x_l" = "x_levels") 
    
  
  harmony_sort
}


# loop through all harmony pairs in the harmony table
# uses dist_harmony_pair used for calculatin%>% max pairiwise
# distance for one harmony pair

dist_harmony_tbl <- function(.data, harmony_tbl, response, prob,
                             dist_distribution = NULL, hierarchy_tbl = NULL, dist_ordered = NULL, create_gran_data = NULL,...){
  step1_data <- step1(.data, harmony_tbl, response, hierarchy_tbl,create_gran_data,...)
  (1: length(step1_data)) %>%
    purrr::map(function(rowi){
      step_datai <- step1_data %>%
        magrittr::extract2(rowi)
      z <- dist_harmony_pair(step_datai, prob, dist_distribution, dist_ordered,create_gran_data,...)
      c(z$val, z$max_distance)
    })
}

# average of max pairwise distance for one harmony pair
dist_harmony_pair <-function(step1_datai,
                             prob = seq(0.01, 0.99, 0.01),
                             dist_distribution = "normal", dist_ordered,create_gran_data,...)
{
  colnames(step1_datai) <- paste0("L",colnames(step1_datai))
  colNms <- colnames(step1_datai)[2:ncol(step1_datai)]
  lencol <- length(colNms)
  lenrow <- nrow(step1_datai)
  
  step2 <- NULL
  for (i in 1:lencol) {
    step2[[i]] <- lapply(step1_datai[[colNms[i]]], quantile_extractx)
  }
  
  step3 <- rep(list(diag(lenrow)), lencol)
  
  step4 <- prob <- a <-  b <- mu <- sigma <- array(NA, dim = lencol)
  
  dist_vector <- vector()
  ## Logic
  #__ find the stepped sum difference of density vector elements
  for (k in 1:lencol){
    
    dist <- matrix(NA,
                   nrow = lenrow,
                   ncol = lenrow) ## Matrix
    row_of_col_max <- NULL
    for(i in 1:(lenrow-1))
    {
      for (j in (i+1):lenrow)
      {
        m1 <- step2[[k]][[i]]
        m2 <- step2[[k]][[j]]
        dist[i, j] <- compute_JSD(m1, m2)
        dist[dist == 0] <- NA
        if(dist_ordered)
        {
          
          if(j!=i+1) dist[i, j] <-NA
        }
      }
    }
    
    max_dist <- max(dist, na.rm = TRUE)
    
    dist[lower.tri(dist)] <- NA
    len_uniq_dist <- lenrow^2 - length(which(is.na(dist)))
    prob[k] <- (1- 1/len_uniq_dist)
    
    mu[k] <- mean(dist, na.rm = TRUE)
    sigma[k] <- stats::sd(dist, na.rm = TRUE)
    
    if(dist_distribution == "general")
    {
      a[k] <- stats::quantile(as.vector(dist), prob = 1-prob[k], type = 8, na.rm = TRUE)
      step4[k] <- max_dist/a[k]
    }
    
    if(dist_distribution == "normal")
    {
      b[k] <- stats::qnorm(p = prob[k], mean = mu[k], sd = sigma[k])
      a[k] <- 1/(len_uniq_dist*stats::dnorm(b[k], mean = mu[k], sd = sigma[k]))
      step4[k] <- dplyr::if_else(len_uniq_dist==1, mu[k], (max_dist - b[k])/a[k])
    }
    
    d<- as.vector(dist)
    d <- d[!is.na(d)]
    dist_vector <- rbind(dist_vector,d)
  }
  
  row.names(dist_vector)
  
  # normalised max of normalised max
  
  nmax_nmax <- stats::median(step4, na.rm = TRUE)/log(lencol)
  
  # unnormalised max of normalised max
  
  max_nmax <- max(step4)
  
  # normalised max of unormalised max
  
  nmax_max <- stats::median(max_dist, na.rm = TRUE)/log(lencol)
  
  # unnormalised max of unormalised max
  
  max_max <- max(max_dist, na.rm = TRUE)
  
  value <- list(val = nmax_nmax, distvec = dist_vector, max_distance = max_max)
  value
}

# create two granularities at once
create_gran_pair <-  function(.data, gran1, gran2, hierarchy_tbl = NULL)
{
  .data %>%
    create_gran(gran1, hierarchy_tbl) %>%
    create_gran(gran2, hierarchy_tbl)
}


#harmony_data <-create_harmony_data(.data, harmony_tbl, response)

# <- for each element of the list formed

step1 <- function(.data, harmony_tbl, response = NULL, hierarchy_tbl = NULL, create_gran_data = NULL,...){
  
  harmony_data <-create_harmony_data(.data, harmony_tbl, response, hierarchy_tbl, create_gran_data)
  
  (1: length(harmony_data)) %>%
    purrr::map(function(rowi){
      harmony_datai <- harmony_data %>% magrittr::extract2(rowi)
      namesi <- names(harmony_datai)
      
      #responsei <- create_harmony_datai[[response]]
      
      harmony_datai  %>% 
        ungroup() %>% 
        dplyr::mutate(
          response = harmony_datai[[response]]
        ) %>%
        dplyr::select(-!!response) %>%
        tidyr::pivot_wider(names_from = namesi[1],
                           values_from = response,
                           values_fn = list(response = list))
    })
}

# create data for each row of harmony table
# a list created with a tsibble in each element corresponding to each row of the harmony table
# create_harmony_data(smart_meter10, harmony_tbl, "general_supply_kwh")
create_harmony_data <- function(.data = NULL, harmony_tbl = NULL, response = NULL, hierarchy_tbl = NULL,
                                create_gran_data= TRUE,...)
{
  if(create_gran_data)
  {
    (1:nrow(harmony_tbl)) %>% purrr::map(function(rowi){
      .data %>% create_gran_pair(harmony_tbl$facet_variable[rowi],
                                 harmony_tbl$x_variable[rowi], hierarchy_tbl) %>%
        tibble::as_tibble() %>%
        dplyr::select(harmony_tbl$facet_variable[rowi],
                      harmony_tbl$x_variable[rowi],
                      .data[[tidyselect::all_of(response)]])
    })
  }
  else
  {
    return(.data)
  }
}
# already put
#step1_data <- step1(.data, harmony_tbl, response)

# already put
# dist_harmony_data <- dist_harmony_tbl(step1_data)

# compute_JSD <- function(x, y, message = FALSE)
# {
#   mat <- rbind(x, y)
#   return(philentropy::JSD(mat))
# }

# density_extractx <- function(x)
# {
#   stats::density(x)$y
# }

#  Rob's code for computing JSD using quantiles
#
# Compute Jensen-Shannon distance
# based on quantiles q and p at probabilities prob
JS <- function(prob,q,p)
{
  # Compute approximate densities
  x <- seq(min(q,p),max(q,p), l=201)
  qpmf <- pmf(x,prob,q)
  ppmf <- pmf(x,prob,p)
  m <- 0.5 * (ppmf + qpmf)
  JS <- suppressWarnings(0.5*(sum(stats::na.omit(ppmf*log(ppmf/m))) +
                                sum(stats::na.omit(qpmf*log(qpmf/m)))))
  return(JS)
}

# Compute approximate discretized density (like a probability mass function)
# at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q)
{
  qcdf <- stats::approx(q,p,xout=x,yleft=0,yright=1, ties = mean)$y
  qpmf <- c(0,diff(qcdf)/ (x[2]-x[1]))
  return(qpmf / sum(qpmf))
}

compute_JSD <- function(x, y, prob = seq(0.01, 0.99, 0.01))
{
  JSD <- JS(prob, x, y)
  return(JSD)
}

quantile_extractx <- function(x =  NULL, prob = seq(0.01, 0.99, by = 0.01))
{
  stats::quantile(x, prob, type=8, na.rm = TRUE)
}

##----global_threshold
global_threshold <- function(.data = NULL,
harmony_tbl = NULL,
response = NULL,
prob = seq(0.01,0.99, 0.01),
hierarchy_tbl = NULL,
create_gran_data = TRUE,
dist_ordered = TRUE,
nsamp = 20,...)
{
  MMPD_obs <-  .data %>%
    rank_harmony(harmony_tbl = harmonies,
                 response = response,
                 create_gran_data = create_gran_data,
                 dist_ordered = dist_ordered,...)
  
  
  MMPD_sample_lst <- (1:nsamp) %>%
    purrr::map(function(nsampi){
      
      if(create_gran_data)
      { 
        response_sample <-  sample(.data[[response]], size = nrow(.data))
        
        data_sample <- .data %>%
          dplyr::mutate(response = response_sample)%>%
          dplyr::select(-!!response) %>%
          dplyr::mutate(
            !!response := response) %>%
          dplyr::select(-response)
      }
      
      else{
        
        .data <- (1:length(.data)) %>%
          purrr::map(function(i){
            .data %>% magrittr::extract2(i) %>%
              dplyr::mutate(id = i)
          })
        
        data <- bind_rows(.data) %>% ungroup()  
        response_sample <-  sample(data[[response]], size = nrow(data))
        
        data_sample <- data %>%
          dplyr::mutate(response = response_sample)%>%
          dplyr::select(-!!response) %>%
          dplyr::mutate(
            !!response := response) %>%
          dplyr::select(id, everything(), - response)
        
        data_sample <- split(data_sample, data_sample$id)
        data_sample <- map(data_sample, ~ (.x %>% select(-1)))
      }
      
      data_sample %>%
        rank_harmony(harmony_tbl = harmonies,
                     response = response,
                     create_gran_data = create_gran_data,
                     dist_ordered = dist_ordered) %>%
        dplyr::select(MMPD)
      
    })
  
  # MMPD_sample <- (1:nsamp) %>%
  #   purrr::map(function(i){
  #     MMPD_sample_lst %>% magrittr::extract2(i) %>%  dplyr::select(MMPD)
  #   })
  # 
  # maxpd_sample <- (1:nsamp) %>%
  #   purrr::map(function(i){
  #     MMPD_sample_lst %>% magrittr::extract2(i) %>%  dplyr::select(max_pd)
  #   })

    
  right_quantile_MMPD <- stats::quantile(unlist(MMPD_sample_lst), probs = 0.95)
  #right_quantile_maxpd <- stats::quantile(unlist(maxpd_sample), probs = 0.95)
  #MMPD_tbl <- MMPD_obs %>% dplyr::mutate(select_harmony = MMPD > right_quantile_MMPD,
                                         #gt_0.95_MMPD = right_quantile_MMPD)
  #gt_maxpd = max_pd > right_quantile_maxpd)

  MMPD_obs <- MMPD_obs %>% mutate(threshold = right_quantile_MMPD) %>% dplyr::mutate(threshold = round(threshold,2)) %>% dplyr::mutate(MMPD = round(MMPD,2)) 
  
  
  MMPD_sample <- unlist(MMPD_sample_lst)
  
  return(list(MMPD_obs, MMPD_sample, right_quantile_MMPD))
}

##----samenull_2by4
harmonies <- tibble::tibble(facet_variable = c("A", "B"),x_variable  = c("B","A"), facet_levels = c(2, 4),x_levels = c(4, 2))
.data = harmonies[1,]

sim_dist1 = rep(distributional::dist_normal(5, 10), 8)


a = sim_plot(.data, sim_dist = sim_dist1, nsamp = 200)

a[[5]]%>% select(-sim_dist) %>% kable()
a[[1]]
a[[2]]
a[[3]]
a[[4]]%>% kable()


##----diffnull_7by11

harmonies <- tibble::tibble(facet_variable = c("A", "B"),x_variable  = c("B","A"), facet_levels = c(2, 4),x_levels = c(4, 2))
.data = harmonies[1,]

sim_dist6 <- distributional::dist_normal(mu = 1:8, sigma = 3)

a = sim_plot(.data, sim_dist = sim_dist6, nsamp = 200)
a[[5]]%>% select(-sim_dist) %>% kable()
a[[1]]
a[[2]]
a[[3]]
a[[4]]%>% kable()

##----diffnull_7by11normal

harmonies <- tibble::tibble(facet_variable = c("A", "B"),x_variable  = c("B","A"), facet_levels = c(2, 4),x_levels = c(4, 2))
.data = harmonies[1,]

sim_dist6 <- distributional::dist_normal(mu = seq(1,24, 3), sigma = 3)

a = sim_plot(.data, sim_dist = sim_dist6, nsamp = 200)
a[[5]]%>% select(-sim_dist) %>% kable()
a[[1]]
a[[2]]
a[[3]]
a[[4]]%>% kable()



##----diffnull_7by11normal2

harmonies <- tibble::tibble(facet_variable = c("A", "B"),x_variable  = c("B","A"), facet_levels = c(2, 4),x_levels = c(4, 2))
.data = harmonies[1,]

sim_dist6 <- distributional::dist_normal(mu = c(1,22, 4, 19, 7, 16, 10, 13), sigma = 3)

a = sim_plot(.data, sim_dist = sim_dist6, nsamp = 200)
a[[5]]%>% select(-sim_dist) %>% kable()
a[[1]]
a[[2]]
a[[3]]
a[[4]]%>% kable()

##----diffnull_2by4
harmonies <- tibble::tibble(facet_variable = c("A", "B"),x_variable  = c("B","A"), facet_levels = c(2, 4),x_levels = c(4, 2))
.data = harmonies[1,]

sim_dist2 <- c(rep(distributional::dist_normal(mu = 10, sigma = 5),2),rep(distributional::dist_exponential(10),2), rep(distributional::dist_weibull(0.5, 2),2), rep(distributional::dist_exponential(5),2))

a = sim_plot(.data, sim_dist = sim_dist2, nsamp = 200)
a[[5]]%>% select(-sim_dist) %>% kable()
a[[1]]
a[[2]]
a[[3]]
a[[4]]%>% kable()






##----samenull_3levels

harmonies <- tibble::tibble(facet_variable = c("A", "B","A", "C", "B", "C"),x_variable  = c("B","A", "C", "A", "C", "B"), facet_levels = c(2, 7, 2, 11, 7, 11),x_levels = c(7, 2, 11, 2, 11, 7))

.data1 = harmonies[1,]
.data3 = harmonies[3,]
.data5 = harmonies[5,]

sim_dist7 <- distributional::dist_normal(0,1)

data1 <- sim_distharmony1(.data1, sim_dist = sim_dist7) %>% unnest(sim_dist) %>% select(-dist) %>% ungroup()
data2 <- data1
names(data2) =  c("Var2", "Var1","sim_dist")
data2 <- data2 %>% select(Var1, everything())


data3 <- sim_distharmony1(.data3, sim_dist = sim_dist7)%>% unnest(sim_dist)%>% select(-dist) %>% ungroup()
data4 <- data3
names(data4) =  c("Var2", "Var1","sim_dist")
data4 <- data4 %>% select(Var1, everything())

  
data5 <- sim_distharmony1(.data5, sim_dist = sim_dist7)%>% unnest(sim_dist) %>% select(-dist) %>% ungroup()
data6 <- data5
names(data6) =  c("Var2", "Var1","sim_dist")
data6 <- data6 %>% select(Var1, everything())


# data_1 = bind_cols(pairn = 1L, data1) %>% select(-dist) %>% unnest(sim_dist)
# data_2 = bind_cols(pairn = 2L, data2) %>% select(pairn, Var1, Var2,sim_dist, -dist)

data_mlist =  list(data1, data2, data3, data4, data5, data6)


global_harmony <-  data_mlist %>%
  global_threshold(harmony_tbl = harmonies,
                   response = "sim_dist",
                   dist_distribution = "normal",
                   dist_ordered = TRUE,
                   create_gran_data = FALSE, nsamp = 20)


g = global_harmony[[2]] %>% as_tibble()
h = global_harmony[[1]]$gt_0.95_MMPD[1]
g 
ggplot(g, aes(x = value)) + geom_histogram()  + geom_vline(xintercept =  h, colour = "red")



pnull <- data1 %>% unnest(sim_dist) %>%
  ggplot(aes(x = Var2, y = sim_dist)) +
  facet_wrap(~Var1) + geom_boxplot() + ggtitle("selected by both max not MMPD even when distribution is same")
pnull

##----diffnull_3levels


harmonies <- tibble::tibble(facet_variable = c("A", "B","A", "C", "B", "C"),x_variable  = c("B","A", "C", "A", "C", "B"), facet_levels = c(2, 7, 2, 11, 7, 11),x_levels = c(7, 2, 11, 2, 11, 7))

.data1 = harmonies[1,]
.data3 = harmonies[3,]
.data5 = harmonies[5,]

sim_dist7 <- distributional::dist_normal(0,1)
sim_dist8 <- c(rep(distributional::dist_normal(mu = 10, sigma = 5),4),rep(distributional::dist_exponential(10),4), rep(distributional::dist_weibull(0.5, 2),4), rep(distributional::dist_exponential(5),4), rep(distributional::dist_exponential(10),6))

sim_dist9 <- distributional::dist_normal(mu = 1:77, sigma = 5)

data1 <- sim_distharmony1(.data1, sim_dist = sim_dist7) %>% unnest(sim_dist) %>% select(-dist) %>% ungroup()
data2 <- data1
names(data2) =  c("Var2", "Var1","sim_dist")
data2 <- data2 %>% select(Var1, everything())


data3 <- sim_distharmony1(.data3, sim_dist = sim_dist8)%>% unnest(sim_dist)%>% select(-dist) %>% ungroup()
data4 <- data3
names(data4) =  c("Var2", "Var1","sim_dist")
data4 <- data4 %>% select(Var1, everything())


data5 <- sim_distharmony1(.data5, sim_dist = sim_dist9)%>% unnest(sim_dist) %>% select(-dist) %>% ungroup()
data6 <- data5
names(data6) =  c("Var2", "Var1","sim_dist")
data6 <- data6 %>% select(Var1, everything())


# data_1 = bind_cols(pairn = 1L, data1) %>% select(-dist) %>% unnest(sim_dist)
# data_2 = bind_cols(pairn = 2L, data2) %>% select(pairn, Var1, Var2,sim_dist, -dist)

data_mlist =  list(data1, data2, data3, data4, data5, data6)


global_harmony <-  data_mlist %>%
  global_threshold(harmony_tbl = harmonies,
                   response = "sim_dist",
                   dist_distribution = "normal",
                   dist_ordered = TRUE,
                   create_gran_data = FALSE, nsamp = 20)

global_harmony$MMPD_tbl %>% kable()

p1 <- data6 %>% unnest(sim_dist) %>%
  ggplot(aes(x = Var2, y = sim_dist)) +
  facet_wrap(~Var1) + geom_boxplot()
p1

p1 <- data3 %>% unnest(sim_dist) %>%
  ggplot(aes(x = Var2, y = sim_dist)) +
  facet_wrap(~Var1) + geom_boxplot() + ggtitle("selected by both MMPD and max")
p1

p2 <- data5 %>% unnest(sim_dist) %>%
  ggplot(aes(x = Var2, y = sim_dist)) +
  facet_wrap(~Var1) + geom_boxplot() + ggtitle("selected by max and not MMPD")
p2

p3 <- data6 %>% unnest(sim_dist) %>%
  ggplot(aes(x = Var2, y = sim_dist)) +
  facet_wrap(~Var1) + geom_boxplot() + ggtitle("selected by max and not MMPD")
p3


##----introduction
sm <- smart_meter10 %>%
  filter(customer_id %in% c(10017936))

library(lubridate)
ptry <- sm %>%
  create_gran("wknd_wday") %>% 
  create_gran("week_month") %>% 
  create_gran("hhour_day") %>% 
  create_gran("month_year") %>% 
  create_gran("day_month") %>%
  filter(year(reading_datetime)==2013)%>% 
  ggplot(aes(x = as.numeric(week_month), 
             y = general_supply_kwh)) +
  geom_line(aes(group = hhour_day,
                color = as.numeric(month_year)))




+ geom_smooth(aes(x = as.numeric(week_month), y = general_supply_kwh), method = "loess", formula = y~x, color = "red", se = TRUE)  + facet_wrap(~wknd_wday)
  

ptry2 <- sm %>%
  filter(year(reading_datetime)==2013) %>% 
  create_gran("wknd_wday") %>% 
  create_gran("hour_day") %>% 
  ggplot(aes(x = as.numeric(hour_day), 
             y = general_supply_kwh)) +
  geom_point()+ geom_smooth(aes(x = as.numeric(week_month), y = general_supply_kwh), method = "loess", formula = y~x, color = "red", se = TRUE)  + facet_wrap(~wknd_wday)
 
  #facet_wrap(~wknd_wday)
  


 p1 <- prob_plot("wknd_wday", 
                       "week_month",
                       plot_type = "boxplot",
                       alpha = 0.5) + ggtitle("") +
  ylab("") + 
  xlab("") +
  geom_smooth(aes(x = as.numeric(week_month), y = general_supply_kwh), method = "loess", formula = y~x, color = "red", se = TRUE) 


p4 <- sm %>% prob_plot("wknd_wday", 
                       "hour_day",
                       plot_type = "boxplot") + ggtitle("") +
  ylab("") + 
  xlab("") + scale_x_discrete(breaks = c(0,seq(3, 23, 3))) +
  geom_violin(alpha = 0.3, color = "red") + 
  geom_smooth(aes(x = as.numeric(hour_day), y = general_supply_kwh), method = "loess", formula = y~x, color = "red", se = TRUE) 

ggarrange(p1, p4, nrow = 2, labels = c("a", "b"))
