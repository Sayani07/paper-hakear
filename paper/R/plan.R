library(drake)
library(dplyr)
library(tidyr)
library(gravitas)
library(tsibble)


   ## Plan targets in here.

    # use gravitas to show motivation
    #plots_motivation = check_if_gravitas(),
    # mean and sd of distribution of maximum and median
    #plot_why_normalise = why_normalise(),
    
    # null distribution
    the_plan <-
      drake_plan(
        
        ## Plan targets in here.
        elec = read_rds("data/elec.rds"),
        
        id2_tsibble =  elec %>% 
          filter(id == 2) %>% 
          as_tsibble(index = date_time),
        
        id4_tsibble =  elec %>% 
          filter(id == 4) %>% 
          as_tsibble(index = date_time),
        
        # hour-of-day and month-of-year (important pair) id2's behavior across different hours of the day very different across months, but for id4 behavior across different hours of the day is not likely a function of month.
        p1 = id2_tsibble %>%
          prob_plot("month_year",
                    "hour_day",
                    response = "kwh",
                    plot_type = "quantile",
                    symmetric = TRUE,
                    quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9)) +
          ggtitle("") + theme(
            strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))),
        
        p2 = id4_tsibble %>%
          prob_plot("month_year",
                    "hour_day",
                    response = "kwh",
                    plot_type = "quantile",
                    symmetric = TRUE,
                    quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9)) + 
          ggtitle("") + theme(
            strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))),
        
        # wknd_wday and week_month (important pair) id2's behavior across different hours of the day very different across months, but for id4 behavior across different hours of the day is not likely a function of month.
        
        p3 = id2_tsibble %>%
          create_gran("week_month") %>% 
          filter(week_month != 5) %>% 
          prob_plot("wknd_wday",
                    "week_month",
                    response = "kwh",
                    plot_type = "quantile",
                    symmetric = FALSE,
                    quantile_prob = c(0.25,0.5,0.75)) +
          ggtitle("") +
          #scale_x_discrete(breaks = seq(0, 23, 4))  + theme(
          theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))),
        
        p4 = id4_tsibble %>%
          create_gran("week_month") %>% 
          filter(week_month != 5) %>% 
          prob_plot("wknd_wday",
                    "week_month",
                    response = "kwh",
                    plot_type = "quantile",
                    symmetric = FALSE,
                    quantile_prob = c(0.25,0.5,0.75)) +
          ggtitle("") +  #+ scale_x_discrete(breaks = seq(0, 23, 4)) + 
          theme(
            strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))),
        
        
        p1p2 = p1 + p2,
        p3p4 = p3 + p4,
        
        target_name = target(
          command = {
            rmarkdown::render(knitr_in("doc/hakear.Rmd"))
            file_out("doc/hakear.pdf")
          }
        )
        
      
      ) 
