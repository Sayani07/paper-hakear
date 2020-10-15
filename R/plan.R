library(drake)
library(dplyr)
library(tidyr)


   ## Plan targets in here.

    # use gravitas to show motivation
    #plots_motivation = check_if_gravitas(),
    # mean and sd of distribution of maximum and median
    #plot_why_normalise = why_normalise(),
    
    # null distribution
    the_plan <-
      drake_plan(
        
        ## Plan targets in here.
        elec <- read_rds("data/elec.rds"),
        
        id2_tsibble <- elec %>% 
          filter(id == 2) %>% 
          as_tsibble(index = date_time),
        
        id4_tsibble <- elec %>% 
          filter(id == 4) %>% 
          as_tsibble(index = date_time),
        
        # hour-of-day and month-of-year (important pair) id2's behavior across different hours of the day very different across months, but for id4 behavior across different hours of the day is not likely a function of month.
        p1 <- id2_tsibble %>%
          prob_plot("month_year",
                    "hour_day",
                    response = "kwh",
                    plot_type = "quantile",
                    symmetric = TRUE,
                    quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9)) +
          ggtitle("") + theme(
            strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))),
        
        p2 <- id4_tsibble %>%
          prob_plot("month_year",
                    "hour_day",
                    response = "kwh",
                    plot_type = "quantile",
                    symmetric = TRUE,
                    quantile_prob = c(0.1, 0.25,0.5,0.75, 0.9)) + 
          ggtitle("") + theme(
            strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))),
        
        # wknd_wday and week_month (important pair) id2's behavior across different hours of the day very different across months, but for id4 behavior across different hours of the day is not likely a function of month.
        
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
          #scale_x_discrete(breaks = seq(0, 23, 4))  + theme(
          theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))),
        
        p4 <- id4_tsibble %>%
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
        
        # simulate many panel data with x levels and facets
        #set.seed(9999),
        # sim_null_orig  = sim_panel_grid (range_nx = 2:10, 
        #                                  range_nfacet = 2:10,
        #                                  ntimes = 500,
        #                                  sim_dist = distributional::dist_normal(5, 10)),
        
        
        #sim_null_max_dist = why_normalise(sim_null_orig),
        
        
        # plot panel grid
        
        #plot_sim_null = plot_panel_grid(sim_null_orig),
        #ggplot(sim_null_orig, aes(x = sim_data)) + geom_histogram() + facet_grid(nx~nfacet)
        
        # compute mmpd for each panel
        # set.seed(9999),
        # mmpd_null_orig = compute_mmpd_panel_grid(sim_null_orig,
        #                                          quantile_prob = seq(0.01, 0.99, 0.01),
        #                                          dist_ordered = TRUE,
        #                                          nperm = 5),
        # # compute mmpd distribution for each panel
        # set.seed(54321),  
        # mmpd_dist_null_grid =   compute_mmpd_null_dist(sim_null_orig,
        #                                                nsim = 200),
        
        
        # visualise mmpd distribution for entire panel grid
        # plot_dist_null_grid =   plot_mmpd_null_grid(mmpd_dist_null_grid,
        #                                             mmpd_null_orig),  
        
        plot_panel_alleq  = sim_panel(nx = 2, nfacet = 4, ntimes = 500) %>% ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot(),
        
        plot_panel_faceteq= sim_panel(nx = 2,
                  nfacet = 4,
                  ntimes = 500,
                  sim_dist = rep(dist_normal(seq(5, 20, 5), 5), each = 2)) %>% ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot(),
        
        plot_panel_xeq = sim_panel(nx = 2,
                                  nfacet = 4,
                                  ntimes = 500,
                                  sim_dist = rep(dist_normal(c(5, 10),10), 4)) %>% ggplot(aes(x = as.factor(id_x), y = sim_data)) + facet_wrap(~id_facet) + geom_boxplot(),
        
        # testing power
        
        # simulate many panel data with x levels and facets
        
        # 
        # sim_panel_data = slct_lvl_orig(sim_null_orig,
        #                                  nx = 2,
        #                                  nfacet = 3),
        # 
        
        
        
        #    
        #    
        # 
        # 
        #' @examples
        #'    sim_panel_data  = sim_panel(nx = 7,
        #'                                    nfacet = 4,
        #'                                    ntimes = 500,
        #'                                    sim_dist = distributional::dist_normal(5, 10))
        #'     #
        #'       # compute quantiles of simulated panel data
        #' 
        #'        sim_panel_quantiles  = compute_quantiles(sim_panel_data,
        #'                                          quantile_prob = seq(0.01, 0.99, 0.01))
        #' 
        #'       # compute pairwise JS distances for each facet
        #' 
        #'       distance_panel_data  = distance_panel(sim_panel_quantiles, #method = "JS",
        #'                                            dist_ordered = FALSE)
        #'       #
        #' # 
        #' #        compute mpd - normalised max pairwise distances for each facet
        #'       normx_data = mpd(sim_panel_data, distance_panel_data,
        #'                        nperm = 2)
        #'       #  # compute mmpd - no
        #'       
        #'       
        #'       
        #'       rmalised max pairwise distances across all facets
        #'       #  # change the function names
        #'        normfacet_data = mmpd(sim_panel_data, 
        #'        normx_data,
        #'                               nperm = 20)
        
      )
    
  # null_dist_sim1 <- null_sim1(),
  # null_dist_sim2 <- null_sim2(),
  # null_dist_sim3 <- null_sim3(),
   
  # alternate distribution
   
  # alt_dist_sim1 <- alt_sim1(),
  # alt_dist_sim2 <- alt_sim2(),
  # alt_dist_sim3 <- alt_sim3(),
  
  # application1
  # 
  # application2

