the_plan <-
  drake_plan(

   ## Plan targets in here.

    # use gravitas to show motivation
    plots_motivation = check_if_gravitas(),
    # mean and sd of distribution of maximum and median
    plot_why_normalise = why_normalise(),
    
    # null distribution
  null_dist_sim1 <- null_sim1(),
  null_dist_sim2 <- null_sim2(),
  null_dist_sim3 <- null_sim3(),
   
  # alternate distribution
   
  alt_dist_sim1 <- alt_sim1(),
  alt_dist_sim2 <- alt_sim2(),
  alt_dist_sim3 <- alt_sim3(),
  
  # application1
  # 
  # application2
  # 
  # 
  
    
)
