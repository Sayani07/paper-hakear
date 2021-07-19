elec_select_harmony = parallel::mclapply(1:8, function(x){
  
  data_id <-  elec_split %>% magrittr::extract2(x) %>% 
    as_tsibble(index = date_time)
  
  harmonies <- data_id %>%
    harmony(
      ugran = "month",
      filter_in = "wknd_wday",
      filter_out = c("hhour", "fortnight")
    )
  
  hakear::select_harmonies(data_id,
                           harmony_tbl = harmonies,
                           response = kwh,
                           nperm = 2,
                           nsamp = 10
  )
  
}, mc.cores = parallel::detectCores() - 1, mc.preschedule = FALSE, mc.set.seed = FALSE)