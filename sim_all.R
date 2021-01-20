wpd_raw <- function(seed = NULL, 
           preprocess = "yes", 
           design = NULL,
           sim_dist = "N(0,1)",
           nx = NULL,
           nfacet = NULL, 
           lambda = 0.67,
           omega = NULL,
           nperm = 100){
  
  set.seed(seed)
  if(design == "null")
  {
     sim_fun = function(nx, nfacet, mean, sd, omega){
      rep(distributional::dist_normal(mean, sd), nx*nfacet)
    }
  }
  else if (design == "vary_x"){
    
  }
  
  
  
    
  }
    
           