**Objective:** 1. Behavior of wpd under different null designs N(0,1), N(5, 1), N(0,5) and N(5,5), Gamma(0,1), Gamma(0,5) for different nx and nfacets. Here, we have to fix a value of lambda = 0.67.

**Assumptions:** There is no difference in distribution between any facet or x-category
nsim = 200
lambda = 0.67

**Questions:** 
 - How raw value of wpd changes with different nx and nfacet for different location and scale of a Normal and non-normal distribution

**Folder structure:**  
 - `R`:  R files for running raw and norm wpd  
 - `job`:  slurm job .sh files calling R scripts for running raw and norm wpd in MonARCH  
 - `data`:  output files from R/job  
 - `result_report`: R files for aggregation and generating plots  
 - `figs`: plots generated for files aggregated data for each simulation scenario  


