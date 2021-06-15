# Detecting distributional differences between temporal granularities for exploratory time series analysis

_Repository to paper, simulation results and all scripts for the project_

- _Paper_: paper/main.pdf
- _Scripts (Paper)_: paper/scripts/main.R
- _Supplementary_: paper/paper-supplementary.pdf
- _Simulations_: simulations/


**General folder structure for simulations**  
 - `R-ind`:  individual R files (e.g.raw, norm, supplementary/test-hpc)  
 - `job`:  slurm job .sh files calling `R-ind` scripts in MonARCH  
 - `data-ind`:  output files from `job`  
 - `R-agg`:  R files for aggregating outputs from `data-ind` into `data-agg` and plotting  
 - `data-agg`:  output files from `R-agg`  
 - `figs`: plots generated from `data-agg` for each simulation scenario  

