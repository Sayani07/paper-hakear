# Distributions of raw and normalised distance measure

These files are large and hence added in .gitignore. They could be generated using the scripts in the  null directory either through a SLURM job (job.sh) or through the Rscript raw_mmpd.R or norm_mmpd.R

The filename is made up of 2 parts
- number of x levels
- number of facet levels

Each file is an rds file containing a single R object. These contain 200 simulations of the corresponding x and facet levels.
For example, raw/14_20_dist.rds would mean it contains the raw distance measure median(max) for 14 x-axis categories and 20 facet categories run for 200 times.