# Scripts for running several simulations of raw and normalised distance measures to understand their distribution


- The file raw_mmpd.R is a script that computes raw median(max) pairwise distances for two cyclic granularities.

- The file norm_mmpd.R is a script that computes normalised median(max) pairwise distances for two cyclic granularities.

- The file raw_pairwise_max.R is a script that computes all possible pairwise distances and upwrights within facet and downweights between facet pairwise distances for two cyclic granularities.

- The file raw_pairwise_data.R is a script that computes all possible pairwise distances for two cyclic granularities and share the raw distances without any aggregation.

- All job.sh files can be submitted onto a SLURM cluster.

- The file simtable.R constructs a table lining up each simulation scenario with each row number matched to the slurm task id in job.sh.

- The file simtable.csv is a table produced by simtable.R