#! /bin/bash
#SBATCH --job-name=size_small
#SBATCH --output=trace_%a.out
#SBATCH --error=error_%a.err
#SBATCH --mem-per-cpu=4096
#SBATCH --time=168:00:00
#SBATCH --ntasks=1
#SBATCH --constraint=Xeon-Platinum-8260
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=Sayani.Gupta@monash.edu
#SBATCH --export=NONE
module load R/4.0.0-openblas
Rscript size_small.R 
