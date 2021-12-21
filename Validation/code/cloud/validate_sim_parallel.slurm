#!/bin/bash
#SBATCH -n 8 													# Number of cores requested
#SBATCH -N 1 													# Ensure that all cores are on one machine
#SBATCH -t 1440													# Runtime in minutes
#SBATCH -p shared												# Partition to submit to
#SBATCH --mem-per-cpu=5000 										# Memory per cpu in MB (see also --mem-per-cpu)
#SBATCH --open-mode=append		 	                             

source new-modules.sh
module load gcc/10.2.0-fasrc01
module load R/4.1.0-fasrc01
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER
R CMD BATCH --quiet --no-restore --no-save /n/home01/khunter33/Power-Under-Multiplicity/Validation/Simulations/gen_validation_results_all.R /n/home01/khunter33/Power-Under-Multiplicity/log/$(date +"%Y%m%d_%H%M%S%N").out



