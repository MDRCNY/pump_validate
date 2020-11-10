#!/bin/bash
#SBATCH -n 8 													# Number of cores requested
#SBATCH -N 1 													# Ensure that all cores are on one machine
#SBATCH -t 120													# Runtime in minutes
#SBATCH -p test												    # Partition to submit to
#SBATCH --mem-per-cpu=500 										# Memory per cpu in MB (see also --mem-per-cpu)
#SBATCH --open-mode=append
#SBATCH --mail-type=ALL        									# Type of email notification- BEGIN,END,FAIL,ALL
#SBATCH --mail-user=kristenbhunter@gmail.com 					# Email

source new-modules.sh
module load gcc/7.1.0-fasrc01
module load R
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER
R CMD BATCH --quiet --no-restore --no-save /n/home01/khunter33/Power-Under-Multiplicity/Validation/Simulations/gen_validation_results_all.R /n/home01/khunter33/Power-Under-Multiplicity/$(date +"%Y%m%d_%H%M%S").out



