To run on odyssey:

1. Run simulations

Set params to:
runSim = TRUE 
runPump = FALSE
runPowerUp = FALSE

Do not run mdes and ss

Use code/cloud/run_sim.txt

2. Run Pump and Powerup and combine simulation results

runSim = FALSE
runPump = TRUE
runPowerUp = TRUE

In gen_validation_results_all.R
make sure overwrite = TRUE or delete all old files in Validation/data/*.RDS
If desired, run mdes and ss

sbatch code/cloud/validate_pum.slurm