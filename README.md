# Validation repo

This repo contains code and results that validate the power results from the [PUMP package](https://github.com/MDRCNY/PUMP).

This package compares the PUMP package power estimation with a simulation-based approach to validate that the
power calculations are correct.

Development of this project was supported by a grant from the Institute of Education Sciences (R305D170030).

Authors:
  - Kristen Hunter
  - Luke Miratrix
  - Kristin Porter
  - Zarni Htet

## Validation results

The complete set of validation results can be found in validation/results.
This folder contains Rmarkdown files and output PDF files for all validation scenarios.
Each file shows graphs comparing power estimated using monte carlo simulations compared to PUMP and PowerUP.
Note that PowerUp only works with a single outcome, M = 1.
For a general guide to the format of these documents, see validatate_format.

The script we used for running all validation scenarios is validation/code/gen_validation_results_all.R.
This script can take several days to run, depending on choice of simulation parameters.

## Running your own validation

To run your own validation of a specific scenario, use validation/code/gen_validation_results_one.R.  
Set the design, model, and parameters to your desired values.
For more information about these choices, see the PUMP package documentation.

You can choose what ways to calculate power to compare calculations:
- `runSim` runs Monte Carlo simulations to calculate power
- `runPump` runs the PUMP package to calculate power
- `runPowerUp` runs the Powerup package to calculate power. 

You can run `validate_power()`, `validate_mdes()`, or `validate_sample()`.
For `validate_sample()`, you can choose the appropriate sample type, `J`, `K`, or `nbar` depending on the design.

The functions return data frames with estimates and lower and upper confidence interval bounds (for monte carlo simulations) of the requested parameter.

### Folder structure

* __code__
     * __adjust_WY.R__: Code specific to Westfall-young adjustments
     * __estimate_power_with_simulation.R__: Estimating power with Monte Carlo Simulations
     * __gen_validation_results_all.R__: Script that runs all validation scenarios
     * __gen_validation_results_one.R__: Script that runs a single validation scenario
     * __misc.R__: supporting functions
     * __validate_power.R__: wrapper functions that calculate power by simulation, pump, powerup
     * __cloud__: contains code for running batch scripts in the cloud
          * __readme.txt__: reminder for how to run all validation scenarios on the cloud
          * __run_sim.txt__: bash script to run all validation scenarios in parallel on the cloud
          * __validate_sim_parallel.slurm__: slurm file for running all simulations in parallel on the cloud
          * __validate_pum.slurm__: slurm file for running PUM and PUP results on the cloud
* __results__
     * contains markdown documents and rendered PDFs of all validation scenarios.
     * for a general guide to the format of these documents, see validate_format.

