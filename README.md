# Power-Under-Multiplicity


### Folder Structure

* __Validation__: 
     * __data__
          * stored validation results
     * __Simulations:__ 
          * __adjust_WY.R__: Code specific to Westfall-young adjustments
          * __estimate_power_with_simulation.R__: Estimating power with Monte Carlo Simulations
          * __gen_data.R__: Data generating process for simulated data
          * __gen_validation_results_all.R__: Script that runs all validation scenarios
          * __gen_validation_results_one.R__: Script that runs a single validation scenario
          * __misc.R__: supporting functions
          * __params.R__: universal location for simulation and DGP parameters
          * __validate_power.R__: wrapper functions that calculate power by simulation, pump, powerup
          * __readme.txt__: reminder for how to run all validation scenarios on the cloud
          * __run_sim.txt__: bash script to run all validation scenarios in parallel on the cloud
          * __validate_sim_parallel.slurm__: slurm file for running all simulations in parallel on the cloud
          * __validate_pum.slurm__: slurm file for running PUM and PUP results on the cloud
      * __Validation files__:
          * __validate_format__: Markdown file that outlines the format shared across all validation results files
          * __individual files__: blocked_c2_3fr, blocked_i1_2cfr, blocked_i1_3r, cluster_c2_2r, cluster_c3_3r
          * __validate_wy__: WY validation across all designs
     * __appendix__:
          * draft of appendix outlining validation strategy and results

* __Admin__: _Program_Map.xlsx is here. Please ignore the rest!_
* __Documentation__:
     * __power_estimation_methods__: Fundamental document outlining all designs, parameters, and data generating process.

_Remaining folders and their content:_ 
_Note: If you run into getting the right packages, please let me know._ 

* __DemoPUM__: RShiny Code. R Shiny is launched from here.
* __tests__: unit tests from testthat package.
* __Paper Outline__: draft of paper outline.
