#' ---
#' title: "Monte Carlo Simulation Code"
#' author: "Kristin Porter, Deni Chen and Zarni Htet"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: html_notebook
#' ---
#'

# Installing and Loading Libraries from Bioconductor package
# if (!requireNamespace("BiocManager", quietly = TRUE)){
#   install.packages("BiocManager")
# }
# BiocManager::install("multtest")

# Loading the libraries
library(dplyr)       # for combing data frames
library(here)        # for relative file paths
library(lme4)        # for modeling
library(MASS)
library(multtest)   # Multiple Testing Procedures package
library(nlme)
library(PowerUpR)    # for checking with another power estimation function
library(pum)         # for checking with the new methods
library(RcppEigen)   # rcpp for speed issues
library(snow)        # for parallel coding
library(tibble)      # a modern take on data frames
library(tictoc)      # for timing

#' Estimating Power through simulations
#'
#' Loop through different simulations like in Table C.3 of the paper
#'
#' @param user.params.list list of user-inputted parameters that feed into the DGP
#' @param sim.params.list list of simulation parameters
#' @param design RCT design (see list/naming convention)
#'
#' @return a whole series of power data files for validation
#' @export
#'
#' @examples
validate_power <- function(user.params.list, sim.params.list, design) {

  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(user.params.list, sim.params.list, design)

  #####################
  # Simulation Values #
  #####################

  # simulate and run power calculations
  sim.filename = paste0(params.file.base, "simulation_results.RDS")
  if(sim.params.list[['runSim']]){
    sim_results <- est_power_sim(user.params.list, sim.params.list, design)
    saveRDS(sim_results, file = here("Validation/data", sim.filename))
  } else {
    sim_results <- readRDS(file = here::here("Validation/data", sim.filename))
  }

  ###################
  # Power Up Values #
  ###################

  if(grepl('block', design, ignore.case = TRUE))
  {
    power_up_results <- quiet(
      power.bira2c1(
        es = user.params.list[['MDES']][1], alpha = sim.params.list[['alpha']],
        two.tailed = TRUE,
        p = sim.params.list[['p.j']], g1 = 1, r21 = user.params.list[['R2.1']][1],
        n = user.params.list[['n.j']], J = user.params.list[['J']]
      ))
  }
  # Power_Up_Standard_Error
  power_up_results$se       <- power_up_results$parms$es/power_up_results$ncp
  power_up_results$lower_ci <- power_up_results$power - (1.96 * power_up_results$se)
  power_up_results$upper_ci <- power_up_results$power + (1.96 * power_up_results$se)

  ######################
  # PUMP methods value #
  ######################
  pump.filename <- paste0(params.file.base, "pump_results.RDS")
  if(sim.params.list[['runPump']]){

    iterator <- 0
    pum_combined_results <- NULL

    for (MTP in sim.params.list[['procs']]){

      if(grepl('block', design, ignore.case = TRUE))
      {
        pum_results <- pum::power_blocked_i1_2c(
          M = user.params.list[['M']], MTP = MTP,
          MDES = user.params.list[['MDES']], numFalse = user.params.list[['M']],
          J = user.params.list[['J']], n.j = user.params.list[['n.j']],
          p = sim.params.list[['p.j']],
          alpha = sim.params.list[['alpha']], numCovar.1 = 0, numCovar.2 = 0,
          R2.1 = user.params.list[['R2.1']], R2.2 = user.params.list[['R2.2']],
          ICC = user.params.list[['ICC']], sigma = NULL,
          rho = user.params.list[['rho.default']], omega = NULL,
          tnum = sim.params.list[['tnum']], snum = sim.params.list[['B']],
          ncl = sim.params.list[['ncl']]
        )
      }
      pum_results <- data.frame(pum_results)
      if (iterator == 0){
        pum_combined_results <- pum_results
      }else{
        pum_combined_results <- dplyr::bind_rows(pum_combined_results, pum_results[2,])
      }
      iterator = iterator + 1
    }
    # adding rownames to the pum_combined_results table
    rownames(pum_combined_results) <- c("rawp", sim.params.list[['procs']])
    saveRDS(pum_combined_results, file = here::here("Validation/data", pump.filename))
  } else {
    pum_combined_results <- readRDS(file = here::here("Validation/data", pump.filename))
  }

  compare.filename <- paste0(params.file.base, "comparison_results.RDS")
  compare_results <- gen.results.table(pum_combined_results, power_up_results, sim_results)
  saveRDS(compare_results, file = here::here("Validation/data", compare.filename))

  return(compare_results)

} # validate_power
