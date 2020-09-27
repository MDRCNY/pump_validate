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
library(ggplot2)
library(here)        # for relative file paths
library(lme4)        # for modeling
library(MASS)
library(multtest)    # Multiple Testing Procedures package
library(nlme)
library(pkgcond)     # for suppress_messages
library(PowerUpR)    # for checking with another power estimation function
library(randomizr)   # for treatment assignment
library(RcppEigen)   # rcpp for speed issues
library(reshape2)
library(rlist)
library(snow)        # for parallel coding
library(tibble)      # a modern take on data frames
library(tictoc)      # for timing

################
# choose whether to load package code or local code
# source(here::here("Methods", "utils.R"))
# source(here::here("Methods", "blocked_i1_2cfr.R"))

# to install pum from github, generate a personal authentication token 'foo'
# at https://github.com/settings/tokens
# then run
# devtools::install_github('MDRCNY/pum-p', auth_token = 'foo')
library(pum)         # for checking with the new methods
################

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
validate_power <- function(user.params.list, sim.params.list, design, overwrite = TRUE, gen.wide.results = FALSE) {

  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(user.params.list, sim.params.list, design)
  print(paste('Power validation for:', params.file.base))
  
  current.file = find_file(params.file.base, type = 'power')
  
  if(overwrite | length(current.file) == 0)
  {
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
    
    if(design %in% c('blocked_i1_2c', 'blocked_i1_2f', 'blocked_i1_2r'))
    {
      power_up_results <- power.bira2c1(
        es = user.params.list[['ATE_ES']][1],
        alpha = sim.params.list[['alpha']],
        two.tailed = TRUE,
        p = sim.params.list[['p.j']],
        g1 = 1,
        r21 = user.params.list[['R2.1']][1],
        n = user.params.list[['n.j']],
        J = user.params.list[['J']]
      )
    } else if(design %in% c('simple_c2_2r'))
    {
      power_up_results <- power.cra2r2(
        es = user.params.list[['ATE_ES']][1],
        alpha = sim.params.list[['alpha']],
        two.tailed = TRUE,
        g2 = 1,
        p = sim.params.list[['p.j']],
        rho2 = user.params.list[['ICC.2']][1],
        r21 = user.params.list[['R2.1']][1],
        r22 = user.params.list[['R2.2']][1],
        n = user.params.list[['n.j']],
        J = user.params.list[['J']]
      )
    } else {
      stop(paste('Unknown design:', design)) 
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
        
        if(design %in% c('blocked_i1_2c', 'blocked_i1_2f', 'blocked_i1_2r'))
        {
          pum_results <- power_blocked_i1_2c(
            M = user.params.list[['M']], MTP = MTP,
            MDES = user.params.list[['ATE_ES']], numFalse = user.params.list[['M']],
            J = user.params.list[['J']], n.j = user.params.list[['n.j']],
            p = sim.params.list[['p.j']],
            alpha = sim.params.list[['alpha']], numCovar.1 = 0, numCovar.2 = 0,
            R2.1 = user.params.list[['R2.1']], R2.2 = user.params.list[['R2.2']],
            ICC = user.params.list[['ICC']], sigma = NULL,
            rho = user.params.list[['rho.default']], omega = NULL,
            tnum = sim.params.list[['tnum']], snum = sim.params.list[['B']],
            ncl = sim.params.list[['ncl']]
          )
        } else if(design %in% c('simple_c2_2r'))
        {
          stop(paste('Unknown design:', design))
        } else {
          stop(paste('Unknown design:', design)) 
        }
        
        pum_results <- data.frame(pum_results)
        if (iterator == 0) {
          pum_combined_results <- pum_results
        } else {
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
    
    compare.filename <- paste0(params.file.base, "comparison_power_results.RDS")
    if(gen.wide.results)
    {
      compare_results <- gen.combined.results.wide(pum_combined_results, power_up_results, sim_results) 
    } else
    {
      compare_results <- gen.combined.results.long(pum_combined_results, power_up_results, sim_results)
    }
    
    saveRDS(compare_results, file = here::here("Validation/data", compare.filename))
    
    return(compare_results)
  } else
  {
    print('Validation already completed.')
  }
} # validate_power

#' Estimating MDES
#'
#'
#' @param user.params.list list of user-inputted parameters that feed into the DGP
#' @param sim.params.list list of simulation parameters
#' @param design RCT design (see list/naming convention)
#'
#' @return a whole series of MDES data files for validation
#' @export
#'
#' @examples
validate_mdes <- function(user.params.list, sim.params.list, design, overwrite = TRUE) {
  
  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(user.params.list, sim.params.list, design)
  print(paste('MDES validation for:', params.file.base))
  
  current.file = find_file(params.file.base, type = 'mdes')
  
  if(overwrite | length(current.file) == 0)
  {
    procs <- sim.params.list[['procs']]
    if(!("rawp" %in% sim.params.list[['procs']]))
    {
      procs = c("rawp", procs)
    }
    
    power.results = find_file(params.file.base, type = 'power')
    if(length(power.results) == 0)
    {
      stop(paste('Power results table needed for params:', params.file.base))
    }
    
    mdes_compare_results <- NULL
    for (MTP in procs){
      mdes_results <- mdes_blocked_i1_2c(
        power = power.results[power.results$MTP == MTP & power.results$power_type == 'indiv' & power.results$method == 'pum', 'value'],
        MTP = MTP,
        # fixed parameters
        M = user.params.list[['M']],
        numFalse = user.params.list[['M']],
        J = user.params.list[['J']],
        n.j = user.params.list[['n.j']],
        power.definition = "indiv",
        marginError = sim.params.list[['MoE']],
        p = sim.params.list[['p.j']],
        alpha = sim.params.list[['alpha']],
        numCovar.1 = 1, numCovar.2 = 1,
        R2.1 = user.params.list[['R2.1']][1], R2.2 = user.params.list[['R2.2']][1],
        ICC = user.params.list[['ICC.2']][1],
        mod.type = "constant",
        rho = user.params.list[['rho.default']],
        omega = user.params.list[['omega.2']],
        tnum = sim.params.list[['tnum']], snum = sim.params.list[['B']],
        ncl = sim.params.list[['ncl']],
        max.iter = sim.params.list[['max.iter']]
      )
      mdes_compare_results <- rbind(mdes_compare_results, mdes_results)
    }
    compare.filename <- paste0(params.file.base, "comparison_mdes_results.RDS")
    
    mdes_compare_results[,2:3] <- apply(mdes_compare_results[,2:3], 2, as.numeric)
    mdes_compare_results[,'Targeted MDES'] <- user.params.list[['ATE_ES']][1]
    rownames(mdes_compare_results) <- NULL
    
    saveRDS(mdes_compare_results, file = here::here("Validation/data", compare.filename))
    return(mdes_compare_results)
  } else
  {
    print('Validation already completed.')
  }
  
} # validate_mdes


#' Estimating sample size
#'
#'
#' @param user.params.list list of user-inputted parameters that feed into the DGP
#' @param sim.params.list list of simulation parameters
#' @param design RCT design (see list/naming convention)
#'
#' @return a whole series of sample size data files for validation
#' @export
#'
#' @examples
validate_sample <- function(user.params.list, sim.params.list, design, overwrite = TRUE) {
  
  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(user.params.list, sim.params.list, design)
  print(paste('Sample validation for:', params.file.base))
  
  current.file = find_file(params.file.base, type = 'sample')
  
  if(overwrite | length(current.file) == 0)
  {
    
    procs <- sim.params.list[['procs']]
    if(!("rawp" %in% sim.params.list[['procs']]))
    {
      procs = c("rawp", procs)
    }
    
    power.results = find_file(params.file.base, type = 'power')
    if(length(power.results) == 0)
    {
      stop(paste('Power results table needed for params:', params.file.base))
    }
    
    sample_compare_results <- NULL
    for(type in c('J', 'n.j'))
    {
      for (MTP in procs)
      {
        sample_results <- sample_blocked_i1_2c(
          power = power.results[power.results$MTP == MTP & power.results$power_type == 'indiv' & power.results$method == 'pum', 'value'],
          MTP = MTP,
          typesample = type,
          # fixed parameters
          MDES = user.params.list[['ATE_ES']][[1]],
          M = user.params.list[['M']],
          numFalse = user.params.list[['M']],
          J = user.params.list[['J']],
          n.j = user.params.list[['n.j']],
          power.definition = "indiv",
          marginError = sim.params.list[['MoE']],
          p = sim.params.list[['p.j']],
          alpha = sim.params.list[['alpha']],
          numCovar.1 = 1, numCovar.2 = 1,
          R2.1 = user.params.list[['R2.1']][1], R2.2 = user.params.list[['R2.2']][1],
          ICC = user.params.list[['ICC.2']][1],
          mod.type = "constant",
          rho = user.params.list[['rho.default']],
          omega = user.params.list[['omega.2']],
          tnum = sim.params.list[['tnum']], snum = sim.params.list[['B']],
          ncl = sim.params.list[['ncl']],
          max.iter = sim.params.list[['max.iter']]
        )
        sample_results$type <- type
        sample_compare_results <- rbind(sample_compare_results, sample_results)
      }
    }
    sample_compare_results[,3:4] = apply(sample_compare_results[,3:4], 2, as.numeric)
    compare.filename <- paste0(params.file.base, "comparison_sample_results.RDS")
    saveRDS(sample_compare_results, file = here::here("Validation/data", compare.filename))
    return(sample_compare_results)
  } else
  {
    print('Validation already completed.')
  }

} # validate_sample

### DEBUG
if(FALSE)
{
  MTP = 'Bonferroni';
  power = power.results[power.results$MTP == MTP & power.results$power_type == 'indiv' & power.results$method == 'pum', 'value'];
  MTP = MTP;
  M = user.params.list[['M']];
  numFalse = user.params.list[['M']];
  J = user.params.list[['J']];
  n.j = user.params.list[['n.j']];
  power.definition = "indiv";
  marginError = sim.params.list[['MoE']];
  p = sim.params.list[['p.j']];
  alpha = sim.params.list[['alpha']];
  numCovar.1 = 1; numCovar.2 = 1;
  R2.1 = user.params.list[['R2.1']][1]; R2.2 = user.params.list[['R2.2']][1];
  ICC = user.params.list[['ICC.2']][1];
  mod.type = "constant";
  rho = user.params.list[['rho.default']];
  omega = user.params.list[['omega.2']];
  tnum = sim.params.list[['tnum']]; snum = sim.params.list[['B']];
  ncl = sim.params.list[['ncl']];
  max.iter = sim.params.list[['max.iter']];
  updateProgress = NULL;
  typesample = 'J';
  J0 = 10; n.j0 = 10;
  MDES = user.params.list[['ATE_ES']][[1]];
  two.tailed = TRUE; max.iter = 100; tol = 0.1
}