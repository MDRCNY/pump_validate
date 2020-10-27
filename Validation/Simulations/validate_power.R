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
library(foreach)
library(ggplot2)
library(here)        # for relative file paths
library(lme4)        # for modeling
library(MASS)
library(multtest)    # Multiple Testing Procedures package
library(nlme)
library(parallel)
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
source(here::here("Methods", "utils.R"))
source(here::here("Methods", "blocked_i1_2cfr.R"))

# to install pum from github, generate a personal authentication token 'foo'
# at https://github.com/settings/tokens
# then run
# devtools::install_github('MDRCNY/pum-p', auth_token = 'foo')
# library(pum)         # for checking with the new methods
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
validate_power <- function(user.params.list, sim.params.list, design, q = 1, overwrite = TRUE, gen.wide.results = FALSE) {

  # design = "blocked_i1_2c"
  
  t1 = Sys.time()
  
  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(user.params.list, sim.params.list, design)
  message(paste('Power validation for:', params.file.base))
  
  current.file = find_file(params.file.base, type = 'power')
  
  if(overwrite | length(current.file) == 0)
  {

    if(sim.params.list[['parallel']])
    {
      cl <- makeSOCKcluster(rep("localhost", sim.params.list[['ncl']]))
    } else
    {
      cl <- NULL
    }
    
    #####################
    # Simulation Values #
    #####################
    
    # simulate and run power calculations
    adjp.filename = paste0(params.file.base, "adjp_", q, ".RDS")
    if(sim.params.list[['runSim']]){
      message('Running simulation')
      adjp.proc <- est_power_sim(user.params.list, sim.params.list, design, cl)
      saveRDS(adjp.proc, file = here("Validation/data", adjp.filename))
    } else {
      adjp.files = grep(paste0(params.file.base, 'adjp_'), list.files(here("Validation/data")), value = TRUE)
      if(length(adjp.files) > 0)
      {
        message(paste('Reading in simulation adjp results.', length(adjp.files), 'results files found.'))
        adjp.proc <- readRDS(file = here::here("Validation/data", adjp.files[1]))
        for(adjp.file in adjp.files[2:length(adjp.files)])
        {
          adjp.proc.q <- readRDS(file = here::here("Validation/data", adjp.file))
          adjp.proc <- abind(adjp.proc, adjp.proc.q, along = 1)
        }
      } else
      {
        warning(paste('New simulation adjp results not run, no simulation adjp results found for parameters:', params.file.base))
        adjp.proc <- NULL
      }
    }
    
    # if we have all the iterations, save it out!
    if(!is.null(adjp.proc) & dim(adjp.proc)[1] == sim.params.list[['S']]*sim.params.list[['Q']])
    {
      sim.filename = paste0(params.file.base, "simulation_results.RDS")
      sim_results <- calc_power(adjp.proc, user.params.list, sim.params.list)
      saveRDS(sim_results, file = here("Validation/data", sim.filename))
    } else
    {
      warning(paste(
        'Number of iterations loaded does not match parameters provided. Number of adjusted p-values:',
        dim(adjp.proc)[1], '. S =', sim.params.list[['S']], ', Q =', sim.params.list[['Q']]
      ))
      sim_results <- NULL
    }
    
    ###################
    # Power Up Values #
    ###################
    powerup.filename <- paste0(params.file.base, "powerup_results.RDS")
    if(sim.params.list[['runPowerUp']])
    {
      message('Running PowerUp')
      if(design %in% c('blocked_i1_2c', 'blocked_i1_2f', 'blocked_i1_2r'))
      {
        powerup_results <- power.bira2c1(
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
        powerup_results <- power.cra2r2(
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
      powerup_results$se       <- powerup_results$parms$es/powerup_results$ncp
      powerup_results$lower_ci <- powerup_results$power - (1.96 * powerup_results$se)
      powerup_results$upper_ci <- powerup_results$power + (1.96 * powerup_results$se)
      
      powerup_results <- data.frame(
        MTP = 'rawp',
        variable = 'indiv',
        method = 'pup',
        value = c(powerup_results$power, powerup_results$lower_ci, powerup_results$upper_ci),
        value.type = c('adjusted_power', 'ci_lower',  'ci_upper')
      )
      saveRDS(powerup_results, file = here("Validation/data", powerup.filename))
    } else
    {
      if(file.exists(here::here("Validation/data", powerup.filename)))
      {
        message('Reading in PowerUp results')
        powerup_results <- readRDS(file = here::here("Validation/data", powerup.filename))
      } else
      {
        warning(paste('PowerUp results not run, no PowerUp results found for parameters:', params.file.base))
        powerup_results <- NULL
      }
      
    }
    
    ######################
    # PUMP methods value #
    ######################
    pump.filename <- paste0(params.file.base, "pump_results.RDS")
    if(sim.params.list[['runPump']]){
      
      message('Running PUMP')
      
      iterator <- 0
      pum_combined_results <- NULL
      
      for (MTP in sim.params.list[['procs']]){
        
        if(design %in% c('blocked_i1_2c', 'blocked_i1_2f', 'blocked_i1_2r'))
        {
          pum_results_iter <- power_blocked_i1_2c(
            M = user.params.list[['M']], MTP = MTP,
            MDES = user.params.list[['ATE_ES']],
            J = user.params.list[['J']], n.j = user.params.list[['n.j']],
            p = sim.params.list[['p.j']],
            alpha = sim.params.list[['alpha']], numCovar.1 = 0, numCovar.2 = 0,
            R2.1 = user.params.list[['R2.1']], R2.2 = user.params.list[['R2.2']],
            ICC = user.params.list[['ICC']], sigma = NULL,
            rho = user.params.list[['rho.default']], omega = NULL,
            tnum = sim.params.list[['tnum']], snum = sim.params.list[['B']],
            cl = cl
          )
        } else if(design %in% c('simple_c2_2r'))
        {
          stop(paste('Unknown design:', design))
        } else {
          stop(paste('Unknown design:', design)) 
        }
        
        pum_results_iter <- data.frame(pum_results_iter)
        if (iterator == 0) {
          pum_results <- pum_results_iter
        } else {
          pum_results <- dplyr::bind_rows(pum_results, pum_results_iter[2,])
        }
        iterator = iterator + 1
      }
      # adding rownames to the pum_combined_results table
      rownames(pum_results) <- c("rawp", sim.params.list[['procs']])
      
      pum_results_table <- data.frame(pum_results[,c('indiv', 'min1', 'min2', 'complete')])
      pum_results_table$MTP <- rownames(pum_results_table)
      pum_results <- melt(pum_results_table, id.vars = 'MTP')
      pum_results$method = 'pum'
      pum_results$value.type = 'adjusted_power'
      
      saveRDS(pum_results, file = here::here("Validation/data", pump.filename))
    } else {
      if(file.exists(here::here("Validation/data", pump.filename)))
      {
        message('Reading in PUMP results')
        pum_results <- readRDS(file = here::here("Validation/data", pump.filename))
      } else
      {
        warning(paste('PUMP results not run, no PUMP found for parameters:', params.file.base))
        pum_results <- NULL
      }

    }
  
    if(!is.null(sim_results) | !is.null(powerup_results) | !is.null(sim_results))
    {
      compare.filename <- paste0(params.file.base, "comparison_power_results.RDS")
      compare_results_long <- data.frame(rbind(pum_results, powerup_results, sim_results))
      colnames(compare_results_long) <- c('MTP', 'power_type', 'value', 'method', 'value.type')
      compare_results <- compare_results_long[,c('MTP', 'power_type','method', 'value.type', 'value')]
      saveRDS(compare_results, file = here::here("Validation/data", compare.filename))
    } else
    {
      compare_results <- NULL
    }

    if(sim.params.list[['parallel']])
    {
      parallel::stopCluster(cl)
    }
    
    t2 = Sys.time()
    message(paste('Total time:', difftime(t2, t1, units = 'mins'), 'minutes'))

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
  
  if(sim.params.list[['parallel']])
  {
    cl <- makeSOCKcluster(rep("localhost", sim.params.list[['ncl']]))
  } else
  {
    cl <- NULL
  }
  
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
    
    power.file = find_file(params.file.base, type = 'power')
    if(length(power.file) == 0)
    {
      stop(paste('Power results table needed for params:', params.file.base))
    }
    power.results = readRDS(power.file)
    
    mdes_compare_results <- NULL
    for (MTP in procs){
      mdes_results <- mdes_blocked_i1_2c(
        power = power.results[power.results$MTP == MTP & power.results$power_type == 'indiv' & power.results$method == 'pum', 'value'],
        MTP = MTP,
        # fixed parameters
        M = user.params.list[['M']],
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
        parallel = sim.params.list[['parallel']], ncl = sim.params.list[['ncl']],
        cl = cl,
        max.iter = sim.params.list[['max.iter']]
      )
      mdes_compare_results <- rbind(mdes_compare_results, mdes_results)
    }
    compare.filename <- paste0(params.file.base, "comparison_mdes_results.RDS")
    
    mdes_compare_results[,2:3] <- apply(mdes_compare_results[,2:3], 2, as.numeric)
    mdes_compare_results = cbind(mdes_compare_results, user.params.list[['ATE_ES']][1])
    colnames(mdes_compare_results) = c('MTP', 'Adjusted MDES', 'Indiv Power', 'Targeted MDES')
    rownames(mdes_compare_results) <- NULL
    
    if(sim.params.list[['parallel']])
    {
      parallel::stopCluster(cl)
    }
    
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
    
    if(sim.params.list[['parallel']])
    {
      cl <- makeSOCKcluster(rep("localhost", sim.params.list[['ncl']]))
    } else
    {
      cl <- NULL
    }
    
    procs <- sim.params.list[['procs']]
    if(!("rawp" %in% sim.params.list[['procs']]))
    {
      procs = c("rawp", procs)
    }
    
    power.file = find_file(params.file.base, type = 'power')
    if(length(power.file) == 0)
    {
      stop(paste('Power results table needed for params:', params.file.base))
    }
    power.results = readRDS(power.file)
    
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
          MDES = user.params.list[['ATE_ES']][1],
          M = user.params.list[['M']],
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
          max.iter = sim.params.list[['max.iter']],
          cl = cl
        )
        sample_results$type <- type
        sample_compare_results <- rbind(sample_compare_results, sample_results)
      }
    }
    sample_compare_results[,3:4] = apply(sample_compare_results[,3:4], 2, as.numeric)
    compare.filename <- paste0(params.file.base, "comparison_sample_results.RDS")
    
    if(sim.params.list[['parallel']])
    {
      parallel::stopCluster(cl)
    }
    
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
  M = user.params.list[['M']];
  MDES = rep(user.params.list[['ATE_ES']], M);
  J = user.params.list[['J']];
  n.j = user.params.list[['n.j']];
  power.definition = "indiv";
  marginError = sim.params.list[['MoE']];
  p = sim.params.list[['p.j']];
  alpha = sim.params.list[['alpha']];
  numCovar.1 = 1; numCovar.2 = 1;
  R2.1 = user.params.list[['R2.1']];
  R2.2 = user.params.list[['R2.2']];
  ICC = user.params.list[['ICC.2']];
  mod.type = "constant";
  rho = user.params.list[['rho.default']];
  omega = user.params.list[['omega.2']];
  tnum = sim.params.list[['tnum']]; snum = sim.params.list[['B']];
  parallel = sim.params.list[['parallel']]; ncl = sim.params.list[['ncl']];
  max.iter = sim.params.list[['max.iter']];
  updateProgress = NULL;
  typesample = 'J';
  J0 = 10; n.j0 = 10;
  two.tailed = TRUE; max.iter = 100; tol = 0.1;
  # cl <- makeSOCKcluster(rep("localhost", sim.params.list[['ncl']]))
  cl <- NULL
}
