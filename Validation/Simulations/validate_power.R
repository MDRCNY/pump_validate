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

# blkvar package
# install.packages("remotes")
# remotes::install_github("lmiratrix/blkvar")

# Loading the libraries
library(abind)
library(blkvar)
library(dplyr)       # for combing data frames
library(foreach)
library(ggplot2)
library(gridExtra)
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
# local code
# source(here::here("Methods", "utils.R"))
# source(here::here("Methods", "pump_power.R"))
# source(here::here("Methods", "pump_wy.R"))
# source(here::here("Methods", "utils.R"))
################
# install pum from github
# one way to install pum from github:
# generate a personal authentication token 'foo'
# at https://github.com/settings/tokens
# then run
# devtools::install_github('MDRCNY/pum-p', ref = 'main', auth_token = 'foo')
################
library(pum)
################


#' Estimating Power through simulations
#'
#' Loop through different simulations like in Table C.3 of the paper
#'
#' @param user.params.list list of user-inputted parameters that feed into the DGP
#' @param sim.params.list list of simulation parameters
#' @param design RCT design (see list/naming convention)
#' @param q Index of simulation iteration if parallelizing across simulations
#' @param overwrite If simulation output files already exist, whether to overwrite
#'
#' @return NULL. Saves out a series of simulation validation RDS files.
#' @export
#'
#' @examples
validate_power <- function(user.params.list, sim.params.list, design, q = 1, overwrite = TRUE)
{
  if( (user.params.list[['M']] == 1 & length(sim.params.list[['procs']]) > 1) |
      (user.params.list[['M']] == 1 & length(sim.params.list[['procs']]) == 1 & !('Bonferroni' %in% sim.params.list[['procs']] )))
  {
    stop(print("Multiple testing corrections are not needed when M = 1. Please change multiple testing procedures or increase M."))
  } 
  
  t1 = Sys.time()
  
  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(user.params.list, sim.params.list, design)
  message(paste('Power validation for:', params.file.base))
  
  current.file <- find_file(params.file.base, type = 'power')
  
  # store some files in intermediate results file
  data.dir <- here::here("Validation/data")
  intermediate.data.dir <- paste0(data.dir, "/intermediate_results/")
  if(!dir.exists(intermediate.data.dir))
  {
    dir.create(intermediate.data.dir)
  }
  
  # search for simulation results
  adjp.files <- grep(paste0(params.file.base, 'adjp_'), list.files(intermediate.data.dir), value = TRUE)
  
  # what if we only have some of expected files? then we force overwrite
  if( (length(adjp.files) > 0) & (length(adjp.files) != sim.params.list[['Q']]) )
  {
    overwrite = TRUE
  }
  
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
    adjp.filename <- paste0(params.file.base, "adjp_", q, ".RDS")
    if( (overwrite | length(adjp.files) == 0) & sim.params.list[['runSim']]){
      message('Running simulation')
      adjp.proc <- est_power_sim(user.params.list, sim.params.list, design, cl)
      saveRDS(adjp.proc, file = paste0(intermediate.data.dir, adjp.filename))
    } else {
      if(length(adjp.files) > 0)
      {
        message(paste('Reading in simulation adjp results.', length(adjp.files), 'results files found.'))
        adjp.proc <- readRDS(file = paste0(intermediate.data.dir, adjp.files[1]))
        if(length(adjp.files) > 1)
        {
          for(adjp.file in adjp.files[2:length(adjp.files)])
          {
            adjp.proc.q <- readRDS(file = paste0(intermediate.data.dir, adjp.file))
            adjp.proc <- abind(adjp.proc, adjp.proc.q, along = 1)
          }
        }
      } else
      {
        warning(paste('New simulation adjp results not run, no simulation adjp results found for parameters:', params.file.base))
        adjp.proc <- NULL
      }
    }
    
    # if we have all the iterations, save it out!
    if(!is.null(adjp.proc) && dim(adjp.proc)[1] == sim.params.list[['S']]*sim.params.list[['Q']])
    {
      sim.filename = paste0(params.file.base, "simulation_results.RDS")
      
      power.results <- NULL
      for(p in 1:dim(adjp.proc)[3])
      {
        proc.results <- get.power.results(
          pval.mat = adjp.proc[,,p],
          ind.nonzero = user.params.list[['ATE_ES']] > 0,
          alpha = sim.params.list[['alpha']]
        )
        power.results <- rbind(power.results, proc.results)
      }
      rownames(power.results) <- dimnames(adjp.proc)[[3]]
      
      # calculate confidence intervals
      se.power <- sqrt(0.25/sim.params.list[['S']]) 
      CI.lower.power <- power.results - 1.96 * se.power
      CI.upper.power <- power.results + 1.96 * se.power
      # make sure that intervals do not produce power below 0 or above 1
      for(i in 1:nrow(CI.lower.power))
      {
        for(j in 1:ncol(CI.lower.power))
        {
          CI.lower.power[i,j] <- max(CI.lower.power[i,j], 0)
          CI.lower.power[i,j] <- min(CI.lower.power[i,j], 1)
          CI.upper.power[i,j] <- max(CI.upper.power[i,j], 0)
          CI.upper.power[i,j] <- min(CI.upper.power[i,j], 1)
        }
      }
      
      adj_power <- list(power.results, CI.lower.power, CI.upper.power)
      names(adj_power) <- c("adjusted_power", "ci_lower", "ci_upper")
      
      get_adj_power_melt = function(adj_power)
      {
        adj_power <- data.frame(adj_power)
        adj_power$MTP <- rownames(adj_power)
        adj_power_melt <- melt(adj_power, id.vars = 'MTP')
        adj_power_melt$method = 'sim'
        return(adj_power_melt)
      }
      
      # bind results together
      sim_results <- list.rbind(lapply(adj_power, get_adj_power_melt))
      sim_results$value.type <- sapply(rownames(sim_results), function(x){strsplit(x, '\\.')[[1]][1]})
      
      saveRDS(sim_results, file = paste0(intermediate.data.dir, sim.filename))
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
    powerup.file <- paste0(intermediate.data.dir, powerup.filename)
    
    if( (overwrite | !file.exists(powerup.file))  & sim.params.list[['runPowerUp']])
    {
      message('Running PowerUp')
      if(design == 'd1.1_m2fc')
      {
        powerup_results <- power.bira2c1(
          es = user.params.list[['ATE_ES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g1 = 1,
          r21 = user.params.list[['R2.1']][1],
          n = user.params.list[['nbar']],
          J = user.params.list[['J']]
        )
      } else if(design == 'd2.1_m2fc')
      {
        powerup_results <- power.bira2c1(
          es = user.params.list[['ATE_ES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g1 = 1,
          r21 = user.params.list[['R2.1']][1],
          n = user.params.list[['nbar']],
          J = user.params.list[['J']]
        )
      } else if(design == 'd2.1_m2ff')
      {
        powerup_results <- power.bira2f1(
          es = user.params.list[['ATE_ES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g1 = 1,
          r21 = user.params.list[['R2.1']][1],
          n = user.params.list[['nbar']],
          J = user.params.list[['J']]
        )
      } else if(design == 'd2.1_m2fr')
      {
        powerup_results <- power.bira2r1(
          es = user.params.list[['ATE_ES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g2 = 1,
          rho2 = user.params.list[['ICC.2']][1],
          omega2 = user.params.list[['omega.2']],
          r21 = user.params.list[['R2.1']][1],
          r2t2 = 0,
          n = user.params.list[['nbar']],
          J = user.params.list[['J']]
        )
      } else if(design == 'd3.1_m3rr2rr')
      {
        powerup_results <- power.bira3r1(
          es = user.params.list[['ATE_ES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g3 = 1,
          rho2 = user.params.list[['ICC.2']][1],
          rho3 = user.params.list[['ICC.3']][1],
          omega2 = user.params.list[['omega.2']],
          omega3 = user.params.list[['omega.3']],
          r21 = user.params.list[['R2.1']][1],
          r2t2 = 0, r2t3 = 0,
          n = user.params.list[['nbar']],
          J = user.params.list[['J']],
          K = user.params.list[['K']]
        )
      } else if(design == c('d2.2_m2rc'))
      {
        powerup_results <- power.cra2r2(
          es = user.params.list[['ATE_ES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g2 = 1,
          rho2 = user.params.list[['ICC.2']][1],
          r21 = user.params.list[['R2.1']][1],
          r22 = user.params.list[['R2.2']][1],
          n = user.params.list[['nbar']],
          J = user.params.list[['J']]
        )
      } else if(design == c('d3.3_m3rc2rc'))
      {
        powerup_results <- power.cra3r3(
          es = user.params.list[['ATE_ES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g3 = 1,
          rho2 = user.params.list[['ICC.2']][1],
          rho3 = user.params.list[['ICC.3']][1],
          r21 = user.params.list[['R2.1']][1],
          r22 = user.params.list[['R2.2']][1],
          r23 = user.params.list[['R2.3']][1],
          n = user.params.list[['nbar']],
          J = user.params.list[['J']],
          K = user.params.list[['K']]
        )
      } else if(design == c('d3.2_m3ff2rc'))
      {
        powerup_results <- power.bcra3f2(
          es = user.params.list[['ATE_ES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g2 = 1,
          rho2 = user.params.list[['ICC.2']][1],
          r21 = user.params.list[['R2.1']][1],
          r22 = user.params.list[['R2.2']][1],
          n = user.params.list[['nbar']],
          J = user.params.list[['J']],
          K = user.params.list[['K']]
        )
      } else if(design == c('d3.2_m3rr2rc'))
      {
        powerup_results <- power.bcra3r2(
          es = user.params.list[['ATE_ES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g3 = 1,
          rho2 = user.params.list[['ICC.2']][1],
          rho3 = user.params.list[['ICC.3']][1],
          omega3 = user.params.list[['omega.3']],
          r21 = user.params.list[['R2.1']][1],
          r22 = user.params.list[['R2.2']][1],
          r2t3 = 0,
          n = user.params.list[['nbar']],
          J = user.params.list[['J']],
          K = user.params.list[['K']]
        )
      } else {
        stop(paste('Unknown design:', design)) 
      }
      # Power_Up_Standard_Error
      # powerup_results$se       <- powerup_results$parms$es/powerup_results$ncp
      # powerup_results$lower_ci <- powerup_results$power - (1.96 * powerup_results$se)
      # powerup_results$upper_ci <- powerup_results$power + (1.96 * powerup_results$se)
      
      powerup_results <- data.frame(
        MTP = 'rawp',
        variable = 'D1indiv',
        method = 'pup',
        value = powerup_results$power,
        value.type = 'adjusted_power'
      )
      
      # powerup_results <- data.frame(
      #   MTP = 'rawp',
      #   variable = 'D1indiv',
      #   method = 'pup',
      #   value = c(powerup_results$power, powerup_results$lower_ci, powerup_results$upper_ci),
      #   value.type = c('adjusted_power', 'ci_lower',  'ci_upper')
      # )
      saveRDS(powerup_results, file = powerup.file)
    } else
    {
      if(file.exists(powerup.file))
      {
        message('Reading in PowerUp results')
        powerup_results <- readRDS(file = powerup.file)
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
    pump.file <- paste0(intermediate.data.dir, pump.filename)
    
    if((overwrite | !file.exists(pump.file)) & sim.params.list[['runPump']])
    {
      
      message('Running PUMP')
      
      iterator <- 0
      pump_results <- NULL
      
      for (MTP in sim.params.list[['procs']]){
        pump_results_iter <- pump_power(
          design = design,
          MTP = MTP,
          MDES = user.params.list[['ATE_ES']],
          M = user.params.list[['M']], J = user.params.list[['J']], K = user.params.list[['K']],
          nbar = user.params.list[['nbar']],
          Tbar = sim.params.list[['Tbar']],
          alpha = sim.params.list[['alpha']],
          numCovar.1 = 1, numCovar.2 = 1, numCovar.3 = 1,
          R2.1 = user.params.list[['R2.1']], R2.2 = user.params.list[['R2.2']], R2.3 = user.params.list[['R2.3']],
          ICC.2 = user.params.list[['ICC.2']], ICC.3 = user.params.list[['ICC.3']],
          rho = user.params.list[['rho.default']],
          omega.2 = user.params.list[['omega.2']], omega.3 = user.params.list[['omega.3']],
          tnum = sim.params.list[['tnum']], B = sim.params.list[['B']],
          cl = cl
        )
        pump_results_iter <- data.frame(pump_results_iter)
        if (iterator == 0) {
          pump_results <- pump_results_iter
        } else {
          pump_results <- dplyr::bind_rows(pump_results, pump_results_iter[2,])
        }
        iterator = iterator + 1
      }
      # format results table nicely
      pump_results_table <- pump_results
      pump_results_table$MTP <- rownames(pump_results_table)
      pump_results <- melt(pump_results_table, id.vars = 'MTP')
      pump_results$method = 'pum'
      pump_results$value.type = 'adjusted_power'
      
      saveRDS(pump_results, file = pump.file)
    } else {
      if(file.exists(pump.file))
      {
        message('Reading in PUMP results')
        pump_results <- readRDS(pump.file)
      } else
      {
        warning(paste('PUMP results not run, no PUMP found for parameters:', params.file.base))
        pump_results <- NULL
      }
      
    }
    
    if(!is.null(sim_results) | !is.null(powerup_results) | !is.null(pump_results))
    {
      compare.filename <- paste0(params.file.base, "comparison_power_results.RDS")
      compare_results_long <- data.frame(rbind(pump_results, powerup_results, sim_results))
      colnames(compare_results_long) <- c('MTP', 'power_type', 'value', 'method', 'value.type')
      compare_results <- compare_results_long[,c('MTP', 'power_type','method', 'value.type', 'value')]
      saveRDS(compare_results, file = paste(data.dir, compare.filename, sep = "/"))
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

#' Validate MDES calculations
#'
#'
#' @param user.params.list list of user-inputted parameters that feed into the DGP
#' @param sim.params.list list of simulation parameters
#' @param design RCT design (see list/naming convention)
#' @param overwrite If simulation output files already exist, whether to overwrite
#'
#' @return NULL. Saves out a series of MDES validation RDS files.
#' @export
#'
#' @examples
validate_mdes <- function(user.params.list, sim.params.list, design,
                          power.definition = 'D1indiv', plot.path = FALSE, overwrite = TRUE) {

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
  
  current.file <- find_file(params.file.base, type = 'mdes')
  
  if(overwrite | length(current.file) == 0)
  {
    procs <- sim.params.list[['procs']]
    if(!("rawp" %in% sim.params.list[['procs']]))
    {
      procs <- c("rawp", procs)
    }
    
    power.file <- find_file(params.file.base, type = 'power')
    if(length(power.file) == 0)
    {
      stop(paste('Power results table needed for params:', params.file.base))
    }
    power.results <- readRDS(power.file)
    
    mdes_compare_results <- plot_data <- NULL
    for (MTP in procs){
      target.power <- power.results$value[
        power.results$MTP == MTP &
        power.results$power_type == power.definition &
        power.results$method == 'pum'
      ]
      mdes_results <- pump_mdes(
        design = design,
        MTP = MTP,
        M = user.params.list[['M']], J = user.params.list[['J']], K = user.params.list[['K']],
        target.power = target.power,
        power.definition = power.definition,
        tol = sim.params.list[['tol']],
        nbar = user.params.list[['nbar']],
        Tbar = sim.params.list[['Tbar']],
        alpha = sim.params.list[['alpha']],
        numCovar.1 = 1, numCovar.2 = 1, numCovar.3 = 1,
        R2.1 = user.params.list[['R2.1']], R2.2 = user.params.list[['R2.2']], R2.3 = user.params.list[['R2.3']],
        ICC.2 = user.params.list[['ICC.2']], ICC.3 = user.params.list[['ICC.3']],
        rho = user.params.list[['rho.default']],
        omega.2 = user.params.list[['omega.2']], omega.3 = user.params.list[['omega.3']],
        tnum = sim.params.list[['tnum']], B = sim.params.list[['B']],
        start.tnum = sim.params.list[['start.tnum']],
        final.tnum = sim.params.list[['final.tnum']],
        max.cum.tnum = sim.params.list[['max.cum.tnum']],
        max.steps = sim.params.list[['max.steps']],
        cl = cl
      )
      mdes_compare_results <- rbind(mdes_compare_results, mdes_results$mdes.results)
      plot_data <- rbind(plot_data, mdes_results$test.pts)
    }
    
    if(plot.path)
    {
      # plot_data <- plot_data[plot_data$step > 0,]
      plot.power <- ggplot(plot_data, aes(x = step, y = power)) +
        geom_point() + geom_line() +
        facet_wrap(.~MTP) +
        geom_hline(aes(yintercept = target.power)) +
        ylim(0, 1)
      plot.mdes <- ggplot(plot_data, aes(x = step, y = pt)) +
        geom_point() + geom_line() +
        facet_wrap(.~MTP)
      print(grid.arrange(plot.power, plot.mdes, top = design))
    }
    
    mdes.filename <- paste0(params.file.base, 'comparison_mdes_', power.definition, '_results.RDS')
    
    mdes_compare_results[,2:3] <- apply(mdes_compare_results[,2:3], 2, as.numeric)
    mdes_compare_results = cbind(mdes_compare_results, user.params.list[['ATE_ES']][1])
    colnames(mdes_compare_results) <- c('MTP', 'Adjusted MDES', paste(power.definition, 'Power'), 'Target MDES')
    rownames(mdes_compare_results) <- NULL
    
    if(sim.params.list[['parallel']])
    {
      parallel::stopCluster(cl)
    }
    
    saveRDS(mdes_compare_results, file = here("Validation/data", mdes.filename))
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
#' @param overwrite If simulation output files already exist, whether to overwrite
#'
#' @return NULL. Saves out a series of sample validation RDS files.
#' @export
#'
#' @examples
validate_sample <- function(user.params.list, sim.params.list, design,
                            power.definition = 'D1indiv', typesample,
                            plot.path = FALSE, overwrite = TRUE) {
  
  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(user.params.list, sim.params.list, design)
  print(paste('Sample validation for:', params.file.base))
  
  current.file <- find_file(params.file.base, type = 'sample')
  
  # convert to a single MDES
  user.params.list[['ATE_ES']] <- user.params.list[['ATE_ES']][1]
  
  # nullify parameters
  if ( typesample == "nbar" ) {
    user.params.list[['nbar']] <- NULL
  } else if ( typesample == "J" ) {
    user.params.list[['J']] <- NULL
  } else if ( typesample == "K" ) {
    user.params.list[['K']] <- NULL
  }
  
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
      procs <- c("rawp", procs)
    }
    
    power.file <- find_file(params.file.base, type = 'power')
    if(length(power.file) == 0)
    {
      stop(paste('Power results table needed for params:', params.file.base))
    }
    power.results <- readRDS(power.file)
    
    sample_compare_results <- plot_data <- NULL
    for (MTP in procs)
    {
      target.power <- power.results$value[
        power.results$MTP == MTP &
        power.results$power_type == power.definition &
        power.results$method == 'pum'
      ]
      sample_results <- pump_sample(
        design = design,
        MTP = MTP,
        typesample = typesample,
        MDES = user.params.list[['ATE_ES']],
        M = user.params.list[['M']],
        J = user.params.list[['J']],
        K = user.params.list[['K']],
        target.power = target.power,
        power.definition = power.definition,
        tol = sim.params.list[['tol']],
        nbar = user.params.list[['nbar']],
        Tbar = sim.params.list[['Tbar']],
        alpha = sim.params.list[['alpha']],
        numCovar.1 = 1, numCovar.2 = 1, numCovar.3 = 1,
        R2.1 = user.params.list[['R2.1']],
        R2.2 = user.params.list[['R2.2']],
        R2.3 = user.params.list[['R2.3']],
        ICC.2 = user.params.list[['ICC.2']],
        ICC.3 = user.params.list[['ICC.3']],
        rho = user.params.list[['rho.default']],
        omega.2 = user.params.list[['omega.2']],
        omega.3 = user.params.list[['omega.3']],
        tnum = sim.params.list[['tnum']],
        B = sim.params.list[['B']],
        start.tnum = sim.params.list[['start.tnum']],
        final.tnum = sim.params.list[['final.tnum']],
        max.cum.tnum = sim.params.list[['max.cum.tnum']],
        max.steps = sim.params.list[['max.steps']],
        cl = cl
      )
      sample_compare_results <- rbind(sample_compare_results, sample_results$ss.results)
      plot_data <- rbind(plot_data, sample_results$test.pts)
    }
    sample_compare_results[,3:4] = apply(sample_compare_results[,3:4], 2, as.numeric)
    sample.filename <- paste0(
      params.file.base, 'comparison_sample_', typesample, '_', power.definition, '_results.RDS'
    )
    
    if(plot.path)
    {
      # plot_data <- plot_data[plot_data$step > 0,]
      plot.power = ggplot(plot_data, aes(x = step, y = power)) +
        geom_point() + geom_line() +
        facet_wrap(.~MTP) +
        geom_hline(aes(yintercept = target.power)) +
        ylim(0, 1)
      plot.mdes = ggplot(plot_data, aes(x = step, y = pt)) +
        geom_point() + geom_line() +
        facet_wrap(.~MTP)
      print(grid.arrange(plot.power, plot.mdes, top = design))
    }
    
    if(sim.params.list[['parallel']])
    {
      parallel::stopCluster(cl)
    }
    
    saveRDS(sample_compare_results, file = here("Validation/data", sample.filename))
    return(sample_compare_results)
  } else
  {
    print('Validation already completed.')
  }
  
} # validate_sample

### DEBUG
if(FALSE)
{
  design = "blocked_i1_2c";
  # design = 'simple_c2_2r';
  # design = 'simple_c3_3r';
  MTP = 'Bonferroni';
  # MTP = 'Holm';
  # MTP = 'WY-SD';
  numZero = NULL;
  target.power = 0.8;
  M = user.params.list[['M']];
  ATE_ES = user.params.list[['ATE_ES']]
  MDES = user.params.list[['ATE_ES']]
  J = user.params.list[['J']];
  K = user.params.list[['K']];
  nbar = user.params.list[['nbar']];
  power.definition = "D1indiv";
  tol = sim.params.list[['tol']];
  Tbar = sim.params.list[['Tbar']];
  alpha = sim.params.list[['alpha']];
  numCovar.1 = 1; numCovar.2 = 1;
  R2.1 = user.params.list[['R2.1']];
  R2.2 = user.params.list[['R2.2']];
  R2.3 = user.params.list[['R2.3']];
  ICC.2 = user.params.list[['ICC.2']];
  ICC.3 = user.params.list[['ICC.3']];
  rho = user.params.list[['rho.default']];
  rho.matrix = NULL;
  omega.2 = user.params.list[['omega.2']];
  omega.3 = user.params.list[['omega.3']];
  numCovar.1 = 1; numCovar.2 = 1; numCovar.3 = 1;
  tnum = sim.params.list[['tnum']]; B = sim.params.list[['B']];
  max.cum.tnum = sim.params.list[['max.cum.tnum']];
  updateProgress = NULL;
  typesample = 'nbar';
  # typesample = 'J';
  J0 = 10; nbar0 = 10; K0 = 4;
  two.tailed = TRUE;
  # cl <- makeSOCKcluster(rep("localhost", sim.params.list[['ncl']]))
  cl = NULL
  start.tnum = 1000; max.steps = 30; max.cum.tnum = 50000; final.tnum = 20000 
}
