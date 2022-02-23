library(abind)
library(ggplot2)
library(here)
library(PowerUpR)
library(randomizr)
library(reshape2)
library(rlist)
library(snow)

################
library(PUMP)
################


#' Estimating Power through simulations
#'
#' Loop through different simulations like in Table C.3 of the paper
#'
#' @param model.params.list list of user-inputted parameters that feed into the DGP
#' @param sim.params.list list of simulation parameters
#' @param d_m RCT d_m (see list/naming convention)
#' @param q Index of simulation iteration if parallelizing across simulations
#' @param overwrite If simulation output files already exist, whether to overwrite
#'
#' @return NULL. Saves out a series of simulation validation RDS files.
#' @export
#'
#' @examples
validate_power <- function(model.params.list, sim.params.list, d_m, q = 1, overwrite = TRUE)
{

  # set defaults
  if(is.null(sim.params.list[['Q']]))
  {
    sim.params.list[['Q']] <- 1
  }
  
  t1 = Sys.time()
  
  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(model.params.list, sim.params.list, d_m)
  message(paste('Power validation for:', params.file.base))
  
  current.file <- find_file(params.file.base, type = 'power')
  
  # store some files in intermediate results file
  data.dir <- here::here("output")
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
      adjp.proc <- est_power_sim(model.params.list, sim.params.list, d_m, cl)
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
      sim.filename <- paste0(params.file.base, "simulation_results.RDS")
      
      power.results <- NULL
      for(p in 1:dim(adjp.proc)[3])
      {
        proc.results <- PUMP::get_power_results(
          adj.pval.mat = adjp.proc[,,p],
          unadj.pval.mat = adjp.proc[,,'None'],
          ind.nonzero = model.params.list[['MDES']] > 0,
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
        adj_power_melt$method = 'Sim'
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
      if(d_m == 'd1.1_m2fc')
      {
        powerup_results <- power.bira2c1(
          es = model.params.list[['MDES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g1 = 1,
          r21 = model.params.list[['R2.1']][1],
          n = model.params.list[['nbar']],
          J = model.params.list[['J']]
        )
      } else if(d_m == 'd2.1_m2fc')
      {
        powerup_results <- power.bira2c1(
          es = model.params.list[['MDES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g1 = 1,
          r21 = model.params.list[['R2.1']][1],
          n = model.params.list[['nbar']],
          J = model.params.list[['J']]
        )
      } else if(d_m == 'd2.1_m2ff')
      {
        powerup_results <- power.bira2f1(
          es = model.params.list[['MDES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g1 = 1,
          r21 = model.params.list[['R2.1']][1],
          n = model.params.list[['nbar']],
          J = model.params.list[['J']]
        )
      } else if(d_m == 'd2.1_m2fr')
      {
        powerup_results <- power.bira2r1(
          es = model.params.list[['MDES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g2 = 1,
          rho2 = model.params.list[['ICC.2']][1],
          omega2 = model.params.list[['omega.2']][1],
          r21 = model.params.list[['R2.1']][1],
          r2t2 = 0,
          n = model.params.list[['nbar']],
          J = model.params.list[['J']]
        )
      } else if(d_m == 'd3.1_m3rr2rr')
      {
        powerup_results <- power.bira3r1(
          es = model.params.list[['MDES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g3 = 1,
          rho2 = model.params.list[['ICC.2']][1],
          rho3 = model.params.list[['ICC.3']][1],
          omega2 = model.params.list[['omega.2']][1],
          omega3 = model.params.list[['omega.3']][1],
          r21 = model.params.list[['R2.1']][1],
          r2t2 = 0, r2t3 = 0,
          n = model.params.list[['nbar']],
          J = model.params.list[['J']],
          K = model.params.list[['K']]
        )
      } else if(d_m == c('d2.2_m2rc'))
      {
        powerup_results <- power.cra2r2(
          es = model.params.list[['MDES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g2 = 1,
          rho2 = model.params.list[['ICC.2']][1],
          r21 = model.params.list[['R2.1']][1],
          r22 = model.params.list[['R2.2']][1],
          n = model.params.list[['nbar']],
          J = model.params.list[['J']]
        )
      } else if(d_m == c('d3.3_m3rc2rc'))
      {
        powerup_results <- power.cra3r3(
          es = model.params.list[['MDES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g3 = 1,
          rho2 = model.params.list[['ICC.2']][1],
          rho3 = model.params.list[['ICC.3']][1],
          r21 = model.params.list[['R2.1']][1],
          r22 = model.params.list[['R2.2']][1],
          r23 = model.params.list[['R2.3']][1],
          n = model.params.list[['nbar']],
          J = model.params.list[['J']],
          K = model.params.list[['K']]
        )
      } else if(d_m == c('d3.2_m3ff2rc'))
      {
        powerup_results <- power.bcra3f2(
          es = model.params.list[['MDES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g2 = 1,
          rho2 = model.params.list[['ICC.2']][1],
          r21 = model.params.list[['R2.1']][1],
          r22 = model.params.list[['R2.2']][1],
          n = model.params.list[['nbar']],
          J = model.params.list[['J']],
          K = model.params.list[['K']]
        )
      } else if(d_m == c('d3.2_m3rr2rc'))
      {
        powerup_results <- power.bcra3r2(
          es = model.params.list[['MDES']][1],
          alpha = sim.params.list[['alpha']],
          two.tailed = TRUE,
          p = sim.params.list[['Tbar']],
          g3 = 1,
          rho2 = model.params.list[['ICC.2']][1],
          rho3 = model.params.list[['ICC.3']][1],
          omega3 = model.params.list[['omega.3']][1],
          r21 = model.params.list[['R2.1']][1],
          r22 = model.params.list[['R2.2']][1],
          r2t3 = 0,
          n = model.params.list[['nbar']],
          J = model.params.list[['J']],
          K = model.params.list[['K']]
        )
      } else if(d_m %in% PUMP::pump_info()$Context$d_m) {
        # if a valid but non-powerup d_m
        powerup_results <- data.frame(
          power = NA
        )
      } else
      {
        stop(paste('Unknown d_m:', d_m)) 
      }

      powerup_results <- data.frame(
        MTP = 'None',
        variable = 'D1indiv',
        method = 'PowerUp',
        value = powerup_results$power,
        value.type = 'adjusted_power'
      )
      
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
      
      for (MTP in sim.params.list[['MTP']]){
        pump_results_iter <- PUMP::pump_power(
          d_m = d_m,
          MTP = MTP,
          MDES = model.params.list[['MDES']],
          M = model.params.list[['M']],
          J = model.params.list[['J']],
          K = model.params.list[['K']],
          nbar = model.params.list[['nbar']],
          Tbar = sim.params.list[['Tbar']],
          alpha = sim.params.list[['alpha']],
          numCovar.1 = model.params.list[['numCovar.1']],
          numCovar.2 = model.params.list[['numCovar.2']],
          numCovar.3 = model.params.list[['numCovar.3']],
          R2.1 = model.params.list[['R2.1']],
          R2.2 = model.params.list[['R2.2']],
          R2.3 = model.params.list[['R2.3']],
          ICC.2 = model.params.list[['ICC.2']],
          ICC.3 = model.params.list[['ICC.3']],
          rho = model.params.list[['rho.default']],
          omega.2 = model.params.list[['omega.2']],
          omega.3 = model.params.list[['omega.3']],
          tnum = sim.params.list[['tnum']],
          B = sim.params.list[['B']],
          verbose = TRUE
        )
        if (iterator == 0) {
          pump_results <- pump_results_iter
        } else {
          pump_results <- rbind(pump_results, pump_results_iter[2,])
        }
        iterator <- iterator + 1
      }
      # format results table nicely
      pump_results_table <- pump_results
      pump_results <- melt(pump_results_table, id.vars = 'MTP')
      pump_results$method = 'PUMP'
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
      
      # add in parameter info
      compare_results$d_m      <- d_m
      compare_results$S        <- sim.params.list$S * sim.params.list$Q
      compare_results$M        <- model.params.list$M
      compare_results$MDES     <- model.params.list$MDES[1]
      compare_results$numZero  <- sum(model.params.list$MDES == 0)
      compare_results$J        <- ifelse(!is.null(model.params.list$J), model.params.list$J, NA) 
      compare_results$K        <- ifelse(!is.null(model.params.list$K), model.params.list$K, NA) 
      compare_results$nbar     <- model.params.list$nbar
      compare_results$rho      <- model.params.list$rho.default
      compare_results$omega.2  <- ifelse(!is.null(model.params.list$omega.2[1]), model.params.list$omega.2[1], NA) 
      compare_results$omega.3  <- ifelse(!is.null(model.params.list$omega.3[1]), model.params.list$omega.3[1], NA) 
      compare_results$R2.1     <- ifelse(!is.null(model.params.list$R2.1[1]),    model.params.list$R2.1[1], NA) 
      compare_results$R2.2     <- ifelse(!is.null(model.params.list$R2.2[1]),    model.params.list$R2.2[1], NA) 
      compare_results$R2.3     <- ifelse(!is.null(model.params.list$R2.3[1]),    model.params.list$R2.3[1], NA) 
      compare_results$ICC.2    <- ifelse(!is.null(model.params.list$ICC.2[1]),   model.params.list$ICC.2[1], NA) 
      compare_results$ICC.3    <- ifelse(!is.null(model.params.list$ICC.3[1]),   model.params.list$ICC.3[1], NA) 
      
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
#' @param model.params.list list of user-inputted parameters that feed into the DGP
#' @param sim.params.list list of simulation parameters
#' @param d_m RCT d_m (see list/naming convention)
#' @param overwrite If simulation output files already exist, whether to overwrite
#'
#' @return NULL. Saves out a series of MDES validation RDS files.
#' @export
#'
#' @examples
validate_mdes <- function(model.params.list, sim.params.list, d_m,
                          power.definition = 'D1indiv', plot.path = FALSE, overwrite = TRUE) {

  if(sim.params.list[['parallel']])
  {
    cl <- makeSOCKcluster(rep("localhost", sim.params.list[['ncl']]))
  } else
  {
    cl <- NULL
  }
  
  # set defaults
  if(is.null(sim.params.list[['Q']]))
  {
    sim.params.list[['Q']] <- 1
  }
  
  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(model.params.list, sim.params.list, d_m)
  print(paste('MDES validation for:', params.file.base))
  
  current.file <- find_file(params.file.base, type = 'mdes')
  
  if(overwrite | length(current.file) == 0)
  {
    MTP <- sim.params.list[['MTP']]
    
    power.file <- find_file(params.file.base, type = 'power')
    if(length(power.file) == 0)
    {
      stop(paste('Power results table needed for params:', params.file.base))
    }
    power.results <- readRDS(power.file)
    
    mdes_compare_results <- plot_data <- NULL
    for (proc in MTP){
      target.power <- power.results$value[
        power.results$MTP == proc &
        power.results$power_type == power.definition &
        power.results$method == 'PUMP'
      ]
      
      
      mdes_results_iter <- PUMP::pump_mdes(
        d_m = d_m,
        MTP = proc,
        M = model.params.list[['M']],
        J = model.params.list[['J']],
        K = model.params.list[['K']],
        target.power = target.power,
        power.definition = power.definition,
        tol = sim.params.list[['tol']],
        nbar = model.params.list[['nbar']],
        Tbar = sim.params.list[['Tbar']],
        alpha = sim.params.list[['alpha']],
        numCovar.1 = model.params.list[['numCovar.1']],
        numCovar.2 = model.params.list[['numCovar.2']],
        numCovar.3 = model.params.list[['numCovar.3']],
        R2.1 = model.params.list[['R2.1']],
        R2.2 = model.params.list[['R2.2']],
        R2.3 = model.params.list[['R2.3']],
        ICC.2 = model.params.list[['ICC.2']],
        ICC.3 = model.params.list[['ICC.3']],
        rho = model.params.list[['rho.default']],
        omega.2 = model.params.list[['omega.2']], omega.3 = model.params.list[['omega.3']],
        B = sim.params.list[['B']],
        start.tnum = sim.params.list[['start.tnum']],
        final.tnum = sim.params.list[['final.tnum']],
        max.steps = sim.params.list[['max.steps']],
        verbose = TRUE
      )
      mdes_compare_results <- rbind(mdes_compare_results, mdes_results_iter)
    }

    mdes.filename <- paste0(params.file.base, 'comparison_mdes_', power.definition, '_results.RDS')
    
    mdes_compare_results[,2:3] <- apply(mdes_compare_results[,2:3], 2, as.numeric)
    mdes_compare_results <- as.data.frame(mdes_compare_results)
    mdes_compare_results = cbind(mdes_compare_results, model.params.list[['MDES']][1])
    colnames(mdes_compare_results) <- c('MTP', 'Adjusted MDES', paste(power.definition, 'Power'), 'Target MDES')
    rownames(mdes_compare_results) <- NULL
    
    # add in parameter info
    mdes_compare_results$d_m      <- d_m
    mdes_compare_results$S        <- sim.params.list$S * sim.params.list$Q
    mdes_compare_results$M        <- model.params.list$M
    mdes_compare_results$MDES     <- model.params.list$MDES[1]
    mdes_compare_results$numZero  <- sum(model.params.list$MDES == 0)
    mdes_compare_results$J        <- ifelse(!is.null(model.params.list$J), model.params.list$J, NA) 
    mdes_compare_results$K        <- ifelse(!is.null(model.params.list$K), model.params.list$K, NA) 
    mdes_compare_results$nbar     <- model.params.list$nbar
    mdes_compare_results$rho      <- model.params.list$rho.default
    mdes_compare_results$omega.2  <- ifelse(!is.null(model.params.list$omega.2[1]), model.params.list$omega.2[1], NA) 
    mdes_compare_results$omega.3  <- ifelse(!is.null(model.params.list$omega.3[1]), model.params.list$omega.3[1], NA) 
    mdes_compare_results$R2.1     <- ifelse(!is.null(model.params.list$R2.1[1]),    model.params.list$R2.1[1], NA) 
    mdes_compare_results$R2.2     <- ifelse(!is.null(model.params.list$R2.2[1]),    model.params.list$R2.2[1], NA) 
    mdes_compare_results$R2.3     <- ifelse(!is.null(model.params.list$R2.3[1]),    model.params.list$R2.3[1], NA) 
    mdes_compare_results$ICC.2    <- ifelse(!is.null(model.params.list$ICC.2[1]),   model.params.list$ICC.2[1], NA) 
    mdes_compare_results$ICC.3    <- ifelse(!is.null(model.params.list$ICC.3[1]),   model.params.list$ICC.3[1], NA) 
    
    if(sim.params.list[['parallel']])
    {
      parallel::stopCluster(cl)
    }
    
    saveRDS(mdes_compare_results, file = here::here("output", mdes.filename))
    return(mdes_compare_results)
  } else
  {
    print('Validation already completed.')
  }
  
} # validate_mdes


#' Estimating sample size
#'
#'
#' @param model.params.list list of user-inputted parameters that feed into the DGP
#' @param sim.params.list list of simulation parameters
#' @param d_m RCT d_m (see list/naming convention)
#' @param overwrite If simulation output files already exist, whether to overwrite
#'
#' @return NULL. Saves out a series of sample validation RDS files.
#' @export
#'
#' @examples
validate_sample <- function(model.params.list, sim.params.list, d_m,
                            power.definition = 'D1indiv', typesample,
                            plot.path = FALSE, overwrite = TRUE) {
  
  # set defaults
  if(is.null(sim.params.list[['Q']]))
  {
    sim.params.list[['Q']] <- 1
  }
  
  # for saving out and reading in files based on simulation parameters
  params.file.base <- gen_params_file_base(model.params.list, sim.params.list, d_m)
  print(paste(
    'Sample validation with power def:', power.definition,
    'and typesample', typesample,
    'for:', params.file.base
  ))
  
  current.file <- find_file(params.file.base, type = 'sample')
  
  # convert to a single MDES
  model.params.list[['MDES']] <- model.params.list[['MDES']][1]
  
  # nullify parameters
  if ( typesample == "nbar" ) {
    model.params.list[['nbar']] <- NULL
  } else if ( typesample == "J" ) {
    model.params.list[['J']] <- NULL
  } else if ( typesample == "K" ) {
    model.params.list[['K']] <- NULL
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
    
    MTP <- sim.params.list[['MTP']]
    
    power.file <- find_file(params.file.base, type = 'power')
    if(length(power.file) == 0)
    {
      stop(paste('Power results table needed for params:', params.file.base))
    }
    power.results <- readRDS(power.file)
    
    sample_compare_results <- plot_data <- NULL
    for (proc in MTP)
    {
      target.power <- power.results$value[
        power.results$MTP == proc &
        power.results$power_type == power.definition &
        power.results$method == 'PUMP'
      ]
      
      sample_results_iter <- PUMP::pump_sample(
        d_m = d_m,
        MTP = proc,
        typesample = typesample,
        MDES = model.params.list[['MDES']],
        M = model.params.list[['M']],
        J = model.params.list[['J']],
        K = model.params.list[['K']],
        target.power = target.power,
        power.definition = power.definition,
        tol = sim.params.list[['tol']],
        nbar = model.params.list[['nbar']],
        Tbar = sim.params.list[['Tbar']],
        alpha = sim.params.list[['alpha']],
        numCovar.1 = model.params.list[['numCovar.1']],
        numCovar.2 = model.params.list[['numCovar.2']],
        numCovar.3 = model.params.list[['numCovar.3']],
        R2.1 = model.params.list[['R2.1']],
        R2.2 = model.params.list[['R2.2']],
        R2.3 = model.params.list[['R2.3']],
        ICC.2 = model.params.list[['ICC.2']],
        ICC.3 = model.params.list[['ICC.3']],
        rho = model.params.list[['rho.default']],
        omega.2 = model.params.list[['omega.2']],
        omega.3 = model.params.list[['omega.3']],
        B = sim.params.list[['B']],
        start.tnum = sim.params.list[['start.tnum']],
        final.tnum = sim.params.list[['final.tnum']],
        max.steps = sim.params.list[['max.steps']],
        verbose = TRUE
      )
      sample_compare_results <- rbind(sample_compare_results, sample_results_iter)
      # plot_data <- rbind(plot_data, sample_results_iter$test.pts)
    }
    sample_compare_results <- as.data.frame(sample_compare_results)
    sample_compare_results[,3:4] = apply(sample_compare_results[,3:4], 2, as.numeric)
    sample.filename <- paste0(
      params.file.base, 'comparison_sample_', typesample, '_', power.definition, '_results.RDS'
    )
    
    # add in parameter info
    sample_compare_results$d_m      <- d_m
    sample_compare_results$S        <- sim.params.list$S * sim.params.list$Q
    sample_compare_results$M        <- model.params.list$M
    sample_compare_results$MDES     <- model.params.list$MDES[1]
    sample_compare_results$numZero  <- sum(model.params.list$MDES == 0)
    sample_compare_results$J        <- ifelse(!is.null(model.params.list$J), model.params.list$J, NA) 
    sample_compare_results$K        <- ifelse(!is.null(model.params.list$K), model.params.list$K, NA) 
    sample_compare_results$nbar     <- model.params.list$nbar
    sample_compare_results$rho      <- model.params.list$rho.default
    sample_compare_results$omega.2  <- ifelse(!is.null(model.params.list$omega.2[1]), model.params.list$omega.2[1], NA) 
    sample_compare_results$omega.3  <- ifelse(!is.null(model.params.list$omega.3[1]), model.params.list$omega.3[1], NA) 
    sample_compare_results$R2.1     <- ifelse(!is.null(model.params.list$R2.1[1]),    model.params.list$R2.1[1], NA) 
    sample_compare_results$R2.2     <- ifelse(!is.null(model.params.list$R2.2[1]),    model.params.list$R2.2[1], NA) 
    sample_compare_results$R2.3     <- ifelse(!is.null(model.params.list$R2.3[1]),    model.params.list$R2.3[1], NA) 
    sample_compare_results$ICC.2    <- ifelse(!is.null(model.params.list$ICC.2[1]),   model.params.list$ICC.2[1], NA) 
    sample_compare_results$ICC.3    <- ifelse(!is.null(model.params.list$ICC.3[1]),   model.params.list$ICC.3[1], NA) 
    
    if(sim.params.list[['parallel']])
    {
      parallel::stopCluster(cl)
    }
    
    saveRDS(sample_compare_results, file = here::here("output", sample.filename))
    return(sample_compare_results)
  } else
  {
    print('Validation already completed.')
  }
  
} # validate_sample

### DEBUG
if(FALSE)
{
  MTP = 'BF';
  # MTP = 'Holm';
  # MTP = 'WY-SD';
  numZero = NULL;
  target.power = 0.8;
  
  M = model.params.list[['M']];
  MDES = model.params.list[['MDES']]
  MDES = model.params.list[['MDES']]
  J = model.params.list[['J']];
  K = model.params.list[['K']];
  nbar = model.params.list[['nbar']];
  power.definition = "D1indiv";
  tol = sim.params.list[['tol']];
  Tbar = sim.params.list[['Tbar']];
  alpha = sim.params.list[['alpha']];
  R2.1 = model.params.list[['R2.1']];
  R2.2 = model.params.list[['R2.2']];
  R2.3 = model.params.list[['R2.3']];
  ICC.2 = model.params.list[['ICC.2']];
  ICC.3 = model.params.list[['ICC.3']];
  rho = model.params.list[['rho.default']];
  rho.matrix = NULL;
  omega.2 = model.params.list[['omega.2']];
  omega.3 = model.params.list[['omega.3']];
  numCovar.1 = model.params.list[['numCovar.1']];
  numCovar.2 = model.params.list[['numCovar.2']];
  numCovar.3 = model.params.list[['numCovar.3']];
  tnum = sim.params.list[['tnum']]; B = sim.params.list[['B']];
  max.cum.tnum = sim.params.list[['max.cum.tnum']];
  
  
  updateProgress = NULL;
  typesample = 'nbar';
  # typesample = 'J';
  J0 = 10; nbar0 = 10; K0 = 4;
  two.tailed = TRUE;
  start.tnum = 1000; max.steps = 30; max.cum.tnum = 50000; final.tnum = 20000 
}
