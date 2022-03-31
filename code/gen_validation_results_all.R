#-----------------------------------------------------------------#
# script that produces all output results
# all d_ms, all cases of interest
#------------------------------------------------------------------#

library(here)

# overwrite existing results that have already been saved?
overwrite = TRUE
# whether or not to run power, mdes and sample size
run.power = TRUE
run.mdes.ss = FALSE
# whether to run limited westfall young validations
run.wy = FALSE
# which d_ms to run
run.d1.1 = FALSE
run.d2.1 = FALSE
run.d2.2 = FALSE
run.d3.1 = FALSE
run.d3.3 = FALSE
run.d3.2 = TRUE

# for parallel processing
q <- as.numeric(as.character(Sys.getenv("q")))
if(is.na(q)) { q <- 1 }

#------------------------------------------------------------------#
# source files
#------------------------------------------------------------------#

source(here::here("code", "adjust_WY.R"))
source(here::here("code", "estimate_power_with_simulation.R"))
source(here::here("code", "validate_power.R"))
source(here::here("code", "sim.R"))
source(here::here("code", "misc.R"))


#------------------------------------------------------------------#
# simulation parameters
#------------------------------------------------------------------#

sim.params.list <- list(
  S = 250                     # Number of samples for Monte Carlo Simulation
  , Q = 20                    # Number of times entire simulation is repeated, so total iterations = S * Q
  , B = NULL                 # Number of samples for WestFall-Young. The equivalent is snum in our new method.
  , alpha = 0.05             # Significance level
  , tol = 0.01               # tolerance for MDES and sample  size calculations
  , Tbar = 0.5               # Binomial assignment probability
  , tnum = 10000             # Number of test statistics (samples) for all procedures other than Westfall-Young
  , parallel = TRUE          # parallelize within each monte carlo iteration
  , ncl = 8                  # Number of computer clusters
  , start.tnum = 200         # number of iterations for starting to testing mdes and power
  , final.tnum = 100000      # final number of iterations to check power
  , max.steps = 20           # maximum number of iterations for MDES or sample size calculations
  , max.cum.tnum = 10000000  # maximum cumulative tnum for MDES and sample size
  , MTP = c("BF", "BH", "HO") # Multiple testing procedures
  , runSim = FALSE       # If TRUE, we will re-run the simulation. If FALSE, we will pull previous run result.
  , runPump = TRUE    # If TRUE, we will run method from our package. If FALSE, we will pull previous run result.
  , runPowerUp = TRUE    # If TRUE, we will run method from powerup. If FALSE, we will pull previous run result.
)

#------------------------------------------------------------------#
# model parameters
# assumes same correlation structure across all outcomes, covariates, random effects, etc.
# assumes equal sized schools that are evenly split between districts
#------------------------------------------------------------------#

M <- 3
rho.default <- 0.5
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)

model.params.list <- list(
  M = 3                                   # number of outcomes
  , J = 30                                # number of schools
  , K = 10                                # number of districts (for two-level model, set K = 1)
  , nbar = 50                             # number of individuals per school
  , rho.default = 0.5                     # default rho value
  # , S.id = NULL                         # N-length vector of indiv school assignments (optional)
  # , D.id = NULL                         # N-length vector of indiv district assignments (optional)
  ################################################## grand mean outcome and impact
  # , Xi0 = 0                             # scalar grand mean outcome under no treatment
  , MDES = rep(0.125, M)                  # minimum detectable effect size      
  ################################################## level 3: districts
  , numCovar.3 = 1                        # number of district covariates
  , R2.3 = rep(0.1, M)                    # percent of district variation explained by district covariates
  # , rho.V = default.rho.matrix          # MxM correlation matrix of district covariates
  , ICC.3 = rep(0.2, M)                   # district intraclass correlation
  , omega.3 = rep(0.1, M)                 # ratio of district effect size variability to random effects variability
  # , rho.w0 = default.rho.matrix         # MxM matrix of correlations for district random effects
  # , rho.w1 = default.rho.matrix         # MxM matrix of correlations for district impacts
  , kappa.w = matrix(0, M, M)             # MxM matrix of correlations between district random effects and impacts
  ################################################## level 2: schools
  , numCovar.2 = 1                        # number of school covariates
  , R2.2 = rep(0.1, M)                    # percent of school variation explained by school covariates
  # , rho.X = default.rho.matrix          # MxM correlation matrix of school covariates
  , ICC.2 = rep(0.2, M)                   # school intraclass correlation	
  , omega.2 = rep(0.1, M)                 # ratio of school effect size variability to random effects variability
  # , rho.u0 = default.rho.matrix         # MxM matrix of correlations for school random effects
  # , rho.u1 = default.rho.matrix         # MxM matrix of correlations for school impacts
  , kappa.u = matrix(0, M, M)             # MxM matrix of correlations between school random effects and impacts
  ################################################## level 1: individuals
  , numCovar.1 = 1                        # number of individual covariates
  , R2.1 = rep(0.1, M)                    # percent of indiv variation explained by indiv covariates
  # , rho.C = default.rho.matrix          # MxM correlation matrix of individual covariates
  # , rho.r = default.rho.matrix          # MxM matrix of correlations for individual residuals 
)

model.params.default <- model.params.list
sim.params.default <- sim.params.list

#------------------------------------------------------------------#
# Test WY
#------------------------------------------------------------------#
if(run.wy)
{
  sim.params.list <- sim.params.default
  sim.params.list[['MTP']] <- c("BF", "BH", "HO", "WY-SS", "WY-SD")
  
  #------------------------------------#
  # blocked 2 level
  #------------------------------------#
  if(run.d2.1)
  {
    model.params.list <- model.params.default
    sim.params.list[['S']] <- 100
    sim.params.list[['B']] <- 1000

    model.params.list[['K']] <- 1
    model.params.list[['ICC.3']] <- NULL
    model.params.list[['omega.3']] <- NULL
    model.params.list[['numCovar.3']] <- 0
    model.params.list[['R2.3']] <- NULL
    model.params.list[['numCovar.2']] <- 0
    model.params.list[['R2.2']] <- NULL
    
    model.params.list[['J']] <- 60    

    # so power isn't too high
    model.params.list[['MDES']] <- rep(0.05, M)
    model.params.list[['nbar']] <- 30

    model.params.list[['omega.2']] <- NULL
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
    model.params.list[['omega.2']] <- model.params.default[['omega.2']]
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  
    gc()
  }

  #------------------------------------#
  # cluster 2 level
  #------------------------------------#
  
  if(run.d2.2)
  {
    model.params.list <- model.params.default
    sim.params.list[['S']] <- 100
    sim.params.list[['B']] <- 1000
    
    model.params.list[['K']] <- 1
    model.params.list[['ICC.3']] <- NULL
    model.params.list[['omega.3']] <- NULL
    model.params.list[['numCovar.3']] <- 0
    model.params.list[['R2.3']] <- NULL
    model.params.list[['omega.2']] <- NULL
    
    # params to help have a decent power
    model.params.list[['ICC.2']] <- rep(0.1, M)
    model.params.list[['J']] <- 60
    model.params.list[['MDES']] <- rep(0.25, M)
    
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
    gc()
  }

  #------------------------------------#
  # blocked 3 level
  #------------------------------------#
  
  if(run.d3.1)
  {
    model.params.list <- model.params.default
    sim.params.list[['S']] <- 30
    sim.params.list[['B']] <- 2000
    
    # assumptions
    model.params.list[['numCovar.3']] <- 0
    model.params.list[['R2.3']] <- NULL
    model.params.list[['numCovar.2']] <- 0
    model.params.list[['R2.2']] <- NULL
    
    # for a reasonable runtime and power
    model.params.list[['nbar']] <- 100
    model.params.list[['J']] <- 10
    model.params.list[['K']] <- 10
    model.params.list[['ICC.2']] <- rep(0.1, M)
    model.params.list[['ICC.3']] <- rep(0.1, M)
    model.params.list[['omega.2']] <- rep(0.05, M)
    model.params.list[['omega.3']] <- rep(0.05, M)
    
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
    
    
    # try a smaller number of clusters
    model.params.list[['J']] <- 5
    model.params.list[['K']] <- 5
    
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
    gc()
  }
  
  #------------------------------------#
  # cluster 3 level
  #------------------------------------#
  
  if(run.d3.3)
  {
    model.params.list <- model.params.default
    sim.params.list[['MTP']] <- c("BF", "BH", "HO", "WY-SS")
    sim.params.list[['S']] <- 30
    sim.params.list[['B']] <- 3000
    
    # assumptions
    model.params.list[['omega.2']] <- NULL
    model.params.list[['omega.3']] <- NULL
    
    # for a reasonable runtime and power
    model.params.list[['J']] <- 20
    model.params.list[['K']] <- 20
    model.params.list[['nbar']] <- 100
    model.params.list[['MDES']] <- rep(0.3, M)
    model.params.list[['ICC.3']] <- rep(0.05, M)
    model.params.list[['ICC.2']] <- rep(0.05, M)
    model.params.list[['R2.1']] <- rep(0.4, M)
    model.params.list[['R2.2']] <- rep(0.4, M)
    model.params.list[['R2.3']] <- rep(0.4, M)
    
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
    
    # for a reasonable runtime and power
    model.params.list[['J']] <- 10
    model.params.list[['K']] <- 10
    
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
    gc()
  }

  #------------------------------------#
  # blocked cluster
  #------------------------------------#
  
  if(run.d3.2)
  {
    # constant effects
    model.params.list <- model.params.default
    sim.params.list[['MTP']] <- c("BF", "BH", "HO", "WY-SS")
    sim.params.list[['S']] <- 50
    sim.params.list[['B']] <- 2000
    
    # assumptions
    model.params.list[['numCovar.3']] <- 0
    model.params.list[['R2.3']] <- NULL
    model.params.list[['omega.2']] <- NULL
    
    # for reasonable power and runtime
    model.params.list[['nbar']] <- 50
    model.params.list[['J']] <- 5
    model.params.list[['K']] <- 10
    model.params.list[['MDES']] <- rep(0.25, M)
    model.params.list[['ICC.3']] <- rep(0.1, M)
    model.params.list[['ICC.2']] <- rep(0.1, M)
    model.params.list[['R2.1']] <- rep(0.4, M)
    model.params.list[['R2.2']] <- rep(0.4, M)
    model.params.list[['R2.3']] <- rep(0.4, M)
    
    
    model.params.list[['omega.3']] <- NULL
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
    
    # random effects
    model.params.list <- model.params.default
    sim.params.list[['S']] <- 30
    sim.params.list[['B']] <- 3000
    
    model.params.list[['numCovar.3']] <- 0
    model.params.list[['R2.3']] <- NULL
    model.params.list[['omega.2']] <- NULL
    
    # for reasonable power and runtime
    model.params.list[['nbar']] <- 100
    model.params.list[['J']] <- 10
    model.params.list[['K']] <- 10
    model.params.list[['MDES']] <- rep(0.125, M)
    model.params.list[['ICC.3']] <- rep(0.1, M)
    model.params.list[['ICC.2']] <- rep(0.1, M)
    model.params.list[['R2.1']] <- rep(0.4, M)
    model.params.list[['R2.2']] <- rep(0.4, M)
    model.params.list[['R2.3']] <- rep(0.4, M)
    
    
    model.params.list[['omega.3']] <- rep(0.05, M)
    power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
    gc()
  }
  # reset
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['MTP']] <- c("BF", "BH", "HO")
}

#------------------------------------------------------------------#
# Blocked 1 level: power
#------------------------------------------------------------------#

if(run.d1.1 & run.power)
{
  scenarios <- 20
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['K']] <- 1
  model.params.list[['J']] <- 1
  model.params.list[['ICC.2']] <- NULL
  model.params.list[['omega.2']] <- NULL
  model.params.list[['ICC.3']] <- NULL
  model.params.list[['omega.3']] <- NULL
  model.params.list[['numCovar.3']] <- 0
  model.params.list[['R2.3']] <- NULL
  model.params.list[['numCovar.2']] <- 0
  model.params.list[['R2.2']] <- NULL
  
  #------------------------------------------------------------------#
  # base case
  #------------------------------------------------------------------#
  
  model.params.list[['nbar']] <- 50
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d1.1_m1c", q = q, overwrite)
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  model.params.list[['nbar']] <- 100
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d1.1_m1c", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 75
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d1.1_m1c", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  # reset
  model.params.list[['nbar']] <- model.params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  model.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d1.1_m1c", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  
  # set to 0
  model.params.list[['R2.1']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d1.1_m1c", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.default']] <- rho.default
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d1.1_m1c", q = q, overwrite)
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.default']] <- rho.default
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d1.1_m1c", q = q, overwrite)
  
  rho.default <- 0.8
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d1.1_m1c", q = q, overwrite)
  
  # reset
  rho.default <- model.params.default[['rho.default']]
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 13 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  model.params.list[['MDES']] <- c(0.125, 0, 0)
  
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d1.1_m1c", q = q, overwrite)
  
  # reset
  model.params.list[['MDES']] <- model.params.default[['MDES']]
  
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  
}

#------------------------------------------------------------------#
# Blocked 2 level: power
#------------------------------------------------------------------#

if(run.d2.1 & run.power)
{
  scenarios <- 20
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['K']] <- 1
  model.params.list[['J']] <- 20
  model.params.list[['ICC.3']] <- NULL
  model.params.list[['omega.3']] <- NULL
  model.params.list[['numCovar.3']] <- 0
  model.params.list[['R2.3']] <- NULL
  model.params.list[['numCovar.2']] <- 0
  model.params.list[['R2.2']] <- NULL
  
  #------------------------------------------------------------------#
  # base case
  #------------------------------------------------------------------#
  
  model.params.list[['nbar']] <- 50
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  model.params.list[['nbar']] <- 100
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 75
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')

  # reset
  model.params.list[['nbar']] <- model.params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  model.params.list[['omega.2']] <- NULL
  
  model.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  
  # set both to 0
  model.params.list[['R2.1']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.default']] <- rho.default
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.default']] <- rho.default
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  
  rho.default <- 0.8
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  
  # reset
  rho.default <- model.params.default[['rho.default']]
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 13 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  model.params.list[['MDES']] <- c(0.125, 0, 0)
  
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  
  # reset
  model.params.list[['MDES']] <- model.params.default[['MDES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  model.params.list[['ICC.2']] <- rep(0.7, M)
  
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  
  model.params.list[['ICC.2']] <- rep(0, M)
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  
  # reset
  model.params.list[['ICC.2']] <- model.params.default[['ICC.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 18 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  model.params.list[['omega.2']] <- rep(0.8, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  
  model.params.list[['omega.2']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 20 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- kappa.matrix
  model.params.list[['omega.2']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fc", q = q, overwrite)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2ff", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr", q = q, overwrite)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- model.params.default[['kappa.w']]
  
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  
}

#------------------------------------------------------------------#
# Blocked 2 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d2.1 & run.mdes.ss)
{
  scenarios <- 9
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['K']] <- 1
  model.params.list[['J']] <- 20
  model.params.list[['ICC.3']] <- NULL
  model.params.list[['omega.3']] <- NULL
  model.params.list[['numCovar.3']] <- 0
  model.params.list[['R2.3']] <- NULL
  model.params.list[['numCovar.2']] <- 0
  model.params.list[['R2.2']] <- NULL
  
  # don't do WY for now
  sim.params.list[['MTP']] <- c("BF", "BH", "HO")
  
  model.params.list[['omega.2']] <- NULL
  mdes.results <- validate_mdes(
    model.params.list, sim.params.list,
    d_m = "d2.1_m2fc", 
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d2.1_m2fc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d2.1_m2fc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed constant, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['omega.2']] <- NULL
  mdes.results <- validate_mdes(
    model.params.list, sim.params.list,
    d_m = "d2.1_m2ff", 
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d2.1_m2ff",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d2.1_m2ff",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed fixed, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  mdes.results <- validate_mdes(
    model.params.list, sim.params.list,
    d_m = "d2.1_m2fr", 
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d2.1_m2fr",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d2.1_m2fr",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed random, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  # reset
  model.params.list <- model.params.default
  sim.params.list[['MTP']] <- sim.params.default[['MTP']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked 2 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

#------------------------------------------------------------------#
# Cluster 2 level: power
#------------------------------------------------------------------#

if(run.d2.2 & run.power)
{
  scenarios <- 10
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL

  # assumptions
  model.params.list[['K']] <- 1
  model.params.list[['ICC.3']] <- NULL
  model.params.list[['omega.3']] <- NULL
  model.params.list[['numCovar.3']] <- 0
  model.params.list[['R2.3']] <- NULL
  model.params.list[['omega.2']] <- NULL
  
  # params to help have a decent power
  model.params.list[['ICC.2']] <- rep(0.1, M)
  model.params.list[['J']] <- 60
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  model.params.list[['nbar']] <- 100
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 75
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 50
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  
  # reset
  model.params.list[['nbar']] <- model.params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  model.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  
  # vary R2.2
  model.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  # reset
  model.params.list[['R2.2']] <- model.params.default[['R2.2']]
  
  # set both to 0
  model.params.list[['R2.1']] <- rep(0, M)
  model.params.list[['R2.2']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  model.params.list[['R2.2']] <- model.params.default[['R2.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 5 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.default']] <- rho.default
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.default']] <- rho.default
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  
  rho.default <- 0.8
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  
  # reset
  rho.default <- model.params.default[['rho.default']]
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- model.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 7 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  model.params.list[['MDES']] <- c(0.125, 0, 0)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  
  # reset
  model.params.list[['MDES']] <- model.params.default[['MDES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  model.params.list[['ICC.2']] <- rep(0.7, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  
  model.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  model.params.list[['ICC.2']] <- model.params.default[['ICC.2']]
  
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- kappa.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.2_m2rc", q = q, overwrite)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- model.params.default[['kappa.w']]
  
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
}

#------------------------------------------------------------------#
# Cluster 2 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d2.2 & run.mdes.ss)
{
  scenarios <- 1
  # back to defaults
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['K']] <- 1
  model.params.list[['ICC.3']] <- NULL
  model.params.list[['omega.3']] <- NULL
  model.params.list[['numCovar.3']] <- 0
  model.params.list[['R2.3']] <- NULL
  model.params.list[['omega.2']] <- NULL
  
  # params to help have a decent power
  model.params.list[['ICC.2']] <- rep(0.1, M)
  model.params.list[['J']] <- 60
  
  mdes.results <- validate_mdes(
    model.params.list, sim.params.list,
    d_m = "d2.2_m2rc",
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d2.2_m2rc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d2.2_m2rc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  # reset
  model.params.list <- model.params.default
  sim.params.list[['MTP']] <- sim.params.default[['MTP']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed cluster 2 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

#------------------------------------------------------------------#
# Blocked 3 level: power
#------------------------------------------------------------------#

if(run.d3.1 & run.power)
{
  scenarios <- 15
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['numCovar.3']] <- 0
  model.params.list[['R2.3']] <- NULL
  model.params.list[['numCovar.2']] <- 0
  model.params.list[['R2.2']] <- NULL
  # for sufficient power and stability
  model.params.list[['K']] <- 15
  model.params.list[['nbar']] <- 100

  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  model.params.list[['nbar']] <- 100
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 75
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 50
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  # reset
  model.params.list[['nbar']] <- 100
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  model.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  

  # set all to 0
  model.params.list[['R2.1']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.V']] <- model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- default.rho.matrix
  model.params.list[['rho.w0']] <- model.params.list[['rho.w1']] <- default.rho.matrix
  model.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  rho.default <- 0.8
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.V']] <- model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- default.rho.matrix
  model.params.list[['rho.w0']] <- model.params.list[['rho.w1']] <- default.rho.matrix
  model.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  # reset
  rho.default <- model.params.default[['rho.default']]
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.V']] <- model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- default.rho.matrix
  model.params.list[['rho.w0']] <- model.params.list[['rho.w1']] <- default.rho.matrix
  model.params.list[['rho.r']] <- default.rho.matrix
  
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 8 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  model.params.list[['MDES']] <- c(0.125, 0, 0)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  # reset
  model.params.list[['MDES']] <- model.params.default[['MDES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  model.params.list[['ICC.2']] <- rep(0.7, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  model.params.list[['ICC.2']] <- model.params.default[['ICC.2']]
  
  
  model.params.list[['ICC.3']] <- rep(0.7, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  model.params.list[['ICC.3']] <- model.params.default[['ICC.3']]
  
  
  model.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  model.params.list[['ICC.2']] <- model.params.default[['ICC.2']]
  
  model.params.list[['ICC.3']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  model.params.list[['ICC.3']] <- model.params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  model.params.list[['omega.3']] <- rep(0.8, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  model.params.list[['omega.2']] <- rep(0.8, M)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  model.params.list[['omega.2']] <- rep(0.8, M)
  model.params.list[['omega.3']] <- rep(0.8, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  
  model.params.list[['omega.2']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  
  model.params.list[['omega.3']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  
  model.params.list[['omega.2']] <- rep(0, M)
  model.params.list[['omega.3']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  model.params.list[['omega.2']] <- model.params.default[['omega.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 15 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- kappa.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.1_m3rr2rr", q = q, overwrite)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- model.params.default[['kappa.w']]
  
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  
}


#------------------------------------------------------------------#
# Blocked 3 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d3.1 & run.mdes.ss)
{
  scenarios <- 1
  # back to defaults
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['numCovar.3']] <- 0
  model.params.list[['R2.3']] <- NULL
  model.params.list[['numCovar.2']] <- 0
  model.params.list[['R2.2']] <- NULL
  # for sufficient power and stability
  model.params.list[['K']] <- 15
  model.params.list[['nbar']] <- 100
  
  mdes.results <- validate_mdes(
    model.params.list, sim.params.list,
    d_m = "d3.1_m3rr2rr", overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.1_m3rr2rr",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.1_m3rr2rr",
    typesample = 'K',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.1_m3rr2rr",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  # reset
  model.params.list <- model.params.default
  sim.params.list[['MTP']] <- sim.params.default[['MTP']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked 3 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

#------------------------------------------------------------------#
# Cluster 3 level: power
#------------------------------------------------------------------#

if(run.d3.3 & run.power)
{
  
  scenarios <- 11
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['omega.2']] <- NULL
  model.params.list[['omega.3']] <- NULL
  
  # param settings to have a decent level of power
  model.params.list[['J']] <- 40
  model.params.list[['K']] <- 20
  model.params.list[['MDES']] <- rep(0.25, M)
  model.params.list[['ICC.3']] <- rep(0.1, M)
  model.params.list[['ICC.2']] <- rep(0.1, M)
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  model.params.list[['nbar']] <- 100
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 75
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 50
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  
  # reset
  model.params.list[['nbar']] <- model.params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  model.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  
  # vary R2.2
  model.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  # reset
  model.params.list[['R2.2']] <- model.params.default[['R2.2']]
  
  # vary R3.2
  model.params.list[['R2.3']] <- rep(0.6, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  # reset
  model.params.list[['R2.3']] <- model.params.default[['R2.3']]
  
  # set all to 0
  model.params.list[['R2.1']] <- rep(0, M)
  model.params.list[['R2.2']] <- rep(0, M)
  model.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  model.params.list[['R2.2']] <- model.params.default[['R2.2']]
  model.params.list[['R2.3']] <- model.params.default[['R2.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.V']] <- model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- default.rho.matrix
  model.params.list[['rho.w0']] <- model.params.list[['rho.w1']] <- default.rho.matrix
  model.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  
  rho.default <- 0.8
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.V']] <- model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- default.rho.matrix
  model.params.list[['rho.w0']] <- model.params.list[['rho.w1']] <- default.rho.matrix
  model.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  
  # reset
  rho.default <- model.params.default[['rho.default']]
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.V']] <- model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- default.rho.matrix
  model.params.list[['rho.w0']] <- model.params.list[['rho.w1']] <- default.rho.matrix
  model.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 8 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  model.params.list[['MDES']] <- c(0.25, 0, 0)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  
  # reset
  model.params.list[['MDES']] <- rep(0.25, M)
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  model.params.list[['ICC.2']] <- rep(0.7, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  model.params.list[['ICC.2']] <- rep(0.1, M)
  
  model.params.list[['ICC.3']] <- rep(0.7, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  model.params.list[['ICC.3']] <- rep(0.1, M)
  
  model.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  model.params.list[['ICC.2']] <- rep(0.1, M)
  
  model.params.list[['ICC.3']] <- rep(0, M)
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  model.params.list[['ICC.3']] <- rep(0.1, M)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- kappa.matrix
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.3_m3rc2rc", q = q, overwrite)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- model.params.default[['kappa.w']]
  
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  
}

#------------------------------------------------------------------#
# Cluster 3 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d3.3 & run.mdes.ss)
{
  scenarios <- 1
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['omega.2']] <- NULL
  model.params.list[['omega.3']] <- NULL
  
  # param settings to have a decent level of power
  model.params.list[['J']] <- 40
  model.params.list[['K']] <- 20
  model.params.list[['MDES']] <- rep(0.25, M)
  model.params.list[['ICC.3']] <- rep(0.1, M)
  model.params.list[['ICC.2']] <- rep(0.1, M)
  
  mdes.results <- validate_mdes(
    model.params.list, sim.params.list,
    d_m = "d3.3_m3rc2rc",
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.3_m3rc2rc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.3_m3rc2rc",
    typesample = 'K',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.3_m3rc2rc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  # reset
  model.params.list <- model.params.default
  sim.params.list[['MTP']] <- sim.params.default[['MTP']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed cluster 3 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

#------------------------------------------------------------------#
# Blocked cluster: power
#------------------------------------------------------------------#

if(run.d3.2 & run.power)
{
  scenarios <- 18
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['numCovar.3']] <- 0
  model.params.list[['R2.3']] <- NULL
  model.params.list[['omega.2']] <- NULL
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  model.params.list[['nbar']] <- 100
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 75
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 4 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  model.params.list[['nbar']] <- 50
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  # reset
  model.params.list[['nbar']] <- model.params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  model.params.list[['R2.1']] <- rep(0.6, M)
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  
  # vary R2.2
  model.params.list[['R2.2']] <- rep(0.6, M)

  model.params.list[['K']] <- 20
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  model.params.list[['K']] <- model.params.default[['K']]

  model.params.list[['J']] <- 50

  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)

  model.params.list[['J']] <- model.params.default[['J']]
  
  # reset
  model.params.list[['R2.2']] <- model.params.default[['R2.2']]
  
  # set all to 0
  model.params.list[['R2.1']] <- rep(0, M)
  model.params.list[['R2.2']] <- rep(0, M)
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  # reset
  model.params.list[['R2.1']] <- model.params.default[['R2.1']]
  model.params.list[['R2.2']] <- model.params.default[['R2.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.V']] <- model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- default.rho.matrix
  model.params.list[['rho.w0']] <- model.params.list[['rho.w1']] <- default.rho.matrix
  model.params.list[['rho.r']] <- default.rho.matrix
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  
  rho.default <- 0.8
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.V']] <- model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- default.rho.matrix
  model.params.list[['rho.w0']] <- model.params.list[['rho.w1']] <- default.rho.matrix
  model.params.list[['rho.r']] <- default.rho.matrix
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  
  # reset
  rho.default <- model.params.default[['rho.default']]
  model.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  model.params.list[['rho.V']] <- model.params.list[['rho.X']] <- model.params.list[['rho.C']] <- default.rho.matrix
  model.params.list[['rho.u0']] <- model.params.list[['rho.u1']] <- default.rho.matrix
  model.params.list[['rho.w0']] <- model.params.list[['rho.w1']] <- default.rho.matrix
  model.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  model.params.list[['MDES']] <- c(0.125, 0, 0)
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  
  # reset
  model.params.list[['MDES']] <- model.params.default[['MDES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  # ICC 2
  model.params.list[['ICC.2']] <- rep(0.7, M)
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  model.params.list[['ICC.2']] <- model.params.default[['ICC.2']]
  
  # ICC 3
  model.params.list[['ICC.3']] <- rep(0.7, M)
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  model.params.list[['ICC.3']] <- model.params.default[['ICC.3']]
  
  # ICC 2 = 0
  model.params.list[['ICC.2']] <- rep(0, M)
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  model.params.list[['ICC.2']] <- model.params.default[['ICC.2']]
  
  # ICC 3 = 0
  model.params.list[['ICC.3']] <- rep(0, M)
  
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  model.params.list[['ICC.3']] <- model.params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 16 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  model.params.list[['omega.3']] <- rep(0.8, M)
  model.params.list[['K']] <- 20
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  
  
  model.params.list[['omega.3']] <- rep(0, M)
  model.params.list[['K']] <- model.params.default[['K']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  
  # vary both omega and ICC
  model.params.list[['omega.3']] <- rep(0.8, M)
  model.params.list[['ICC.3']] <- rep(0.7, M)
  model.params.list[['K']] <- 20
  
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  model.params.list[['ICC.3']] <- model.params.default[['ICC.3']]
  model.params.list[['K']] <- model.params.default[['K']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 18 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- kappa.matrix
  model.params.list[['omega.3']] <- NULL
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3ff2rc", q = q, overwrite)
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  power.results <- validate_power(model.params.list, sim.params.list, d_m = "d3.2_m3rr2rc", q = q, overwrite)
  model.params.list[['kappa.w']] <- model.params.list[['kappa.u']] <- model.params.default[['kappa.w']]
  
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  
}

#------------------------------------------------------------------#
# Cluster 3 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d3.2 & run.mdes.ss)
{
  scenarios <- 2
  model.params.list <- model.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['B']] <- NULL
  
  # assumptions
  model.params.list[['numCovar.3']] <- 0
  model.params.list[['R2.3']] <- NULL
  model.params.list[['omega.2']] <- NULL
  
  model.params.list[['omega.3']] <- NULL
  mdes.results <- validate_mdes(
    model.params.list, sim.params.list,
    d_m = "d3.2_m3ff2rc",
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.2_m3ff2rc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.2_m3ff2rc",
    typesample = 'K',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.2_m3ff2rc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  model.params.list[['omega.3']] <- model.params.default[['omega.3']]
  
  mdes.results <- validate_mdes(
    model.params.list, sim.params.list,
    d_m = "d3.2_m3rr2rc",
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.2_m3rr2rc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.2_m3rr2rc",
    typesample = 'K',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    model.params.list, sim.params.list,
    d_m = "d3.2_m3rr2rc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  # reset
  model.params.list <- model.params.default
  sim.params.list[['MTP']] <- sim.params.default[['MTP']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked cluster scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
