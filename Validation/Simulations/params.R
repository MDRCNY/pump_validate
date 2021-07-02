
#------------------------------------------------------------------#
# source files
#------------------------------------------------------------------#

# MTP Westfall-Young
source(here::here("Validation/Simulations", "adjust_WY.R"))
# data generating function
source(here::here("Validation/Simulations", "gen_data.R"))
# Estimate power for each of the designs by running data generating function S sample times and fitting models to estimate power.
source(here::here("Validation/Simulations", "estimate_power_with_simulation.R"))
# Wrapping and creating 
source(here::here("Validation/Simulations", "validate_power.R"))
# For coloring texts and other purposes
source(here::here("Validation/Simulations", "misc.R"))

#------------------------------------------------------------------#
# simulation parameters
#------------------------------------------------------------------#

sim.params.list <- list(
  S = 10                     # Number of samples for Monte Carlo Simulation
  , Q = 1                   # Number of times entire simulation is repeated, so total iterations = S * Q
  , B = NULL                 # Number of samples for WestFall-Young. The equivalent is snum in our new method.
  , alpha = 0.05             # Significance level
  , tol = 0.01               # tolerance for MDES and sample  size calculations
  , Tbar = 0.5               # Binomial assignment probability
  , tnum = 10000             # Number of test statistics (samples) for all procedures other than Westfall-Young
  , parallel = TRUE          # parallelize within each monte carlo iteration
  , ncl = 8                  # Number of computer clusters (max on RStudio Server is 16)
  , start.tnum = 2000        # number of iterations for starting to testing mdes and power
  , final.tnum = 100000      # final number of iterations to check power
  , max.steps = 20           # maximum number of iterations for MDES or sample size calculations
  , max.cum.tnum = 10000000  # maximum cumulative tnum for MDES and sample size
  , procs = c("Bonferroni", "BH", "Holm") # Multiple testing procedures
  , runSim = TRUE        # If TRUE, we will re-run the simulation. If FALSE, we will pull previous run result.
  , runPump = TRUE         # If TRUE, we will run method from our package. If FALSE, we will pull previous run result.
  , runPowerUp = TRUE       # If TRUE, we will run method from powerup. If FALSE, we will pull previous run result.
)

#------------------------------------------------------------------#
# specific user parameters
# optional: if you want more complicated parameter choices, set them yourself
# otherwise reasonable defaults are assumed
#------------------------------------------------------------------#

# rho.default <- NULL

### school and district assignments
# N-length vector of individual school assignments i.e. (1,1,2,2,3,3)
# S.id <-
# N-length vector of individual district assignments i.e. (1,1,1,2,2,2)
# D.id <-

### covariates
# rho.V <- 
# rho.X <- 
# rho.C <- 

### random effects and impacts
# rho.w <- 
# rho.z <- 
# rho.u <-
# rho.v <- 
# rho.r <- 
# theta.wz <-
# theta.uv <-

#------------------------------------------------------------------#
# default user parameters
# assumes same correlation structure across all outcomes, covariates, random effects, etc.
# assumes equal sized schools that are evenly split between districts
#------------------------------------------------------------------#

M <- 3
rho.default <- 0.5
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)

user.params.list <- list(
  M = 3                                   # number of outcomes
  , J = 20                                # number of schools
  , K = 10                                # number of districts (for two-level model, set K = 1)
  , nbar = 50                             # number of individuals per school
  , rho.default = rho.default             # default rho value (optional)
  , S.id = NULL                           # N-length vector of indiv school assignments (optional)
  , D.id = NULL                           # N-length vector of indiv district assignments (optional)
  ################################################## grand mean otucome and impact
  , Xi0 = 0                               # scalar grand mean outcome under no treatment
  , ATE_ES = rep(0.125, M)                # minimum detectable effect size      
  ################################################## level 3: districts
  , R2.3 = rep(0.1, M)                    # percent of district variation explained by district covariates
  , rho.V = default.rho.matrix            # MxM correlation matrix of district covariates
  , ICC.3 = rep(0.2, M)                   # district intraclass correlation
  , omega.3 = 0.1                         # ratio of district effect size variability to random effects variability
  , rho.w0 = default.rho.matrix           # MxM matrix of correlations for district random effects
  , rho.w1 = default.rho.matrix           # MxM matrix of correlations for district impacts
  , theta.w = matrix(0, M, M)             # MxM matrix of correlations between district random effects and impacts
  ################################################## level 2: schools
  , R2.2 = rep(0.1, M)                    # percent of school variation explained by school covariates
  , rho.X = default.rho.matrix            # MxM correlation matrix of school covariates
  , ICC.2 = rep(0.2, M)                   # school intraclass correlation	
  , omega.2 = 0.1                         # ratio of school effect size variability to random effects variability
  , rho.u0 = default.rho.matrix           # MxM matrix of correlations for school random effects
  , rho.u1 = default.rho.matrix           # MxM matrix of correlations for school impacts
  , theta.u = matrix(0, M, M)             # MxM matrix of correlations between school random effects and impacts
  ################################################## level 1: individuals
  , R2.1 = rep(0.1, M)                    # percent of indiv variation explained by indiv covariates
  , rho.C = default.rho.matrix            # MxM correlation matrix of individual covariates
  , rho.r = default.rho.matrix            # MxM matrix of correlations for individual residuals 
)
