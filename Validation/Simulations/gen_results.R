###########################################################################
# script that produces output results to be put into an r markdown file
###########################################################################

library(here)

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

design <- "blocked_i1_2c"

sim.params.list <- list(
  S = 100           # Number of samples for Monte Carlo Simulation
  , B = 2            # Number of samples for WestFall-Young. The equivalent is snum in our new method.
  , maxT = FALSE     # In WY procedure, whether to adjust based on ordered rawp values or ordered rawT values
  , alpha = 0.05     # Significance level
  , MoE = 0.05       # Margin of error
  , p.j = 0.5        # Binomial assignment probability within a school
  , tnum = 10000     # Number of test statistics (samples) for all procedures other than Westfall-Young
  , ncl = 2          # Number of computer clusters (max on RStudio Server is 16)
  , procs = c("Bonferroni", "BH", "Holm")
                     # Multiple testing procedures to compute power for 
  , runSim = TRUE    # If TRUE, we will re-run the simulation. If FALSE, we will pull previous run result.
  , runPump = TRUE   # If TRUE, we will run method from our package. If FALSE, we will pull previous run result.
  , check = FALSE    # Run checks such as printing out quantities
)


# optional: if you want more complicated parameter choices, set them yourself
# otherwise reasonable defaults are assumed

# rho.default <- NULL

### school and district assignments
# N-length vector of individual school assignments i.e. (1,1,2,2,3,3)
# S.j <-
# N-length vector of individual district assignments i.e. (1,1,1,2,2,2)
# S.k <-

### covariates
# rho.D <- 
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

# assumes same correlation structure across all outcomes, covariates, random effects, etc.
# assumes equal sized schools that are evenly split between districts

M <- 3
rho.default <- 0.5
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)

user.params.list <- list(
  M = 3                                   # number of outcomes
  , J = 20                                # number of schools
  , K = 2                                 # number of districts (still required for two-level model)
  , N = 100*20                            # number of individuals
  , n.j = 100                             # number of individuals per school
  , rho.default = rho.default             # default rho value (optional)
  , S.j = NULL                            # N-length vector of indiv school assignments (optional)
  , S.k = NULL                            # N-length vector of indiv district assignments (optional)
  ################################################## grand mean otucome and impact
  , Xi0 = 0                               # scalar grand mean outcome under no treatment
  , MDES = rep(0.125, M)                  # minimum detectable effect size      
  ################################################## level 3: districts
  , R2.3 = rep(0, M)                      # percent of district variation explained by district covariates
  # for 2-level model, set to 0
  , rho.D = default.rho.matrix            # MxM correlation matrix of district covariates
  , ICC.3 = rep(0, M)                     # district intraclass correlation
  # for 2-level model, set to 0
  , omega.3 = 0                           # ratio of district effect size variability to random effects variability
  , rho.w = default.rho.matrix            # MxM matrix of correlations for district random effects
  , rho.z = default.rho.matrix            # MxM matrix of correlations for district impacts
  , theta.wz = matrix(0, M, M)            # MxM matrix of correlations between district random effects and impacts
  ################################################## level 2: schools
  , R2.2 = rep(0, M)                      # percent of school variation explained by school covariates
  , rho.X = default.rho.matrix            # MxM correlation matrix of school covariates
  , ICC.2 = rep(0.5, M)                   # school intraclass correlation	
  , omega.2 = 0                           # ratio of school effect size variability to random effects variability
  , rho.u = default.rho.matrix            # MxM matrix of correlations for school random effects
  , rho.v = default.rho.matrix            # MxM matrix of correlations for school impacts
  , theta.uv = matrix(0, M, M)            # MxM matrix of correlations between school random effects and impacts
  ################################################## level 1: individuals
  , R2.1 = rep(0, M)                      # percent of indiv variation explained by indiv covariates
  , rho.C = default.rho.matrix            # MxM correlation matrix of individual covariates
  , rho.r = default.rho.matrix            # MxM matrix of correlations for individual residuals 
)

compare_results <- validate_power(
  user.params.list = user.params.list,
  sim.params.list = sim.params.list, 
  design = design
)
