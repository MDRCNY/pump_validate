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

sim.params.list <- list(
  S = 1000           # Number of samples for Monte Carlo Simulation
  , B = 1000         # Number of samples for WestFall-Young. The equivalent is snum in our new method.
  , maxT = FALSE     # In WY procedure, whether to adjust based on ordered rawp values or ordered rawT values
  , alpha = 0.05     # Significance level
  , MoE = 0.05       # Margin of error
  , p.j = 0.5        # Binomial assignment probability
  , tnum = 10000     # Number of test statistics (samples) for all procedures other than Westfall-Young
  , ncl = parallel::detectCores()
                     # Number of computer clusters (max on RStudio Server is 16)
  , max.iter = 100   # maximum number of iterations for MDES or sample size calculations
  , procs = c("Bonferroni", "BH", "Holm", "WY-SS", "WY-SD")
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
# S.jk <-
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
  , K = 1                                 # number of districts (still required for two-level model)
  , n.j = 75                              # number of individuals per school
  , rho.default = rho.default             # default rho value (optional)
  , S.jk = NULL                           # N-length vector of indiv school assignments (optional)
  , S.k = NULL                            # N-length vector of indiv district assignments (optional)
  ################################################## grand mean otucome and impact
  , Xi0 = 0                               # scalar grand mean outcome under no treatment
  , ATE_ES = rep(0.125, M)                # minimum detectable effect size      
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
  , ICC.2 = rep(0, M)                     # school intraclass correlation	
  , omega.2 = 0.5                         # ratio of school effect size variability to random effects variability
  , rho.u = default.rho.matrix            # MxM matrix of correlations for school random effects
  , rho.v = default.rho.matrix            # MxM matrix of correlations for school impacts
  , theta.uv = matrix(0, M, M)            # MxM matrix of correlations between school random effects and impacts
  ################################################## level 1: individuals
  , R2.1 = rep(0, M)                      # percent of indiv variation explained by indiv covariates
  , rho.C = default.rho.matrix            # MxM correlation matrix of individual covariates
  , rho.r = default.rho.matrix            # MxM matrix of correlations for individual residuals 
)

overwrite = TRUE
scenarios = 24

### vary sample size

user.params.list[['n.j']] <- 100
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)
user.params.list[['omega.2']] = 0.5
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", overwrite)
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)

print('--------------------------------------------------------')
print(paste('Completed 3 out of', scenarios))
print('--------------------------------------------------------')

user.params.list[['n.j']] <- 75
user.params.list[['omega.2']] = 0
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)
user.params.list[['omega.2']] = 0.5
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", overwrite)
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)

print('--------------------------------------------------------')
print(paste('Completed 6 out of', scenarios))
print('--------------------------------------------------------')

user.params.list[['n.j']] <- 50
user.params.list[['omega.2']] = 0
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)
user.params.list[['omega.2']] = 0.5
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", overwrite)
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)

print('--------------------------------------------------------')
print(paste('Completed 9 out of', scenarios))
print('--------------------------------------------------------')

# vary R2
user.params.list[['omega.2']] = 0
user.params.list[['R2.1']] <- rep(0.6, M)
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)
user.params.list[['R2.2']] <- rep(0.6, M)
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)

print('--------------------------------------------------------')
print(paste('Completed 11 out of', scenarios))
print('--------------------------------------------------------')

# vary rho
rho.default <- 0.2
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
user.params.list[['rho.default']] <- rho.default
user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)

rho.default <- 0.8
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
user.params.list[['rho.default']] <- rho.default
user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)

print('--------------------------------------------------------')
print(paste('Completed 13 out of', scenarios))
print('--------------------------------------------------------')

# vary true positives
user.params.list[['ATE_ES']] = c(0.125, 0, 0)
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)

# vary ICC.2
user.params.list[['ATE_ES']] = c(0.125, 0.125, 0.125)
user.params.list[['ICC.2']] = rep(0.5, M)
user.params.list[['omega.2']] = 0.5
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)

print('--------------------------------------------------------')
print(paste('Completed 15 out of', scenarios))
print('--------------------------------------------------------')

# calculate MDES and sample size

## back to defaults
user.params.list[['n.j']] <- 50
rho.default <- 0.5
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
user.params.list[['rho.default']] <- rho.default
user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
user.params.list[['ATE_ES']] = c(0.125, 0.125, 0.125)
user.params.list[['R2.1']] <- rep(0, M)
user.params.list[['R2.2']] <- rep(0, M)
user.params.list[['ICC.2']] = rep(0, M)
user.params.list[['omega.2']] = 0
# don't do WY for now
sim.params.list[['procs']] = c("Bonferroni", "BH", "Holm")

power.results.2c <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)
mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)
sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)

print('--------------------------------------------------------')
print(paste('Completed 18 out of', scenarios))
print('--------------------------------------------------------')

user.params.list[['omega.2']] = 0.5
power.results.2f <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", overwrite)
mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_2f", overwrite)
sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_2f", overwrite)

print('--------------------------------------------------------')
print(paste('Completed 21 out of', scenarios))
print('--------------------------------------------------------')

user.params.list[['omega.2']] = 0.5
power.results.2r <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)
mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)
sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)

print('--------------------------------------------------------')
print(paste('Completed 24 out of', scenarios))
print('--------------------------------------------------------')
