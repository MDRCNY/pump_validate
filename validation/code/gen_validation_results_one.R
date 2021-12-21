#------------------------------------------------------------------#
# script that produces output results for one case
#------------------------------------------------------------------#

library(here)

#------------------------------------------------------------------#
# source files
#------------------------------------------------------------------#

source(here::here("validation/code", "adjust_WY.R"))
source(here::here("validation/code", "estimate_power_with_simulation.R"))
source(here::here("validation/code", "validate_power.R"))
source(here::here("validation/code", "misc.R"))

#------------------------------------------------------------------#
# simulation parameters
#------------------------------------------------------------------#

sim.params.list <- list(
  S = 3                      # Number of samples for Monte Carlo Simulation
  , B = NULL                 # Number of samples for WestFall-Young. The equivalent is snum in our new method.
  , alpha = 0.05             # Significance level
  , tol = 0.01               # tolerance for MDES and sample  size calculations
  , Tbar = 0.5               # Binomial assignment probability
  , tnum = 1000              # Number of test statistics (samples) for all procedures other than Westfall-Young
  , parallel = TRUE          # parallelize within each monte carlo iteration
  , ncl = 8                  # Number of computer clusters
  , start.tnum = 200         # number of iterations for starting to testing mdes and power
  , final.tnum = 100000      # final number of iterations to check power
  , max.steps = 20           # maximum number of iterations for MDES or sample size calculations
  , max.cum.tnum = 10000000  # maximum cumulative tnum for MDES and sample size
  , MTP = c("Bonferroni", "BH", "Holm") # Multiple testing procedures
  , runSim = TRUE            # If TRUE, we will re-run the simulation. If FALSE, we will pull previous run result.
  , runPump = TRUE           # If TRUE, we will run method from our package. If FALSE, we will pull previous run result.
  , runPowerUp = TRUE        # If TRUE, we will run method from powerup. If FALSE, we will pull previous run result.
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
  , K = 1                                 # number of districts (for two-level model, set K = 1)
  , nbar = 50                             # number of individuals per school
  , rho.default = 0.5                     # default rho value
  ################################################## grand mean otucome and impact
  , MDES = rep(0.125, M)                  # minimum detectable effect size      
  ################################################## level 2: schools
  , numCovar.2 = 0                        # number of school covariates
  , R2.2 = rep(0, M)                      # percent of school variation explained by school covariates
  , ICC.2 = rep(0.2, M)                   # school intraclass correlation	
  , omega.2 = rep(0.1, M)                 # ratio of school effect size variability to random effects variability
  ################################################## level 1: individuals
  , numCovar.1 = 1                        # number of individual covariates
  , R2.1 = rep(0.1, M)                    # percent of indiv variation explained by indiv covariates
)

power.results <- validate_power(model.params.list, sim.params.list, d_m = "d2.1_m2fr")
print(power.results)

mdes.results <- validate_mdes(model.params.list, sim.params.list, d_m = "d2.1_m2fr")
print(mdes.results)

j.results <- validate_sample(model.params.list, sim.params.list, d_m = "d2.1_m2fr", typesample = "J")
print(j.results)

nbar.results <- validate_sample(model.params.list, sim.params.list, d_m = "d2.1_m2fr", typesample = "nbar")
print(nbar.results)
