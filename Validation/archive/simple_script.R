library(here)
library(MASS)
library(multtest)
library(RcppEigen)

# if you haven't installed multtest, code for installing from
# Bioconductor
# if (!requireNamespace("BiocManager", quietly = TRUE)){
#   install.packages("BiocManager")
# }
# BiocManager::install("multtest")


# data generating function
source(here::here("Validation/Simulations", "gen_data.R"))
# Estimate power for each of the designs by running data generating function S sample times and fitting models to estimate power.
source(here::here("Validation/Simulations", "estimate_power_with_simulation.R"))
# Wrapping and creating
# source(here::here("Validation/Simulations", "validate_power.R"))
# For coloring texts and other purposes
source(here::here("Validation/Simulations", "misc.R"))

# I am testing out adding this assumption:
# Beta_ijkm = Gamma_1,km + psi * X_jkm + v_jkm

# instead of:
# Beta_ijkm = Gamma_1,km + v_jkm

# the value of psi can be adjusted in user.params.list

# set simulation parameters
sim.params.list = list(
  S = 2000           # Number of samples for Monte Carlo Simulation
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

# set user-set model parameters

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
  , nbar = 100                             # number of individuals per school
  , rho.default = rho.default             # default rho value (optional)
  , S.jk = NULL                            # N-length vector of indiv school assignments (optional)
  , S.k = NULL                            # N-length vector of indiv district assignments (optional)
  ################################################## grand mean otucome and impact
  , Xi0 = 0                               # scalar grand mean outcome under no treatment
  , ATE_ES = rep(0.125, M)                  # minimum detectable effect size
  ################################################## level 3: districts
  , R2.3 = rep(0, M)                      # percent of district variation explained by district covariates
  # for 2-level model, set to 0
  , rho.D = default.rho.matrix            # MxM correlation matrix of district covariates
  , ICC.3 = rep(0, M)                     # district intraclass correlation
  # for 2-level model, set to 0
  , omega.3 = 0                           # ratio of district effect size variability to random effects variability
  , rho.w0 = default.rho.matrix            # MxM matrix of correlations for district random effects
  , rho.w1 = default.rho.matrix            # MxM matrix of correlations for district random effects
  , rho.V = default.rho.matrix            # MxM matrix of correlations for district impacts
  , theta.wz = matrix(0, M, M)            # MxM matrix of correlations between district random effects and impacts
  ################################################## level 2: schools
  , R2.2 = rep(0, M)                      # percent of school variation explained by school covariates
  ######## temp
  , psi = rep(0, M)                       # coefficients of school covariate in treatment effect
  ######## temp
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

# blocked_i1_2c design
# estimate power through simulation
sim.params.list$S = 10
blocked_i1_2c_power <- est_power_sim(user.params.list, sim.params.list, design = 'Blocked_i1_2c')
print(blocked_i1_2c_power)


