context("Test Data Generating Process")

library(here)
library(MASS)
library(testthat)

# data generating function
source(here::here("Validation/Simulations", "gen_data.R"))
source(here::here("Validation/Simulations", "estimate_power_with_simulation.R"))
source(here::here("Validation/Simulations", "misc.R"))

sim.params.list <- list(
  S = 10                # Number of samples for Monte Carlo Simulation
  , Q = 1                 # Number of times entire simulation is repeated, so total iterations = S * Q
  , B = 2                 # Number of samples for WestFall-Young. The equivalent is snum in our new method.
  , maxT = TRUE           # In WY procedure, whether to adjust based on ordered rawp values or ordered rawT values
  , alpha = 0.05          # Significance level
  , tol = 0.01            # tolerance for MDES and sample  size calculations
  , Tbar = 0.5            # Binomial assignment probability
  , tnum = 1000           # Number of test statistics (samples) for all procedures other than Westfall-Young
  , parallel = TRUE       # parallelize within each monte carlo iteration
  , ncl = 2               # Number of computer clusters (max on RStudio Server is 16)
  , start.tnum = 100      # number of iterations for starting to testing mdes and power
  , final.tnum = 200      # final number of iterations to check power
  , max.steps = 20        # maximum number of iterations for MDES or sample size calculations
  , max.cum.tnum = 1000 # maximum cumulative tnum for MDES and sample size
  , procs = c("Bonferroni", "BH", "Holm")
  # , procs = c("Bonferroni", "BH", "Holm", "WY-SS", "WY-SD")
  # Multiple testing procedures
  , runSim = TRUE         # If TRUE, we will re-run the simulation. If FALSE, we will pull previous run result.
  , runPump = TRUE        # If TRUE, we will run method from our package. If FALSE, we will pull previous run result.
  , runPowerUp = TRUE     # If TRUE, we will run method from powerup. If FALSE, we will pull previous run result.
)

M <- 2
rho.default <- 0.5
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)

user.params.list <- list(
  M = 2                                   # number of outcomes
  , J = 20                                # number of schools
  , K = 10                                # number of districts (for two-level model, set K = 1)
  , nbar = 50                             # number of individuals per school
  , rho.default = rho.default             # default rho value (optional)
  , S.id = NULL                           # N-length vector of indiv school assignments (optional)
  , D.id = NULL                           # N-length vector of indiv district assignments (optional)
  ################################################## grand mean otucome and impact
  , Xi0 = 0                               # scalar grand mean outcome under no treatment
  , ATE_ES = rep(0, M)                    # minimum detectable effect size      
  ################################################## level 3: districts
  , R2.3 = rep(0, M)                      # percent of district variation explained by district covariates
  , rho.V = default.rho.matrix            # MxM correlation matrix of district covariates
  , ICC.3 = rep(0, M)                     # district intraclass correlation
  , omega.3 = 0                           # ratio of district effect size variability to random effects variability
  , rho.w0 = default.rho.matrix           # MxM matrix of correlations for district random effects
  , rho.w1 = default.rho.matrix           # MxM matrix of correlations for district impacts
  , theta.w = matrix(0, M, M)             # MxM matrix of correlations between district random effects and impacts
  ################################################## level 2: schools
  , R2.2 = rep(0, M)                      # percent of school variation explained by school covariates
  , rho.X = default.rho.matrix            # MxM correlation matrix of school covariates
  , ICC.2 = rep(0, M)                     # school intraclass correlation	
  , omega.2 = 0                           # ratio of school effect size variability to random effects variability
  , rho.u0 = default.rho.matrix           # MxM matrix of correlations for school random effects
  , rho.u1 = default.rho.matrix           # MxM matrix of correlations for school impacts
  , theta.u = matrix(0, M, M)             # MxM matrix of correlations between school random effects and impacts
  ################################################## level 1: individuals
  , R2.1 = rep(0, M)                      # percent of indiv variation explained by indiv covariates
  , rho.C = default.rho.matrix            # MxM correlation matrix of individual covariates
  , rho.r = default.rho.matrix            # MxM matrix of correlations for individual residuals 
)

params.default <- user.params.list
sim.params.default <- sim.params.list

# --------------------------------------------
# start with all parameters at 0
# --------------------------------------------
design <- "blocked_i1_2c"
user.params.list[['K']] <- 1
user.params.list[['ICC.3']] <- NULL
user.params.list[['omega.3']] <- NULL
user.params.list[['R2.3']] <- NULL
user.params.list[['omega.2']] <- 0
user.params.list[['ICC.2']] <- rep(0, M)

model.params.list <- convert.params(user.params.list)
samp.full <- gen_full_data(model.params.list)
exp.var <- 1

test_that('Tau0 equation is correct', {
  expect_equal(model.params.list$tau0.sq[1], 0)
  expect_equal(model.params.list$tau0.sq[2], 0)
})

test_that('ICC equation is correct', {
  expect_equal(user.params.list$ICC.2[1], 0)
  expect_equal(user.params.list$ICC.2[1], model.params.list$tau0.sq[1]/(model.params.list$tau0.sq[1] + 1))
})

test_that(
  'Residual variances are within 10% of expected variance', {
  expect_equal(round(var(samp.full$Y0[,1]), 2), 1, tolerance = 0.1)
  expect_equal(round(var(samp.full$Y0[,2]), 2), 1, tolerance = 0.1)
})

test_that('Treatment impact is correct',{
  expect_equal(user.params.list$ATE_ES * sqrt(model.params.list$tau0.sq + 1), rep(0, M))
  expect_equal(unname(apply(samp.full$Y1 - samp.full$Y0, 2, mean)), rep(0, M))
})

test_that('Outcomes are within 10% of expected correlation = 0',{
  expect_equal(cor(samp.full$Y0[,1], samp.full$Y0[,2]), rho.default, tolerance = 0.1)
})

# --------------------------------------------
# test when Effect size = 0.125
# --------------------------------------------

user.params.list[['ATE_ES']] <- rep(0.125, M)
model.params.list <- convert.params(user.params.list)
samp.full <- gen_full_data(model.params.list)
exp.var <- 1

test_that('Tau0 equation is correct for ATE_ES = 0.125', {
  expect_equal(model.params.list$tau0.sq[1], 0)
  expect_equal(model.params.list$tau0.sq[2], 0)
})

test_that(
  'Residual variances are within 10% of expected variance for ATE_ES = 0.125', {
    expect_equal(round(var(samp.full$Y0[,1]), 2), 1, tolerance = 0.1)
    expect_equal(round(var(samp.full$Y0[,2]), 2), 1, tolerance = 0.1)
})

test_that('ICC equation is correct', {
  expect_equal(user.params.list$ICC.2[1], 0)
  expect_equal(user.params.list$ICC.2[1], model.params.list$tau0.sq[1]/(model.params.list$tau0.sq[1] + 1))
})

test_that('Treatment impact is correct for ATE_ES = 0.125',{
  expect_equal(user.params.list$ATE_ES * sqrt(model.params.list$tau0.sq + 1), rep(0.125, M))
  expect_equal(unname(apply(samp.full$Y1 - samp.full$Y0, 2, mean)), rep(0.125, M))
})

test_that('Outcomes are within 10% of expected correlation = 0',{
  expect_equal(cor(samp.full$Y0[,1], samp.full$Y0[,2]), rho.default, tolerance = 0.1)
})

# --------------------------------------------
# test when ICC.2 = 0.5 (which actually adds some variance)
# --------------------------------------------

user.params.list[['ICC.2']] <- rep(0.5, M)
model.params.list <- convert.params(user.params.list)
samp.full <- gen_full_data(model.params.list)
exp.var <- model.params.list$tau0.sq[1] + 1

test_that('Tau0 equation is correct for ICC.2 = 0.5', {
  expect_equal(model.params.list$tau0.sq[1], 1)
  expect_equal(model.params.list$tau0.sq[1], 1)
})

test_that('ICC equation is correct', {
  expect_equal(user.params.list$ICC.2[1], 0.5)
  expect_equal(user.params.list$ICC.2[1], model.params.list$tau0.sq[1]/(model.params.list$tau0.sq[1] + 1))
})

test_that(
  'Residual variances are within 40% of expected variance for ATE_ES = 0.125', {
  expect_equal(round(var(samp.full$Y0[,1]), 2), exp.var, tolerance = 0.4, scale = exp.var)
  expect_equal(round(var(samp.full$Y0[,2]), 2), exp.var, tolerance = 0.4, scale = exp.var)
})

test_that('Treatment impact is correct for ICC.2 = 0.5',{
  expect_equal(user.params.list$ATE_ES * sqrt(model.params.list$tau0.sq + 1),
               unname(apply(samp.full$Y1 - samp.full$Y0, 2, mean)))
})

test_that('Outcomes are within 10% of expected correlation = 0',{
  expect_equal(cor(samp.full$Y0[,1], samp.full$Y0[,2]), rho.default, tolerance = 0.1)
})

# --------------------------------------------
# Allow for correlation between outcomes of 0.5
# --------------------------------------------

rho.default <- 0.5
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
user.params.list[['rho.u']] <- default.rho.matrix
model.params.list <- convert.params(user.params.list)
samp.full <- gen_full_data(model.params.list)
exp.var <- model.params.list$tau0.sq[1] + 1

test_that('Tau0 equation is correct for rho.default = 0.5', {
  expect_equal(model.params.list$tau0.sq[1], 1)
  expect_equal(model.params.list$tau0.sq[2], 1)
})

test_that('ICC equation is correct', {
  expect_equal(user.params.list$ICC.2[1], 0.5)
  expect_equal(user.params.list$ICC.2[1], model.params.list$tau0.sq[1]/(model.params.list$tau0.sq[1] + 1))
})

test_that(
  'Residual variances are within 40% of expected variance for rho.default = 0.5', {
  expect_equal(round(var(samp.full$Y0[,1]), 2), exp.var, tolerance = 0.4, scale = exp.var)
  expect_equal(round(var(samp.full$Y0[,2]), 2), exp.var, tolerance = 0.4, scale = exp.var)
})

test_that('Treatment impact is correct for rho.default = 0.5',{
  expect_equal(user.params.list$ATE_ES * sqrt(model.params.list$tau0.sq + 1),
               unname(apply(samp.full$Y1 - samp.full$Y0, 2, mean)))
})

test_that('Outcomes are positively correlated for rho.default = 0.5',{
  expect_gt(cor(samp.full$Y0[,1], samp.full$Y0[,2]), 0)
})

# --------------------------------------------
# Test observed code conversion
# --------------------------------------------

samp.obs <- samp.full
T.ijk <- rep(1, length(samp.full$Y0))
samp.obs$Yobs <- gen_Yobs(samp.full, T.ijk)

test_that(
  'Observed data matches true data for all treated',
  expect_equal(samp.obs$Yobs[,1], samp.full$Y1[,1])
)

T.ijk <- rep(0, length(samp.full$Y0))
samp.obs$Yobs <- gen_Yobs(samp.full, T.ijk)
test_that(
  'Observed data matches true data for all control',
  expect_equal(samp.obs$Yobs[,1], samp.full$Y0[,1])
)

T.ijk <- c(rep(0, length(samp.full$Y0)/2), rep(1, length(samp.full$Y0)/2))
samp.obs$Yobs <- gen_Yobs(samp.full, T.ijk)
test_that(
  'Observed data matches true data for a standard assignment', {
  expect_equal(samp.obs$Yobs[T.ijk == 1,1], samp.full$Y1[T.ijk == 1,1])
  expect_equal(samp.obs$Yobs[T.ijk == 0,1], samp.full$Y0[T.ijk == 0,1])
})


# --------------------------------------------
# Test estimated treatment impact
# --------------------------------------------

T.ijk <- c(rep(0, length(samp.full$Y0)/2), rep(1, length(samp.full$Y0)/2))
samp.obs$Yobs <- gen_Yobs(samp.full, T.ijk)

true.tau <- abs(mean(samp.full$Y1[,1] - samp.full$Y0[,1]))
test_that(
  'Treatment impact estimate is within 10% of true value', {
  expect_equal(
    abs(mean(samp.obs$Yobs[T.ijk == 1,1]) - mean(samp.obs$Yobs[T.ijk == 0,1])),
    true.tau, tolerance = 0.1, scale = true.tau)
})


# --------------------------------------------
# Test block assignments
# --------------------------------------------

S.id <- samp.full$ID$S.id
D.id <- samp.full$ID$D.id

test_that('The number of blocks is correct',{
  expect_equal(length(unique(S.id)), user.params.list$J)
})

test_that('The number of units in each block is correct',{
  expect_equal(as.numeric(unname(table(S.id))), rep(user.params.list$nbar, user.params.list$J))
})

# --------------------------------------------
# Test block treatment assignment
# --------------------------------------------

T.x <- randomizr::block_ra( S.id, prob = sim.params.list$Tbar )
empirical.Tbar <- rep(NA, user.params.list[['J']])
for(j in unique(S.id))
{
  empirical.Tbar[j] = mean(T.x[S.id == j])
}

test_that('Each block has completely random assignment with correct probability',{
  expect_equal(empirical.Tbar, rep(sim.params.list[['Tbar']], user.params.list[['J']]))
})
