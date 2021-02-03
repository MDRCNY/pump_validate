context("Test Data Generating Process")

library(here)
library(MASS)
library(testthat)

set.seed(0130)

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
  , K = 10                                 # number of districts (for two-level model, set K = 1)
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

user.params.default <- user.params.list
sim.params.default <- sim.params.list

exp.Y0.var.fun = function(model.params.list)
{
  return(
    ifelse(is.null(model.params.list[['xi']]), 0, model.params.list[['xi']][1]^2) +
    ifelse(is.null(model.params.list[['eta0.sq']]), 0, model.params.list[['eta0.sq']][1]) +
    model.params.list[['delta']][1]^2 + model.params.list[['tau0.sq']][1] +
    model.params.list[['gamma']][1]^2 + 1
  )
}

exp.psi.fun = function(user.params.list, model.params.list)
{
  return(
    user.params.list[['ATE_ES']] *
    sqrt(ifelse(is.null(model.params.list[['xi']]), 0, model.params.list[['xi']][1]^2) +
         ifelse(is.null(model.params.list[['eta0.sq']]), 0, model.params.list[['eta0.sq']][1]) +
         model.params.list[['delta']][1]^2 + model.params.list[['tau0.sq']][1] +
         model.params.list[['gamma']][1]^2 + 1
  ))
}

# --------------------------------------------
# start with all parameters at 0
# --------------------------------------------

design <- "blocked_i1_2c"
user.params.list[['K']] <- 1
user.params.list[['ICC.3']] <- NULL
user.params.list[['omega.3']] <- NULL
user.params.list[['R2.3']] <- NULL

user.params.list[['ATE_ES']] <- rep(0, M) 
user.params.list[['omega.2']] <- 0
user.params.list[['ICC.2']] <- rep(0, M)
user.params.list[['R2.1']] <- rep(0, M)
user.params.list[['R2.2']] <- rep(0, M)
rho.default <- 0
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
user.params.list[['rho.default']] <- rho.default
user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix

model.params.list <- convert.params(user.params.list)
samp.full <- gen_full_data(model.params.list)

exp.Y0.var <- exp.Y0.var.fun(model.params.list)
test_that(
  'Y0 variances are within 40% of expected variance', {
  expect_equal(round(var(samp.full$Y0[,1]), 2), exp.Y0.var, tolerance = 0.4)
  expect_equal(round(var(samp.full$Y0[,2]), 2), exp.Y0.var, tolerance = 0.4)
})

exp.psi <- exp.psi.fun(user.params.list, model.params.list)
test_that('Treatment impact is correct',{
  expect_equal(unname(apply(samp.full$Y1 - samp.full$Y0, 2, mean)), exp.psi)
})

test_that('Outcomes are within 30% of expected correlation = 0',{
  expect_equal(cor(samp.full$Y0[,1], samp.full$Y0[,2]), rho.default, tolerance = 0.3)
})

# --------------------------------------------
# test when Effect size = 0.125
# --------------------------------------------

user.params.list[['ATE_ES']] <- rep(0.125, M)
model.params.list <- convert.params(user.params.list)
samp.full <- gen_full_data(model.params.list)

exp.Y0.var <- exp.Y0.var.fun(model.params.list)
test_that(
  'Y0 variances are within 40% of expected variance for ATE_ES = 0.125', {
  expect_equal(round(var(samp.full$Y0[,1]), 2), exp.Y0.var, tolerance = 0.4)
  expect_equal(round(var(samp.full$Y0[,2]), 2), exp.Y0.var, tolerance = 0.4)
})

exp.psi <- exp.psi.fun(user.params.list, model.params.list)
test_that('Treatment impact is correct for ATE_ES = 0.125',{
  expect_equal(unname(apply(samp.full$Y1 - samp.full$Y0, 2, mean)), exp.psi)
})

test_that('Outcomes are within 30% of expected correlation = 0',{
  expect_equal(cor(samp.full$Y0[,1], samp.full$Y0[,2]), rho.default, tolerance = 0.3)
})

# --------------------------------------------
# test when ICC.2 = 0.5 (which actually adds some variance)
# --------------------------------------------

user.params.list[['ICC.2']] <- rep(0.5, M)
model.params.list <- convert.params(user.params.list)
samp.full <- gen_full_data(model.params.list)

exp.Y0.var <- exp.Y0.var.fun(model.params.list)
test_that(
  'Y0 variances are within 40% of expected variance for ICC.2 = 0.5', {
  expect_equal(round(var(samp.full$Y0[,1]), 2), exp.Y0.var, tolerance = 0.4)
  expect_equal(round(var(samp.full$Y0[,2]), 2), exp.Y0.var, tolerance = 0.4)
})

exp.psi <- exp.psi.fun(user.params.list, model.params.list)
test_that('Treatment impact is correct for ICC.2 = 0.5',{
  expect_equal(unname(apply(samp.full$Y1 - samp.full$Y0, 2, mean)), exp.psi)
})

test_that('Outcomes are within 30% of expected correlation = 0',{
  expect_equal(cor(samp.full$Y0[,1], samp.full$Y0[,2]), rho.default, tolerance = 0.3)
})

# --------------------------------------------
# Allow for correlation between outcomes of 0.5
# --------------------------------------------

rho.default <- 0.5
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
user.params.list[['rho.default']] <- rho.default
user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix

model.params.list <- convert.params(user.params.list)
samp.full <- gen_full_data(model.params.list)

exp.Y0.var <- exp.Y0.var.fun(model.params.list)
test_that(
  'Y0 variances are within 40% of expected variance for rho.default = 0.5', {
  expect_equal(round(var(samp.full$Y0[,1]), 2), exp.Y0.var, tolerance = 0.4)
  expect_equal(round(var(samp.full$Y0[,2]), 2), exp.Y0.var, tolerance = 0.4)
})

exp.psi <- exp.psi.fun(user.params.list, model.params.list)
test_that('Treatment impact is correct for rho.default = 0.5',{
  expect_equal(unname(apply(samp.full$Y1 - samp.full$Y0, 2, mean)), exp.psi)
})

test_that('Outcomes are positively correlated for rho.default = 0.5',{
  expect_gt(cor(samp.full$Y0[,1], samp.full$Y0[,2]), 0)
})

test_that('Outcomes are within 30% of expected correlation = 0.5',{
  expect_equal(cor(samp.full$Y0[,1], samp.full$Y0[,2]), rho.default, tolerance = 0.3)
})

# --------------------------------------------
# check a 3 level model
# --------------------------------------------

user.params.list <- user.params.default

design <- "blocked_i1_3r"
model.params.list <- convert.params(user.params.list)
samp.full <- gen_full_data(model.params.list)

exp.Y0.var <- exp.Y0.var.fun(model.params.list)
test_that(
  'Y0 variances are within 40% of expected variance for level 3', {
  expect_equal(round(var(samp.full$Y0[,1]), 2), exp.Y0.var, tolerance = 0.4)
  expect_equal(round(var(samp.full$Y0[,2]), 2), exp.Y0.var, tolerance = 0.4)
})

exp.psi <- exp.psi.fun(user.params.list, model.params.list)
test_that('Treatment impact is within 10% for level 3',{
  expect_equal(unname(apply(samp.full$Y1 - samp.full$Y0, 2, mean)), exp.psi, tolerance = 0.1)
})

test_that('Outcomes are positively correlated for level 3',{
  expect_gt(cor(samp.full$Y0[,1], samp.full$Y0[,2]), 0)
})

# --------------------------------------------
# Test observed code conversion
# --------------------------------------------

samp.obs <- samp.full
T.x <- rep(1, length(samp.full$Y0))
samp.obs$Yobs <- gen_Yobs(samp.full, T.x)

test_that(
  'Observed data matches true data for all treated',
  expect_equal(samp.obs$Yobs[,1], samp.full$Y1[,1])
)

T.x <- rep(0, length(samp.full$Y0))
samp.obs$Yobs <- gen_Yobs(samp.full, T.x)
test_that(
  'Observed data matches true data for all control',
  expect_equal(samp.obs$Yobs[,1], samp.full$Y0[,1])
)

T.x <- c(rep(0, length(samp.full$Y0)/2), rep(1, length(samp.full$Y0)/2))
samp.obs$Yobs <- gen_Yobs(samp.full, T.x)
test_that(
  'Observed data matches true data for a standard assignment', {
  expect_equal(samp.obs$Yobs[T.x == 1,1], samp.full$Y1[T.x == 1,1])
  expect_equal(samp.obs$Yobs[T.x == 0,1], samp.full$Y0[T.x == 0,1])
})


# --------------------------------------------
# Test estimated treatment impact
# --------------------------------------------

T.x <- c(rep(0, length(samp.full$Y0)/2), rep(1, length(samp.full$Y0)/2))
samp.obs$Yobs <- gen_Yobs(samp.full, T.x)

true.tau <- abs(mean(samp.full$Y1[,1] - samp.full$Y0[,1]))
test_that(
  'Treatment impact estimate is within 10% of true value', {
  expect_equal(
    abs(mean(samp.obs$Yobs[T.x == 1,1]) - mean(samp.obs$Yobs[T.x == 0,1])),
    true.tau, tolerance = 0.1, scale = true.tau)
})


# --------------------------------------------
# Test block assignments
# --------------------------------------------

J <- 20
K <- 10
nbar <- 50
assignments <- gen_simple_assignments( J = J, K = K, nbar = nbar)
S.id        <- assignments[['S.id']]
D.id        <- assignments[['D.id']]

test_that('The number of schools is correct',{
  expect_equal(length(unique(S.id)), J * K)
})

test_that('The number of districts is correct',{
  expect_equal(length(unique(D.id)), K)
})

test_that('The number of units in each school is correct',{
  expect_equal(as.numeric(unname(table(S.id))), rep(nbar, J * K))
})

# --------------------------------------------
# Test treatment assignments
# --------------------------------------------

Tbar <- 0.5

# two level
K <- 1
assignments <- gen_simple_assignments( J = J, K = K, nbar = nbar)
S.id        <- assignments[['S.id']]
D.id        <- assignments[['D.id']]

T.x <- randomizr::block_ra( S.id, prob = Tbar )
empirical.Tbar <- rep(NA, J)
for(j in unique(S.id))
{
  empirical.Tbar[j] = mean(T.x[S.id == j])
}

test_that('Each block has completely random assignment with correct probability',{
  expect_equal(empirical.Tbar, rep(Tbar, J * K))
})

# cluster 2 level
T.x <- randomizr::cluster_ra( S.id, prob = Tbar )
empirical.Tbar <- rep(NA, J)
for(j in unique(S.id))
{
  empirical.Tbar[j] = sum(T.x[S.id == j])
}

test_that('Each cluster is assigned to either treatment or control',{
  expect_equal(sort(empirical.Tbar), c(rep(0, J/2), rep(nbar, J/2))) 
})

# 3 level
K <- 10
assignments <- gen_simple_assignments( J = J, K = K, nbar = nbar)
S.id        <- assignments[['S.id']]
D.id        <- assignments[['D.id']]

# cluster 3 level
T.x <- randomizr::cluster_ra( D.id, prob = Tbar )
empirical.Tbar <- rep(NA, K)
for(k in unique(D.id))
{
  empirical.Tbar[k] = sum(T.x[D.id == k])
}

test_that('Each cluster is assigned to either treatment or control',{
  expect_equal(sort(empirical.Tbar), c(rep(0, K/2), rep(nbar*J, K/2))) 
})

# blocked cluster
T.x <- randomizr::block_and_cluster_ra( blocks = D.id, clusters = S.id, prob = Tbar )
empirical.Tbar <- rep(NA, J*K)
for(j in unique(S.id))
{
  empirical.Tbar[j] = sum(T.x[S.id == j])
}

test_that('Each cluster is assigned to either treatment or control',{
  expect_equal(sort(empirical.Tbar), c(rep(0, J*K/2), rep(nbar, J*K/2))) 
})
test_that('There is an equal number of clusters for treatment and control',{
  expect_equal(sum(empirical.Tbar == 0), J*K/2) 
})
test_that('First block has clusters assigned properly',{
  expect_equal(sum(empirical.Tbar[1:J] == 0), J/2) 
})
