#------------------------------------------------------------------#
# script that produces output results to be put into an r markdown file
#------------------------------------------------------------------#

library(here)

# overwrite existing results that have already been saved?
overwrite = TRUE
# whether or not to run power, mdes and sample size
run.power = TRUE
run.mdes.ss = FALSE
# which designs to run
run.blocked.2l = TRUE
run.cluster.2l = FALSE
run.blocked.3l = FALSE
run.cluster.3l = FALSE
run.blocked.cluster = FALSE

# simulation and user parameters
source(here::here("Validation/Simulations", "params.R"))

q <- as.numeric(as.character(Sys.getenv("q")))
if(is.na(q)) { q <- 1 }

params.default <- user.params.list
sim.params.default <- sim.params.list

#------------------------------------------------------------------#
# Blocked 2 level: power
#------------------------------------------------------------------#

if(run.blocked.2l & run.power)
{
  scenarios <- 16
  
  # assumptions
  user.params.list[['K']] <- 1
  user.params.list[['ICC.3']] <- NULL
  user.params.list[['omega.3']] <- NULL
  user.params.list[['R2.3']] <- NULL
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['nbar']] <- 100
  user.params.list[['omega.2']] <- 0
  user.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  user.params.list[['omega.2']] <- 0
  user.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  user.params.list[['omega.2']] <- 0
  user.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  
  # reset
  user.params.list[['nbar']] <- params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['omega.2']] <- 0
  user.params.list[['ICC.2']] <- rep(0, M)
  
  user.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  
  # set both to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.default']] <- rho.default
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 13 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  user.params.list[['ATE_ES']] <- c(0.125, 0, 0)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.7, M)
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  # reset
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 15 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  user.params.list[['omega.2']] <- 0.8
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 16 out of', scenarios))
  print('-----------------------------------------------------------------------------')
}

#------------------------------------------------------------------#
# Blocked 2 level: MDES and sample size
#------------------------------------------------------------------#

if(run.blocked.2l & run.mdes.ss)
{
  scenarios <- 6
  # back to defaults
  user.params.list <- params.default
  # assumptions
  user.params.list[['K']] <- 1
  user.params.list[['ICC.3']] <- NULL
  user.params.list[['omega.3']] <- NULL
  user.params.list[['R2.3']] <- NULL
  
  # don't do WY for now
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")
  
  user.params.list[['omega.2']] <- 0
  user.params.list[['ICC.2']] <- rep(0, M)
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list, design = "blocked_i1_2c", 
    power.definition = 'D1indiv', overwrite = overwrite
  )
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list, design = "blocked_i1_2c", 
    power.definition = 'min2', overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list, design = "blocked_i1_2c",
    power.definition = 'D1indiv', overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list, design = "blocked_i1_2c",
    power.definition = 'min2', overwrite = overwrite
  )
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed mdes and sample size for constant, 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list, design = "blocked_i1_2f",
    power.definition = 'D1indiv', overwrite = overwrite
  )
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list, design = "blocked_i1_2f",
    power.definition = 'min2', overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list, design = "blocked_i1_2f",
    power.definition = 'D1indiv', overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list, design = "blocked_i1_2f",
    power.definition = 'min2', overwrite = overwrite
  )
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed mdes and sample size for fixed, 4 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list, design = "blocked_i1_2r",
    power.definition = 'D1indiv', overwrite = overwrite
  )
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list, design = "blocked_i1_2r",
    power.definition = 'min2', overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list, design = "blocked_i1_2r",
    power.definition = 'D1indiv', overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list, design = "blocked_i1_2r",
    power.definition = 'min2', overwrite = overwrite
  )
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed mdes and sample size for random, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  # reset
  user.params.list <- params.default
  sim.params.list[['procs']] <- sim.params.default[['procs']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked 2 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

#------------------------------------------------------------------#
# Cluster 2 level: power
#------------------------------------------------------------------#

if(run.cluster.2l & run.power)
{
  scenarios <- 10
  user.params.list <- params.default

  # assumptions
  user.params.list[['K']] <- 1
  user.params.list[['ICC.3']] <- NULL
  user.params.list[['omega.3']] <- NULL
  user.params.list[['R2.3']] <- NULL
  user.params.list[['omega.2']] <- 0
  
  # params to help have a decent power
  user.params.list[['ICC.2']] <- rep(0.1, M)
  user.params.list[['J']] <- 60
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['nbar']] <- 100
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  # reset
  user.params.list[['nbar']] <- params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  
  # set both to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 5 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.default']] <- rho.default
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 7 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  user.params.list[['ATE_ES']] <- c(0.125, 0, 0)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  user.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list <- params.default
}

#------------------------------------------------------------------#
# Cluster 2 level: MDES and sample size
#------------------------------------------------------------------#

if(run.cluster.2l & run.mdes.ss)
{
  scenarios <- 1
  # back to defaults
  user.params.list <- params.default
  # don't do WY for now
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")
  
  # assumptions
  user.params.list[['K']] <- 1
  user.params.list[['ICC.3']] <- NULL
  user.params.list[['omega.3']] <- NULL
  user.params.list[['R2.3']] <- NULL
  user.params.list[['omega.2']] <- 0
  
  # params to help have a decent power
  user.params.list[['ICC.2']] <- rep(0.1, M)
  user.params.list[['J']] <- 60
  
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "simple_c2_2r", overwrite = overwrite)
  sample.results <- validate_sample(user.params.list, sim.params.list, design = "simple_c2_2r", overwrite = overwrite)
  
  # reset
  user.params.list <- params.default
  sim.params.list[['procs']] <- sim.params.default[['procs']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed cluster 2 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

#------------------------------------------------------------------#
# Blocked 3 level: power
#------------------------------------------------------------------#

if(run.blocked.3l & run.power)
{
  scenarios <- 15
  user.params.list <- params.default
  
  # for sufficient power and stability
  user.params.list[['K']] <- 15
  user.params.list[['nbar']] <- 100

  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['nbar']] <- 100
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  # reset
  user.params.list[['nbar']] <- 100
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- params.default[['R2.3']]
  
  # vary R3.2
  user.params.list[['R2.3']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  
  # set all to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 8 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  user.params.list[['ATE_ES']] <- c(0.125, 0, 0)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  # reset
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  
  
  user.params.list[['ICC.3']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  # reset
  user.params.list[['ICC.3']] <- params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  user.params.list[['omega.3']] <- 0.8
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  user.params.list[['omega.2']] <- 0.8
  user.params.list[['omega.3']] <- params.default[['omega.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  user.params.list[['omega.2']] <- 0.8
  user.params.list[['omega.3']] <- 0.8
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  user.params.list[['omega.3']] <- params.default[['omega.3']]
  
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  user.params.list[['omega.3']] <- params.default[['omega.3']]
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 15 out of', scenarios))
  print('-----------------------------------------------------------------------------')
}


#------------------------------------------------------------------#
# Blocked 3 level: MDES and sample size
#------------------------------------------------------------------#

if(run.blocked.3l & run.mdes.ss)
{
  scenarios <- 1
  # back to defaults
  user.params.list <- params.default
  
  # for sufficient power and stability
  user.params.list[['K']] <- 15
  user.params.list[['nbar']] <- 100
  # don't do WY for now
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")
  
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_3r", overwrite = overwrite)
  sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_3r", overwrite = overwrite)
  
  # reset
  user.params.list <- params.default
  sim.params.list[['procs']] <- sim.params.default[['procs']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked 3 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

#------------------------------------------------------------------#
# Cluster 3 level: power
#------------------------------------------------------------------#

if(run.cluster.3l & run.power)
{
  
  scenarios <- 11
  user.params.list <- params.default
  
  # assumptions
  user.params.list[['omega.2']] <- 0
  user.params.list[['omega.3']] <- 0
  
  # param settings to have a decent level of power
  user.params.list[['J']] <- 40
  user.params.list[['K']] <- 20
  user.params.list[['ATE_ES']] <- rep(0.25, M)
  user.params.list[['ICC.3']] <- rep(0.1, M)
  user.params.list[['ICC.2']] <- rep(0.1, M)

  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['nbar']] <- 100
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  # reset
  user.params.list[['nbar']] <- params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  
  # vary R3.2
  user.params.list[['R2.3']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  
  # set all to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 8 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  user.params.list[['ATE_ES']] <- c(0.125, 0, 0)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- rep(0.25, M)
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  user.params.list[['ICC.2']] <- rep(0.1, M)
  
  user.params.list[['ICC.3']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  user.params.list[['ICC.3']] <- rep(0.1, M)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
}

#------------------------------------------------------------------#
# Cluster 3 level: MDES and sample size
#------------------------------------------------------------------#

if(run.cluster.3l & run.mdes.ss)
{
  scenarios <- 1
  # back to defaults
  user.params.list <- params.default
  # don't do WY for now
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")
  
  # assumptions
  user.params.list[['omega.2']] <- 0
  user.params.list[['omega.3']] <- 0
  
  # param settings to have a decent level of power
  user.params.list[['J']] <- 40
  user.params.list[['K']] <- 20
  user.params.list[['ATE_ES']] <- rep(0.25, M)
  user.params.list[['ICC.3']] <- rep(0.1, M)
  user.params.list[['ICC.2']] <- rep(0.1, M)
  
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "simple_c3_3r", overwrite = overwrite)
  sample.results <- validate_sample(user.params.list, sim.params.list, design = "simple_c3_3r", overwrite = overwrite)
  
  # reset
  user.params.list <- params.default
  sim.params.list[['procs']] <- sim.params.default[['procs']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed cluster 3 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

#------------------------------------------------------------------#
# Blocked cluster: power
#------------------------------------------------------------------#

if(run.blocked.cluster & run.power)
{
  scenarios <- 18
  user.params.list <- params.default
  
  # assumptions
  user.params.list[['omega.2']] <- 0
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['nbar']] <- 100
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 4 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  # reset
  user.params.list[['nbar']] <- params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['R2.1']] <- rep(0.6, M)
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)

  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  # reset
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  
  # vary R3.2
  user.params.list[['R2.3']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  
  # set all to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  user.params.list[['ATE_ES']] <- c(0.125, 0, 0)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.7, M)
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  
  user.params.list[['ICC.3']] <- rep(0.7, M)
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  user.params.list[['ICC.3']] <- params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 16 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  user.params.list[['omega.3']] <- 0.8
  user.params.list[['K']] <- 20
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  user.params.list[['omega.3']] <- params.default[['omega.3']]
  
  # vary both omega and ICC
  user.params.list[['omega.3']] <- 0.8
  user.params.list[['ICC.3']] <- rep(0.7, M)
  user.params.list[['K']] <- 20
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  user.params.list[['omega.3']] <- params.default[['omega.3']]
  user.params.list[['ICC.3']] <- params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 18 out of', scenarios))
  print('-----------------------------------------------------------------------------')
}

#------------------------------------------------------------------#
# Cluster 3 level: MDES and sample size
#------------------------------------------------------------------#

if(run.blocked.cluster & run.mdes.ss)
{
  scenarios <- 2
  # back to defaults
  user.params.list <- params.default
  # don't do WY for now
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")

  # assumptions
  user.params.list[['omega.2']] <- 0
  
  user.params.list[['R2.3']] <- rep(0, M)
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_c2_3f", overwrite = overwrite)
  sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_c2_3f", overwrite = overwrite)
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_c2_3r", overwrite = overwrite)
  sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_c2_3r", overwrite = overwrite)
  
  # reset
  user.params.list <- params.default
  sim.params.list[['procs']] <- sim.params.default[['procs']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked cluster scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
