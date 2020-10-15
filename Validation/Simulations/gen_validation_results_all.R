#------------------------------------------------------------------#
# script that produces output results to be put into an r markdown file
#------------------------------------------------------------------#

library(here)

# overwrite existing results that have already been saved?
overwrite = TRUE
# if TRUE, only run one power calculation, otherwise run all scenarios of interest
run.test = TRUE

# simulation and user parameters
source(here::here("Validation/Simulations", "params.R"))

power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)

if(!run.test)
{
  scenarios = 24
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
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
  print(paste('Completed sample size scenarios, 9 out of', scenarios))
  print('--------------------------------------------------------')

  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2
  user.params.list[['omega.2']] = 0
  user.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)

  print('--------------------------------------------------------')
  print(paste('Completed R2 scenarios, 11 out of', scenarios))
  print('--------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#

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
  print(paste('Completed rho scenarios, 13 out of', scenarios))
  print('--------------------------------------------------------')

  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  user.params.list[['ATE_ES']] = c(0.125, 0, 0)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", overwrite)

  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  user.params.list[['ATE_ES']] = c(0.125, 0.125, 0.125)
  user.params.list[['ICC.2']] = rep(0.5, M)
  user.params.list[['omega.2']] = 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)

  print('--------------------------------------------------------')
  print(paste('Completed true positives and ICC, 15 out of', scenarios))
  print('--------------------------------------------------------')

  #------------------------------------------------------------------#
  # Verify MDES and Sample size
  #------------------------------------------------------------------#

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
  print(paste('Completed mdes and sample size for constant, 18 out of', scenarios))
  print('--------------------------------------------------------')

  user.params.list[['omega.2']] = 0.5
  power.results.2f <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", overwrite)
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_2f", overwrite)
  sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_2f", overwrite)

  print('--------------------------------------------------------')
  print(paste('Completed mdes and sample size for fixed, 21 out of', scenarios))
  print('--------------------------------------------------------')

  user.params.list[['omega.2']] = 0.5
  power.results.2r <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)
  sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_2r", overwrite)

  print('--------------------------------------------------------')
  print(paste('Completed mdes and sample size for random, 24 out of', scenarios))
  print('--------------------------------------------------------')
}

