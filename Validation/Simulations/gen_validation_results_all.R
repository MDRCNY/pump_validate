#------------------------------------------------------------------#
# script that produces output results to be put into an r markdown file
#------------------------------------------------------------------#

library(here)

# overwrite existing results that have already been saved?
overwrite = TRUE
# if TRUE, only run one power calculation, otherwise run all scenarios of interest
run.test = FALSE
# whether or not to run power, mdes and sample size
run.power = FALSE
run.mdes.ss = TRUE
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

if(run.test)
{
  user.params.list[['K']] <- 1
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  if(run.mdes.ss)
  {
    mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
    sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  }
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  user.params.list[['omega.2']] <- 0.5
  user.params.list[['omega.3']] <- 0.5
  user.params.list[['K']] <- 4
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  user.params.list[['omega.2']] <- 0
  user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
}

if(FALSE)
{
  design <- "blocked_i1_2r"
  results_plot <- ggplot(power.results, aes(x = MTP, y = value, color = method)) +
    geom_point() +
    geom_line() +
    facet_wrap(~power_type, labeller = label_both) +
    ylab('Power') +
    ggtitle(paste('Design:', design = design))
  print(results_plot)
}


if(!run.test & run.blocked.2l & run.power)
{
  user.params.list[['K']] <- 1
  
  scenarios = 16
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['n.j']] <- 100
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  user.params.list[['omega.2']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 75
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  user.params.list[['omega.2']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 50
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  user.params.list[['omega.2']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  
  # reset
  user.params.list[['n.j']] <- params.default[['n.j']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['omega.2']] <- 0
  user.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  
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
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
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
  
  user.params.list[['ICC.2']] <- rep(0.8, M)
  user.params.list[['omega.2']] <- 0.5
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
# Verify MDES and Sample size
#------------------------------------------------------------------#

if(!run.test & run.blocked.2l & run.mdes.ss)
{
  scenarios = 3
  # back to defaults
  user.params.list <- params.default
  # don't do WY for now
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")
  
  user.params.list[['omega.2']] <- 0
  power.results.2c <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  #sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed mdes and sample size for constant, 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['omega.2']] <- 0.5
  power.results.2f <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
  #sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed mdes and sample size for fixed, 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['omega.2']] <- 0.5
  power.results.2r <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  mdes.results <- validate_mdes(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  #sample.results <- validate_sample(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed mdes and sample size for random, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  # reset
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  sim.params.list[['procs']] <- params.default[['procs']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked 2 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

if(!run.test & run.cluster.2l & run.power)
{
  user.params.list <- params.default
  user.params.list[['omega.2']] <- 0
  user.params.list[['K']] <- 1
  scenarios = 9
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['n.j']] <- 100
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 75
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 50
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  # reset
  user.params.list[['n.j']] <- params.default[['n.j']]
  
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
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
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
  
  user.params.list[['ICC.2']] <- rep(0.8, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed cluster 2 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')


if(!run.test & run.blocked.3l & run.power)
{
  user.params.list <- params.default
  user.params.list[['omega.2']] <- 0.5
  user.params.list[['omega.3']] <- 0.5
  user.params.list[['K']] <- 4
  scenarios = ifelse(run.mdes.ss, 24, 14)

  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['n.j']] <- 100
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 75
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 50
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  # reset
  user.params.list[['n.j']] <- params.default[['n.j']]
  
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
  
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.default']] <- rho.default
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
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
  
  user.params.list[['ICC.2']] <- rep(0.8, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  # reset
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  
  
  user.params.list[['ICC.3']] <- rep(0.8, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  # reset
  user.params.list[['ICC.3']] <- params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  user.params.list[['omega.2']] <- 0.8
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  
  user.params.list[['omega.3']] <- 0.8
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  user.params.list[['omega.3']] <- params.default[['omega.3']]
  
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.8
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_3r", q = q, overwrite)
  user.params.list[['omega.3']] <- params.default[['omega.3']]
  user.params.list[['omega.2']] <- params.default[['omega.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 14 out of', scenarios))
  print('-----------------------------------------------------------------------------')
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked 3 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')

if(!run.test & run.cluster.3l & run.power)
{
  user.params.list <- params.default
  user.params.list[['omega.2']] <- 0
  user.params.list[['omega.3']] <- 0
  user.params.list[['K']] <- 4
  scenarios = ifelse(run.mdes.ss, 24, 11)

  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['n.j']] <- 100
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 75
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 50
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  # reset
  user.params.list[['n.j']] <- params.default[['n.j']]
  
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
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.default']] <- rho.default
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 8 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  user.params.list[['ATE_ES']] <- c(0.125, 0, 0)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.8, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  
  user.params.list[['ICC.3']] <- rep(0.8, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c3_3r", q = q, overwrite)
  user.params.list[['ICC.3']] <- params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed cluster 3 level scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')



if(!run.test & run.blocked.cluster & run.power)
{
  user.params.list <- params.default
  user.params.list[['K']] <- 4
  scenarios <- ifelse(run.mdes.ss, 24, 22)
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['n.j']] <- 100
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 75
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 4 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['n.j']] <- 50
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  # reset
  user.params.list[['n.j']] <- params.default[['n.j']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['R2.1']] <- rep(0.6, M)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- params.default[['R2.2']]
  
  # vary R3.2
  user.params.list[['R2.3']] <- rep(0.6, M)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  # reset
  user.params.list[['R2.3']] <- params.default[['R2.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 12 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.default']] <- rho.default
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  # reset
  rho.default <- params.default[['rho.default']]
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u']] <- user.params.list[['rho.v']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed rho scenarios, 16 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary true positives
  #------------------------------------------------------------------#
  
  user.params.list[['ATE_ES']] <- c(0.125, 0, 0)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.8, M)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  user.params.list[['ICC.2']] <- params.default[['ICC.2']]
  
  user.params.list[['ICC.3']] <- rep(0.8, M)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.list[['omega.3']] <- 0.5
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3r", q = q, overwrite)
  user.params.list[['ICC.3']] <- params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 22 out of', scenarios))
  print('-----------------------------------------------------------------------------')
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked cluster scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
