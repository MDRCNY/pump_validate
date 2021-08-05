#-----------------------------------------------------------------#
# script that produces all output results
# all designs, all cases of interest
#------------------------------------------------------------------#

library(here)

# overwrite existing results that have already been saved?
overwrite = TRUE
# whether or not to run power, mdes and sample size
run.power = FALSE
run.mdes.ss = TRUE
run.wy = FALSE
# which designs to run
run.d2.1 = TRUE
run.d2.2 = TRUE
run.d3.1 = TRUE
run.d3.3 = TRUE
run.d3.2 = TRUE
run.power.def = FALSE
run.extremes = TRUE

# simulation and user parameters
source(here::here("Validation/Simulations", "params.R"))

q <- as.numeric(as.character(Sys.getenv("q")))
if(is.na(q)) { q <- 1 }

user.params.default <- user.params.list
sim.params.default <- sim.params.list


#------------------------------------------------------------------#
# Test WY
#------------------------------------------------------------------#
if(run.wy)
{
  sim.params.list <- sim.params.default
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm", "WY-SS")
  # sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm", "WY-SS", "WY-SD")
  
  user.params.list <- user.params.default
  
  #------------------------------------#
  # blocked 2 level
  #------------------------------------#
  if(run.d2.1)
  {
    user.params.list <- user.params.default

    user.params.list[['J']] <- 60    

    user.params.list[['K']] <- 1
    user.params.list[['ICC.3']] <- NULL
    user.params.list[['omega.3']] <- NULL
    user.params.list[['R2.3']] <- NULL

    # so power isn't too high
    user.params.list[['ATE_ES']] <- rep(0.05, M)
    user.params.list[['nbar']] <- 30

    user.params.list[['omega.2']] <- 0
    # power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
    user.params.list[['omega.2']] <- user.params.default[['omega.2']]
    # power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
    power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
    
    # # try other combinations of params
    # user.params.list[['J']] <- 60
    # sim.params.list[['B']] <- 200
    # power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
    # 
    # user.params.list[['J']] <- 15
    # sim.params.list[['B']] <- 3000
    # power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
 
    sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm", "WY-SS")    

    gc()
  }

  #------------------------------------#
  # cluster 2 level
  #------------------------------------#
  
  if(run.d2.2)
  {
    user.params.list <- user.params.default
    
    user.params.list[['K']] <- 1
    user.params.list[['ICC.3']] <- NULL
    user.params.list[['omega.3']] <- NULL
    user.params.list[['R2.3']] <- NULL
    user.params.list[['omega.2']] <- 0
    
    # params to help have a decent power
    user.params.list[['ICC.2']] <- rep(0.1, M)
    user.params.list[['J']] <- 60
    user.params.list[['ATE_ES']] <- rep(0.25, M)
    
    power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
    gc()
  }

  #------------------------------------#
  # blocked 3 level
  #------------------------------------#
  
  if(run.d3.1)
  {
    user.params.list <- user.params.default
    
    # for a reasonable runtime and power
    user.params.list[['nbar']] <- 20
    user.params.list[['J']] <- 20
    user.params.list[['K']] <- 20
    
    power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
    gc()
  }
  
  #------------------------------------#
  # cluster 3 level
  #------------------------------------#
  
  if(run.d3.3)
  {
    user.params.list <- user.params.default
    
    # assumptions
    user.params.list[['omega.2']] <- 0
    user.params.list[['omega.3']] <- 0
    
    # for a reasonable power
    user.params.list[['J']] <- 40
    user.params.list[['K']] <- 20
    user.params.list[['ATE_ES']] <- rep(0.25, M)
    user.params.list[['ICC.3']] <- rep(0.1, M)
    user.params.list[['ICC.2']] <- rep(0.1, M)
    
    power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
    gc()
  }

  #------------------------------------#
  # blocked cluster
  #------------------------------------#
  
  if(run.d3.2)
  {
    user.params.list <- user.params.default
    
    # assumptions
    user.params.list[['omega.2']] <- 0
    
    user.params.list[['R2.3']] <- rep(0, M)
    user.params.list[['ICC.3']] <- rep(0, M)
    power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
    
    # for reasonable power and runtime
    user.params.list[['nbar']] <- 30
    user.params.list[['J']] <- 15
    user.params.list[['K']] <- 15
    user.params.list[['ATE_ES']] <- rep(0.25, M)
    
    user.params.list[['R2.3']] <- user.params.default[['R2.3']]
    user.params.list[['ICC.3']] <- user.params.default[['ICC.3']]
    power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
    gc()
  }
  # reset
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")
}

#------------------------------------------------------------------#
# Blocked 2 level: power
#------------------------------------------------------------------#

if(run.d2.1 & run.power)
{
  scenarios <- 20
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
  # assumptions
  user.params.list[['K']] <- 1
  user.params.list[['ICC.3']] <- NULL
  user.params.list[['omega.3']] <- NULL
  user.params.list[['R2.3']] <- NULL
  
  #------------------------------------------------------------------#
  # unblocked design
  #------------------------------------------------------------------#
  user.params.list[['J']] <- 1
  power.results <- validate_power(user.params.list, sim.params.list, design = "d1.1_m2fc", q = q, overwrite)
  user.params.list[['J']] <- user.params.default[['J']]
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['nbar']] <- 100
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
  
  # reset
  user.params.list[['nbar']] <- user.params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['omega.2']] <- 0
  
  user.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- user.params.default[['R2.2']]
  
  # set both to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- user.params.default[['R2.2']]
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.default']] <- rho.default
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.default']] <- rho.default
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  
  # reset
  rho.default <- user.params.default[['rho.default']]
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
  
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- user.params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.7, M)
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
  
  user.params.list[['ICC.2']] <- rep(0, M)
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
  
  # reset
  user.params.list[['ICC.2']] <- user.params.default[['ICC.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 18 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  user.params.list[['omega.2']] <- 0.8
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 20 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- kappa.matrix
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fc", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2ff", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- user.params.default[['kappa.w']]
  
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
}

#------------------------------------------------------------------#
# Blocked 2 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d2.1 & run.mdes.ss)
{
  scenarios <- 9
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
  # assumptions
  user.params.list[['K']] <- 1
  user.params.list[['ICC.3']] <- NULL
  user.params.list[['omega.3']] <- NULL
  user.params.list[['R2.3']] <- NULL
  
  # don't do WY for now
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")
  
  user.params.list[['omega.2']] <- 0
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d2.1_m2fc", 
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d2.1_m2fc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d2.1_m2fc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed constant, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d2.1_m2ff", 
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d2.1_m2ff",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d2.1_m2ff",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed fixed, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d2.1_m2fr", 
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d2.1_m2fr",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d2.1_m2fr",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed random, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  # reset
  user.params.list <- user.params.default
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

if(run.d2.2 & run.power)
{
  scenarios <- 10
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default

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
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  
  # reset
  user.params.list[['nbar']] <- user.params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- user.params.default[['R2.2']]
  
  # set both to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  user.params.list[['R2.2']] <- user.params.default[['R2.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed R2 scenarios, 5 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary rho
  #------------------------------------------------------------------#
  
  rho.default <- 0
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.default']] <- rho.default
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  
  rho.default <- 0.2
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.default']] <- rho.default
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  
  # reset
  rho.default <- user.params.default[['rho.default']]
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
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- user.params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  
  user.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  user.params.list[['ICC.2']] <- user.params.default[['ICC.2']]
  
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 9 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- kappa.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "d2.2_m2rc", q = q, overwrite)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- user.params.default[['kappa.w']]
  
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
}

#------------------------------------------------------------------#
# Cluster 2 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d2.2 & run.mdes.ss)
{
  scenarios <- 1
  # back to defaults
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
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
  
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d2.2_m2rc", overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d2.2_m2rc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d2.2_m2rc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  # reset
  user.params.list <- user.params.default
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

if(run.d3.1 & run.power)
{
  scenarios <- 15
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
  # for sufficient power and stability
  user.params.list[['K']] <- 15
  user.params.list[['nbar']] <- 100

  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['nbar']] <- 100
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
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
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- user.params.default[['R2.3']]
  
  # vary R3.2
  user.params.list[['R2.3']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  
  # set all to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  user.params.list[['R2.2']] <- user.params.default[['R2.2']]
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  
  
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
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
  # reset
  rho.default <- user.params.default[['rho.default']]
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
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- user.params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  user.params.list[['ICC.2']] <- user.params.default[['ICC.2']]
  
  
  user.params.list[['ICC.3']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  user.params.list[['ICC.3']] <- user.params.default[['ICC.3']]
  
  
  user.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  user.params.list[['ICC.2']] <- user.params.default[['ICC.2']]
  
  user.params.list[['ICC.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  # reset
  user.params.list[['ICC.3']] <- user.params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  user.params.list[['omega.3']] <- 0.8
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
  user.params.list[['omega.2']] <- 0.8
  user.params.list[['omega.3']] <- user.params.default[['omega.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
  user.params.list[['omega.2']] <- 0.8
  user.params.list[['omega.3']] <- 0.8
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  user.params.list[['omega.3']] <- user.params.default[['omega.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
  user.params.list[['omega.3']] <- user.params.default[['omega.3']]
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  
  user.params.list[['omega.2']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  
  user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  user.params.list[['omega.3']] <- user.params.default[['omega.3']]
  
  user.params.list[['omega.2']] <- 0
  user.params.list[['omega.3']] <- 0
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  
  user.params.list[['omega.3']] <- user.params.default[['omega.3']]
  user.params.list[['omega.2']] <- user.params.default[['omega.2']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 15 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- kappa.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.1_m3rr2rr", q = q, overwrite)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- user.params.default[['kappa.w']]
  
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
}


#------------------------------------------------------------------#
# Blocked 3 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d3.1 & run.mdes.ss)
{
  scenarios <- 1
  # back to defaults
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")
  
  # for sufficient power and stability
  user.params.list[['K']] <- 15
  user.params.list[['nbar']] <- 100
  
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.1_m3rr2rr", overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.1_m3rr2rr",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.1_m3rr2rr",
    typesample = 'K',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.1_m3rr2rr",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  # reset
  user.params.list <- user.params.default
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

if(run.d3.3 & run.power)
{
  
  scenarios <- 11
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
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
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 1 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  
  # reset
  user.params.list[['nbar']] <- user.params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 3 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['R2.1']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.2']] <- user.params.default[['R2.2']]
  
  # vary R3.2
  user.params.list[['R2.3']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  
  # set all to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  user.params.list[['R2.2']] <- user.params.default[['R2.2']]
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  
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
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  
  # reset
  rho.default <- user.params.default[['rho.default']]
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
  
  user.params.list[['ATE_ES']] <- c(0.25, 0, 0)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- rep(0.25, M)
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  user.params.list[['ICC.2']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  user.params.list[['ICC.2']] <- rep(0.1, M)
  
  user.params.list[['ICC.3']] <- rep(0.7, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  user.params.list[['ICC.3']] <- rep(0.1, M)
  
  user.params.list[['ICC.2']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  user.params.list[['ICC.2']] <- rep(0.1, M)
  
  user.params.list[['ICC.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  user.params.list[['ICC.3']] <- rep(0.1, M)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 11 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- kappa.matrix
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.3_m3rc2rc", q = q, overwrite)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- user.params.default[['kappa.w']]
  
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
}

#------------------------------------------------------------------#
# Cluster 3 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d3.3 & run.mdes.ss)
{
  scenarios <- 1
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
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
  
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'K',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  # reset
  user.params.list <- user.params.default
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

if(run.d3.2 & run.power)
{
  scenarios <- 18
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
  # assumptions
  user.params.list[['omega.2']] <- 0
  
  #------------------------------------------------------------------#
  # vary sample size
  #------------------------------------------------------------------#
  user.params.list[['nbar']] <- 100
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 2 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 75
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed 4 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  user.params.list[['nbar']] <- 50
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  # reset
  user.params.list[['nbar']] <- user.params.default[['nbar']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed sample size scenarios, 6 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  
  #------------------------------------------------------------------#
  # Vary R2
  #------------------------------------------------------------------#
  
  # vary R2.1
  user.params.list[['R2.1']] <- rep(0.6, M)
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  
  # vary R2.2
  user.params.list[['R2.2']] <- rep(0.6, M)

  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  # reset
  user.params.list[['R2.2']] <- user.params.default[['R2.2']]
  
  # vary R3.2
  user.params.list[['R2.3']] <- rep(0.6, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  
  # set all to 0
  user.params.list[['R2.1']] <- rep(0, M)
  user.params.list[['R2.2']] <- rep(0, M)
  user.params.list[['R2.3']] <- rep(0, M)
  
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  # reset
  user.params.list[['R2.1']] <- user.params.default[['R2.1']]
  user.params.list[['R2.2']] <- user.params.default[['R2.2']]
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  
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
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  
  rho.default <- 0.8
  user.params.list[['rho.default']] <- rho.default
  default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)
  user.params.list[['rho.V']] <- user.params.list[['rho.X']] <- user.params.list[['rho.C']] <- default.rho.matrix
  user.params.list[['rho.u0']] <- user.params.list[['rho.u1']] <- default.rho.matrix
  user.params.list[['rho.w0']] <- user.params.list[['rho.w1']] <- default.rho.matrix
  user.params.list[['rho.r']] <- default.rho.matrix
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  
  # reset
  rho.default <- user.params.default[['rho.default']]
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
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  
  # reset
  user.params.list[['ATE_ES']] <- user.params.default[['ATE_ES']]
  
  #------------------------------------------------------------------#
  # Vary ICC
  #------------------------------------------------------------------#
  
  # ICC 2
  user.params.list[['ICC.2']] <- rep(0.7, M)
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  user.params.list[['ICC.2']] <- user.params.default[['ICC.2']]
  
  # ICC 3
  user.params.list[['ICC.3']] <- rep(0.7, M)
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  # ICC 2 = 0
  user.params.list[['ICC.2']] <- rep(0, M)
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  user.params.list[['ICC.2']] <- user.params.default[['ICC.2']]
  
  # ICC 3 = 0
  user.params.list[['ICC.3']] <- rep(0, M)
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  user.params.list[['ICC.3']] <- user.params.default[['ICC.3']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed true positives and ICC, 16 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary Omega
  #------------------------------------------------------------------#
  
  user.params.list[['omega.3']] <- 0.8
  user.params.list[['K']] <- 20
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  user.params.list[['omega.3']] <- user.params.default[['omega.3']]
  
  
  user.params.list[['omega.3']] <- 0
  user.params.list[['K']] <- user.params.default[['K']]
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  user.params.list[['omega.3']] <- user.params.default[['omega.3']]
  
  # vary both omega and ICC
  user.params.list[['omega.3']] <- 0.8
  user.params.list[['ICC.3']] <- rep(0.7, M)
  user.params.list[['K']] <- 20
  
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  
  user.params.list[['omega.3']] <- user.params.default[['omega.3']]
  user.params.list[['ICC.3']] <- user.params.default[['ICC.3']]
  user.params.list[['K']] <- user.params.default[['K']]
  
  print('-----------------------------------------------------------------------------')
  print(paste('Completed omega, 18 out of', scenarios))
  print('-----------------------------------------------------------------------------')
  
  #------------------------------------------------------------------#
  # Vary kappa
  #------------------------------------------------------------------#
  kappa <- 0.4
  kappa.matrix <- diag(kappa, M)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- kappa.matrix
  user.params.list[['R2.3']] <- rep(0, M)
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3rr2rc", q = q, overwrite)
  user.params.list[['kappa.w']] <- user.params.list[['kappa.u']] <- user.params.default[['kappa.w']]
  
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
}

#------------------------------------------------------------------#
# Cluster 3 level: MDES and sample size
#------------------------------------------------------------------#

if(run.d3.2 & run.mdes.ss)
{
  scenarios <- 2
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")
  
  # assumptions
  user.params.list[['omega.2']] <- 0
  
  user.params.list[['R2.3']] <- rep(0, M)
  user.params.list[['ICC.3']] <- rep(0, M)
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.2_m3ff2rc",
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.2_m3ff2rc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.2_m3ff2rc",
    typesample = 'K',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.2_m3ff2rc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  user.params.list[['R2.3']] <- user.params.default[['R2.3']]
  user.params.list[['ICC.3']] <- user.params.default[['ICC.3']]
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.2_m3rr2rc",
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.2_m3rr2rc",
    typesample = 'J',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.2_m3rr2rc",
    typesample = 'K',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.2_m3rr2rc",
    typesample = 'nbar',
    overwrite = overwrite
  )
  
  # reset
  user.params.list <- user.params.default
  sim.params.list[['procs']] <- sim.params.default[['procs']]
}

print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')
print(paste('Completed blocked cluster scenarios'))
print('---------------------------------------------------------------------------------')
print('---------------------------------------------------------------------------------')


#------------------------------------------------------------------#
# Power definitions: power
#------------------------------------------------------------------#

if(run.power.def & run.power)
{
  scenarios <- 18
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
  
  # assumptions
  user.params.list[['omega.2']] <- 0
  user.params.list[['R2.3']] <- rep(0, M)
  
  #------------------------------------------------------------------#
  # raw individual power
  #------------------------------------------------------------------#
  user.params.list$ATE_ES <- rep(0.1, 3)
  user.params.list$J <- 3
  user.params.list$K <- 22
  user.params.list$nbar <- 150
  user.params.list$R2.1 <- 0.1
  user.params.list$R2.2 <- 0.7
  user.params.list$ICC.2 <- 0.05
  user.params.list$ICC.3 <- 0.4
  user.params.list$rho <- 0.4
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  
  #------------------------------------------------------------------#
  # Bonferroni individual power
  #------------------------------------------------------------------#
  user.params.list$ATE_ES <- rep(0.1, 3)
  user.params.list$J <- 4
  user.params.list$K <- 17
  user.params.list$nbar <- 160
  user.params.list$R2.1 <- 0.3
  user.params.list$R2.2 <- 0.75
  user.params.list$ICC.2 <- 0.05
  user.params.list$ICC.3 <- 0.4
  user.params.list$rho <- 0.4
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  
  #------------------------------------------------------------------#
  # Bonferroni min1 power
  #------------------------------------------------------------------#
  user.params.list$ATE_ES <- rep(0.1, 3)
  user.params.list$J <- 4
  user.params.list$K <- 16
  user.params.list$nbar <- 150
  user.params.list$R2.1 <- 0.25
  user.params.list$R2.2 <- 0.75
  user.params.list$ICC.2 <- 0.05
  user.params.list$ICC.3 <- 0.4
  user.params.list$rho <- 0.4
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
  
  #------------------------------------------------------------------#
  # Bonferroni complete power
  #------------------------------------------------------------------#
  user.params.list$ATE_ES <- rep(0.1, 3)
  user.params.list$J <- 5
  user.params.list$K <- 18
  user.params.list$nbar <- 170
  user.params.list$R2.1 <- 0.25
  user.params.list$R2.2 <- 0.75
  user.params.list$ICC.2 <- 0.05
  user.params.list$ICC.3 <- 0.4
  user.params.list$rho <- 0.4
  power.results <- validate_power(user.params.list, sim.params.list, design = "d3.2_m3ff2rc", q = q, overwrite)
}


#------------------------------------------------------------------#
# Power definitions: MDES and SS
#------------------------------------------------------------------#

if(run.power.def & run.mdes.ss)
{
  scenarios <- 1
  # back to defaults
  user.params.list <- user.params.default
  sim.params.list <- sim.params.default
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
  

  # MDES power defs
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'D1indiv',
    overwrite = overwrite
  )
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'min1',
    overwrite = overwrite
  )
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'min2',
    overwrite = overwrite
  )
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  # SS type J power defs
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'D1indiv',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'min1',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'min2',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  # SS type K power defs
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'K',
    power.definition = 'D1indiv',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'K',
    power.definition = 'min1',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'K',
    power.definition = 'min2',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'K',
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  # SS type nbar power defs
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'nbar',
    power.definition = 'D1indiv',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'nbar',
    power.definition = 'D1indiv',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'nbar',
    power.definition = 'min1',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'nbar',
    power.definition = 'min2',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'nbar',
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  # change correlation
  user.params.list[['rho']] <- 0
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'min1',
    overwrite = overwrite
  )
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'min1',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  # change correlation
  user.params.list[['rho']] <- 0.8
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'min1',
    overwrite = overwrite
  )
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'min1',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  # change correlation
  user.params.list[['rho']] <- 1
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'min1',
    overwrite = overwrite
  )
  mdes.results <- validate_mdes(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'min1',
    overwrite = overwrite
  )
  sample.results <- validate_sample(
    user.params.list, sim.params.list,
    design = "d3.3_m3rc2rc",
    typesample = 'J',
    power.definition = 'complete',
    overwrite = overwrite
  )
  
  # reset
  user.params.list <- user.params.default
  sim.params.list[['procs']] <- sim.params.default[['procs']]
}