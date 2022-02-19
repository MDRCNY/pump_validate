library(PUMP)
source(here::here('code', 'estimate_power_with_simulation.R'))
# base.dir = '/Users/khunter/Dropbox/mordred/pump_validate/'
base.dir = '/n/home01/khunter33/pump_validate/'

# simulation parameters
n.sims <- 1000
d_m.list <- c('d2.1_m2fc')
#d_m.list <- c('d2.1_m2fr', 'd2.2_m2rc', 'd3.1_m3rr2rr', 'd3.2_m3rr2rc')
rho.list <- c(0, 0.5, 0.99)

Sys.setenv(TZ = 'America/New_york')
time.start <- Sys.time()
run <- format(time.start, format = '%Y%m%d_%H')


get_rawt <- function(d_m, model.params.list, Tbar, n.sims = 100)
{
  M <- model.params.list$M
  rawt.all <- matrix(NA, nrow = n.sims, ncol = M)
  dgp.params.list <- PUMP::convert_params(model.params.list)
  
  # number of simulations
  for(s in 1:n.sims)
  {
    if (s %% 100 == 0){ message(paste0("Now processing simulation ", s, " of ", n.sims)) }
    
    # generate simulated data
    samp.full <- PUMP::gen_full_data(dgp.params.list)
    S.id <- samp.full$ID$S.id
    D.id  <- samp.full$ID$D.id
    
    # generate treatment assignments
    T.x <- PUMP::gen_T.x(d_m = d_m,
                         S.id = S.id, D.id = D.id,
                         nbar = dgp.params.list$nbar,
                         Tbar = 0.5)
    
    # convert full data to observed data
    samp.obs <- samp.full
    samp.obs$Yobs <- PUMP::gen_Yobs(samp.full, T.x)
    
    # calculate t statistics
    dat.all <- makelist_samp(samp.obs, T.x)
    rawpt.out <- get_rawpt(dat.all, d_m = d_m, model.params.list = model.params.list)
    rawt <- sapply(rawpt.out[['rawpt']], function(s){ return(s[['tstat']])})
    rawt.all[s,] <- rawt
  }
  
  return(rawt.all)
}

get_cor <- function(rawt.all)
{
  
  # calculate correlation
  cor.tstat <- cor(rawt.all)
  est.cor <- cor.tstat[lower.tri(cor.tstat)]
  
  return(est.cor)
}


model.params.list <- list(
  M = 3                             # number of outcomes
  , J = 30                         # number of schools
  , K = 10                          # number of districts
  , nbar = 50                       # number of individuals per school
  , rho.default = 0.5               # default rho value (optional)
  ################################################## impact
  , MDES = 0.125                    # minimum detectable effect size      
  ################################################## level 3: districts
  , numCovar.3 = 0                  # number of district covariates
  , R2.3 = 0                        # percent of district variation
  # explained by district covariates
  , ICC.3 = 0.2                     # district intraclass correlation
  , omega.3 = 0                     # ratio of district effect size variability
  # to random effects variability
  ################################################## level 2: schools
  , numCovar.2 = 0                  # number of school covariates
  , R2.2 = 0                        # percent of school variation
  # explained by school covariates
  , ICC.2 = 0.2                     # school intraclass correlation	
  , omega.2 = 0                     # ratio of school effect size variability
  # to random effects variability
  ################################################## level 1: individuals
  , numCovar.1 = 0                  # number of individual covariates
  , R2.1 = 0                        # percent of indiv variation explained
  # by indiv covariates
)
Tbar <- 0.5

# store results
out.data <- NULL

for(d_m in d_m.list)
{
  print(paste('Design:', d_m))
  
  for(rho in rho.list)
  {
    print(paste('Rho:', rho))
    
    model.params.list$rho.default <- rho
    
    rawt.all <- get_rawt(
      d_m = d_m,
      model.params.list = model.params.list,
      Tbar = Tbar, 
      n.sims = n.sims
    )
    est.cor <- matrix(get_cor(rawt.all), ncol = M, nrow = 1)
    
    cor.data <- data.frame(
      d_m = d_m,
      input.rho = rho,
      n.sims = n.sims,
      output.rho = est.cor
    )
    out.data <- rbind(out.data, cor.data)
    
    saveRDS(out.data, file = paste0(base.dir, 'cor_results_', run, '.rds'))
    print(out.data)
  }
}





