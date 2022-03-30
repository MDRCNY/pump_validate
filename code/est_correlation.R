library(PUMP)
# source(here::here('code', 'estimate_power_with_simulation.R'))

# simulation parameters
n.sims <- 200
# d_m.list <- c('d2.1_m2fc')
d_m.list <- c('d2.1_m2fr', 'd2.2_m2rc', 'd3.1_m3rr2rr', 'd3.2_m3rr2rc')
rho.list <- c(0, 0.2, 0.5, 0.8)

Sys.setenv(TZ = 'America/New_york')
time.start <- Sys.time()
run <- format(time.start, format = '%Y%m%d_%H')
file.name = paste0(here::here(), '/cor_results_', run, '.rds')
print(file.name)



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

    cor.tstat <- check_cor(
      d_m = 'd2.1_m2fr', model.params.list, Tbar = Tbar, n.sims = n.sims
    )
    
    cor.data <- data.frame(
      d_m = d_m,
      input.rho = rho,
      n.sims = n.sims,
      output.rho = cor.tstat
    )
    out.data <- rbind(out.data, cor.data)
    
    saveRDS(out.data, file = file.name)
    print(out.data)
  }
}





