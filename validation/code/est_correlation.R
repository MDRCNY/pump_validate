get_rho <- function(d_m, model.params.list, Tbar, S = 100)
{
  M <- model.params.list$M
  rawt.all <- matrix(NA, nrow = S, ncol = M)
  dgp.params.list <- PUMP::convert_params(model.params.list)
  
  for(s in 1:S)
  {
    if (s %% 10 == 0){ message(paste0("Now processing simulation ", s, " of ", S)) }
    
    samp.full <- PUMP::gen_full_data(dgp.params.list)
    S.id <- samp.full$ID$S.id
    D.id  <- samp.full$ID$D.id
    
    T.x <- PUMP::gen_T.x(d_m = d_m,
                         S.id = S.id, D.id = D.id,
                         nbar = dgp.params.list$nbar,
                         Tbar = 0.5)
    
    
    # convert full data to observed data
    samp.obs <- samp.full
    samp.obs$Yobs <- PUMP::gen_Yobs(samp.full, T.x)
    
    # calculate t statistics
    dat.all <- makelist_samp(samp.obs, T.x) # list length M
    rawpt.out <- get_rawpt(dat.all, d_m = d_m, model.params.list = model.params.list)
    rawt <- sapply(rawpt.out[['rawpt']], function(s){ return(s[['tstat']])})
    rawt.all[s,] <- rawt
  }
  
  # calculate correlation
  cor.tstat<- cor(rawt.all)

  return(cor.tstat)
}

d_m <- 'd3.3_m3rc2rc'
model.params.list <- list(
  M = 3                             # number of outcomes
  , J = 30                          # number of schools
  , K = 10                          # number of districts
  # (for two-level model, set K = 1)
  , nbar = 50                       # number of individuals per school
  , rho.default = 0.5               # default rho value (optional)
  ################################################## impact
  , MDES = 0.125                    # minimum detectable effect size      
  ################################################## level 3: districts
  , numCovar.3 = 1                  # number of district covariates
  , R2.3 = 0.1                      # percent of district variation
  # explained by district covariates
  , ICC.3 = 0.2                     # district intraclass correlation
  , omega.3 = 0.1                   # ratio of district effect size variability
  # to random effects variability
  ################################################## level 2: schools
  , numCovar.2 = 1                  # number of school covariates
  , R2.2 = 0.1                      # percent of school variation
  # explained by school covariates
  , ICC.2 = 0.2                     # school intraclass correlation	
  , omega.2 = 0.1                   # ratio of school effect size variability
  # to random effects variability
  ################################################## level 1: individuals
  , numCovar.1 = 1                  # number of individual covariates
  , R2.1 = 0.1                      # percent of indiv variation explained
  # by indiv covariates
)
Tbar <- 0.5
S <- 10

cor.tstat <- get_rho(d_m, model.params.list, Tbar, S)