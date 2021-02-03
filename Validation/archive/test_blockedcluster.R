# runs simulations

library(here)
library(plyr)
source(here::here("Validation/Simulations", "params.R"))

design = 'blocked_c2_3f'

user.params.list[['J']] <- 20
user.params.list[['K']] <- 10
user.params.list[['nbar']] <- 50
user.params.list[['omega.2']] <- 0
user.params.list[['omega.3']] <- 0.8
user.params.list[['R2.2']] <- rep(0.6, 3)

user.params.list[['ICC.3']] <- rep(0.1, 3)
Tbar <- sim.params.list[['Tbar']]

calc.bias = function(omega3, ICC.3, S)
{
  # omega3 = 0.1; ICC.3 = rep(0.1, 3); S = 10
  
  # user.params.list[['omega.3']] <- omega3
  # print(user.params.list[['omega.3']])
  # user.params.list[['ICC.3']] <- ICC.3
  # print(user.params.list[['ICC.3']])
  
  ate.all = matrix(NA, ncol = M, nrow = S)
  psi.all = matrix(NA, ncol = M, nrow = S)
  delta.all = matrix(NA, ncol = M, nrow = S)
  
  model.params.list <- convert.params(user.params.list)
  
  for(s in 1:S)
  {
    print(s)
    # s = 1;
    set.seed(s)
    samp.full <- gen_full_data(model.params.list)
    
    psi.all[s,] = apply(samp.full$psi1.ijk, 2, mean)
    delta.all[s,] = samp.full$delta
    
    S.id <- samp.full$ID$S.id
    D.id  <- samp.full$ID$D.id
    
    T.x <- randomizr::block_and_cluster_ra( blocks = D.id, clusters = S.id, prob = Tbar )
    
    # convert full data to observed data
    samp.obs <- samp.full
    samp.obs$Yobs <- gen_Yobs(samp.full, T.x)
    mdat <- makelist.samp(samp.obs, T.x)
    
    treat.df = data.frame(D.id = D.id, psi = samp.full$psi1.ijk)
    treat.summary = ddply(treat.df, 'D.id', summarise,
                          psi.1 = unique(psi.1), psi.2 = unique(psi.1), psi.3 = unique(psi.1))
    
    var(treat.summary[,2])
    
    for(m in 1:M)
    {
      # m = 1;
      dat <- mdat[[m]]
      dat$S.id <- as.factor(dat$S.id)
      if(!is.null(dat$D.id)){ dat$D.id <- as.factor(dat$D.id) }
      
      formula = as.formula("Yobs ~ 0 + T.x * D.id - T.x + X.jk + C.ijk + (1 | S.id)")
      library(lmerTest)
      mod <- lmer(formula, data = dat)
      
      compare = cbind(treat.summary[,2], summary(mod)$coefficients[13:22,1])

      
      mod <- interacted_linear_estimators(Yobs = Yobs, Z = T.x, B = D.id, data = dat, control_formula = "X.jk + C.ijk + (1 | S.id)", lmer = TRUE)
      ate.all[s,m] <- mod[1,2]
    }
  }
  bias = apply(ate.all - psi.all, 2, mean)
  return(bias)
}

bias.omega301.ICC301 = calc.bias(omega3 = 0.1, ICC.3 = rep(0.1, 3), S = 10)
bias.omega301.ICC302 = calc.bias(omega3 = 0.1, ICC.3 = rep(0.2, 3), S = 10)
bias.omega31.ICC301 = calc.bias(omega3 = 1, ICC.3 = rep(0.1, 3), S = 10)

print(bias.omega301.ICC301)
print(bias.omega31.ICC301)
print(bias.omega301.ICC302)


