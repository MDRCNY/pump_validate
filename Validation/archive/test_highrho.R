# runs simulations

library(here)
source(here::here("Validation/Simulations", "params.R"))

design = 'blocked_i1_3r'

user.params.list[['K']] <- 15
user.params.list[['nbar']] <- 100
Tbar <- sim.params.list[['Tbar']]

model.params.list <- convert.params(user.params.list)

for(s in 1:S)
{
  # s = 1;
  
  print(s)
  set.seed(s)
  samp.full <- gen_full_data(model.params.list)
  
  S.id <- samp.full$ID$S.id
  D.id  <- samp.full$ID$D.id
  
  T.x <- randomizr::block_ra( S.id, prob = Tbar )
  
  # convert full data to observed data
  samp.obs <- samp.full
  samp.obs$Yobs <- gen_Yobs(samp.full, T.x)
  mdat <- makelist.samp(samp.obs, T.x)
  
  cor(samp.obs$Y0)
  
  for(m in 1:M)
  {
    # m = 1;
    dat <- mdat[[m]]
    dat$S.id <- as.factor(dat$S.id)
    if(!is.null(dat$D.id)){ dat$D.id <- as.factor(dat$D.id) }
    
    form <- as.formula(paste0("Yobs ~ 1 + T.x + V.k + X.jk + C.ijk + (1 + T.x | S.id) + (1 + T.x | D.id)"))
    mod <- lmer(form, data = dat)
  }
}

