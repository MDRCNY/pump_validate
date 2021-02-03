# runs simulations

library(here)
source(here::here("Validation/Simulations", "params.R"))

design = 'blocked_c2_3f'

user.params.list[['J']] <- 20
user.params.list[['K']] <- 10
user.params.list[['nbar']] <- 100
user.params.list[['omega.2']] <- 0
user.params.list[['omega.3']] <- 0.8
user.params.list[['ICC.3']] <- rep(0.7, 3)

# ###### let's write some tests
# ###### model convergence
# model.params.list <- convert.params(user.params.list)
# 
# # generate full, unobserved sample data
# set.seed(123)
# samp.full <- gen_full_data(model.params.list)
# 
# ICC.2 = (delta[1]^2 + tau0.sq[1])/(xi[1]^2 + eta0.sq[1] + delta[1]^2 + tau0.sq[1] + gamma[1]^2 + 1)
# ICC.3 = (xi[1]^2 + eta0.sq[1])/(xi[1]^2 + eta0.sq[1] + delta[1]^2 + tau0.sq[1] + gamma[1]^2 + 1)
# omega.3 = eta1.sq[1]/(eta0.sq[1] + xi[1]^2)
# 
# ICC.2 = var(theta0.ijk[,1] - mu0.ijk[,1])/var(Y0.ijk[,1])
# ICC.3 = var(mu0.ijk[,1])/var(Y0.ijk[,1])
# omega.3 = var(mu1.ijk[,1])/var(mu0.ijk[,1])
# 
# 
# for(s in 1:100)
# {
#   # generate full, unobserved sample data
#   set.seed(s)
#   samp.full <- gen_full_data(model.params.list)
#   S.id <- samp.full$ID$S.id
#   D.id  <- samp.full$ID$D.id
#   
#   T.x <- randomizr::block_and_cluster_ra( blocks = D.id, clusters = S.id, prob = Tbar )
#   
#   # convert full data to observed data
#   samp.obs <- samp.full
#   samp.obs$Yobs <- gen_Yobs(samp.full, T.x)
#   mdat <- makelist.samp(samp.obs, T.x) 
#   
#   for(m in length(mdat))
#   {
#     dat <- mdat[[m]]
#     dat$S.id <- as.factor(dat$S.id)
#     if(!is.null(dat$D.id)){ dat$D.id <- as.factor(dat$D.id) }
#     
#     form <- as.formula(paste0("Yobs ~ 1 + T.x + V.k + X.jk + C.ijk + (1 + T.x | S.id) + (1 + T.x | D.id)"))
#     mod <- tryCatch(
#       lmer(form, data = dat),
#       warning = function(e){ print(paste('warning! s =', s, 'm =', m)); num.warnings <<- num.warnings + 1; print(summary(mod)) }
#     )
#   }
# }


###### model convergence
model.params.list <- convert.params(user.params.list)
Tbar <- sim.params.list[['Tbar']]
num.warnings = 0

for(s in 1:100)
{
  # s = 1;
  # generate full, unobserved sample data
  set.seed(s)
  samp.full <- gen_full_data(model.params.list)
  S.id <- samp.full$ID$S.id
  D.id  <- samp.full$ID$D.id
  
  T.x <- randomizr::block_and_cluster_ra( blocks = D.id, clusters = S.id, prob = Tbar )
  
  # convert full data to observed data
  samp.obs <- samp.full
  samp.obs$Yobs <- gen_Yobs(samp.full, T.x)
  mdat <- makelist.samp(samp.obs, T.x) 
  
  for(m in length(mdat))
  {
    # m = 1;
    dat <- mdat[[m]]
    dat$S.id <- as.factor(dat$S.id)
    if(!is.null(dat$D.id)){ dat$D.id <- as.factor(dat$D.id) }
    
    mod <- interacted_linear_estimators(Yobs = Yobs, Z = T.x, B = D.id, data = dat, control_formula = "X.jk + C.ijk + (1 | S.id)", lmer = TRUE)
    
    # mod <- 
    # tryCatch(
    #   interacted_linear_estimators(Yobs = Yobs, Z = T.x, B = D.id, data = dat, control_formula = "X.jk + C.ijk + (1 | S.id)", lmer = TRUE),
    #   warning = function(e){ print(paste('warning! s =', s, 'm =', m)); num.warnings <<- num.warnings + 1; print(summary(mod)) }
    # )
    # form <- as.formula(paste0("Yobs ~ 1 + T.x + V.k + X.jk + C.ijk + (1 + T.x | S.id) + (1 + T.x | D.id)"))
    # mod <- tryCatch(
    #   lmer(form, data = dat),
    #   warning = function(e){ print(paste('warning! s =', s, 'm =', m)); num.warnings <<- num.warnings + 1; print(summary(mod)) }
    # )
  }
}

print(num.warnings)


# blocked i1 3r
# K = 10, nbar = 50: warnings = 5/100
# K = 15, nbar = 50: warnings = 4/100

