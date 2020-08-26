##
## Simulation to see the role of a predictive covariate for treatment variation
## at level 2 on power.
## 


library(here)
library(MASS)
library(multtest)
library(RcppEigen)
library( tidyverse )


#install.packages("randomizr" )
library( randomizr )
library( lme4 )
library( lmerTest )


# if you haven't installed multtest, code for installing from
# Bioconductor
# if (!requireNamespace("BiocManager", quietly = TRUE)){
#   install.packages("BiocManager")
# }
# BiocManager::install("multtest")


# data generating function
source(here::here("Validation/Simulations", "gen_data.R"))
# Estimate power for each of the designs by running data generating function S sample times and fitting models to estimate power.
source(here::here("Validation/Simulations", "estimate_power_with_simulation.R"))
# Wrapping and creating
# source(here::here("Validation/Simulations", "validate_power.R"))
# For coloring texts and other purposes
source(here::here("Validation/Simulations", "misc.R"))

# I am testing out adding this assumption:
# Beta_ijkm = Gamma_1,km + psi * X_jkm + v_jkm

# instead of:
# Beta_ijkm = Gamma_1,km + v_jkm

# the value of psi can be adjusted in user.params.list

# set simulation parameters
sim.params.list = list(
  S = 2000           # Number of samples for Monte Carlo Simulation
  , maxT = FALSE     # In WY procedure, whether to adjust based on ordered rawp values or ordered rawT values
  , alpha = 0.05     # Significance level
  , MoE = 0.05       # Margin of error
  , p.j = 0.5        # Binomial assignment probability within a school
  , tnum = 10000     # Number of test statistics (samples) for all procedures other than Westfall-Young
  , ncl = 2          # Number of computer clusters (max on RStudio Server is 16)
  , check = FALSE    # Run checks such as printing out quantities
)

# set user-set model parameters

# assumes same correlation structure across all outcomes, covariates, random effects, etc.
# assumes equal sized schools that are evenly split between districts
M = 2
rho.default = 0.5
default.rho.matrix <- gen_corr_matrix(M = M, rho.scalar = rho.default)

user.params.list <- list(
  M = M                                   # number of outcomes
  , J = 10                                # number of schools (per district)
  , K = 9
  , n.j = 20                              # number of individuals per school
  , rho.default = rho.default             # default rho value (optional)
  ################################################## grand mean otucome and impact
  , Xi0 = 0                               # scalar grand mean outcome under no treatment
  , ATE_ES = rep(0.125, M)                  # minimum detectable effect size
  , ICC.2 = rep(0.45, M)                   # school intraclass correlation
  , ICC.3 = rep(0.5, M)                     # district intraclass correlation
  ################################################## level 3: districts
  , R2.3 = rep(0.5, M)                      # percent of district variation explained by district covariates
  # for 2-level model, set to 0
  , rho.D = default.rho.matrix            # MxM correlation matrix of district covariates
  # for 2-level model, set to 0
  , omega.3 = 0.5                           # ratio of district effect size variability to random effects variability
  , rho.w = default.rho.matrix            # MxM matrix of correlations for district random effects
  , rho.z = default.rho.matrix            # MxM matrix of correlations for district impacts
  , theta.wz = matrix(0, M, M)            # MxM matrix of correlations between district random effects and impacts
  
  ################################################## level 2: schools
  , R2.2 = rep(0.7, M)                      # percent of school variation explained by school covariates
  ######## temp
  , psi = rep(0, M)                       # coefficients of school covariate in treatment effect
  ######## temp
  , rho.X = default.rho.matrix            # MxM correlation matrix of school covariates
  , omega.2 = 0.5                           # ratio of school effect size variability to random effects variability
  , rho.u = default.rho.matrix            # MxM matrix of correlations for school random effects
  , rho.v = default.rho.matrix            # MxM matrix of correlations for school impacts
  , theta.uv = matrix(0, M, M)            # MxM matrix of correlations between school random effects and impacts
  
  ################################################## level 1: individuals
  , R2.1 = rep(0, M)                      # percent of indiv variation explained by indiv covariates
  , rho.C = default.rho.matrix            # MxM correlation matrix of individual covariates
  , rho.r = default.rho.matrix            # MxM matrix of correlations for individual residuals
)
rm( M, rho.default, default.rho.matrix )



#' Given a list of potential outcome and covariate components,
#' generate a single dataframe of all the information
gen_data_frame = function( samp.full, Tx = NULL ) {
  
  ID = samp.full$ID
  ID$Tx = Tx
  samp.full$ID = NULL
  
  # Each block of variables has same number of columns?
  ns = sapply( samp.full, ncol )
  stopifnot( length( unique( ns ) ) == 1 )
  
  # Get our variable names
  var.names = names( samp.full )
  #names = paste( rep( names( samp.full ), each=ns[[1]] ), 1:ns[[1]], sep="_" )
  names = paste( rep( var.names, each=ns[[1]] ), 1:ns[[1]], sep="_" )
  names
  
  for ( b in 1:length(samp.full) ) {
    samp.full[b] = as.matrix( samp.full[b] )
  }
  rs = do.call( cbind, samp.full )
  dim( rs )
  colnames( rs )
  colnames(rs) = names
  head( rs )
  rs = as_tibble( rs )
  rs = bind_cols( ID, rs )
  
  
  rs
}




#### Exploring what we get back from the data generating code ####
  
  
  # blocked_i1_2c design
  # estimate power through simulation
  model.params.list = convert.params( user.params.list )
  model.params.list
  names( model.params.list )
  
  samp.full <- gen_full_data(model.params.list, check = FALSE )
  class( samp.full )
  names( samp.full )
  
  table( samp.full$ID$S.k )
  
  
  sapply( samp.full, length )
  sapply( samp.full, class )
  head( samp.full$Y0 )
  head( samp.full$D.k )
  dim( samp.full$Y0 )
  head( samp.full$X.jk )
  dim( samp.full$D.k )
  
  # Randomize
  T.ijk <- rbinom( nrow( samp.full$Y0 ), 1, sim.params.list[['p.j']])
  
  samp.full$Yobs = gen_Yobs(samp.full, T.ijk)
  names( samp.full )
  head( samp.full$Yobs )
  
  samp = gen_data_frame( samp.full )
  
  samp$Tx = T.ijk
  samp
  table( samp$S.jk )
  table( samp$S.jk, samp$S.k, useNA="always"  )
  table( samp$S.k )
  
  library( lme4 )
  M0 = lmer( Yobs_1 ~ 1 +  D.k_1 + X.jk_1 + C.ijk_1 + Tx + (1+Tx | S.jk ),
             data = samp )
  summary( M0 )
  
  #mdat <- makelist.samp(M, samp.full, T.ijk, model.params.list, design = "Blocked_i1_2c" ) #list length M
  #mdat <- makelist.samp(M, samp.full, T.ijk, model.params.list, design = "Blocked_i1_2c" )
  #length( mdat )
  #head( mdat[[1]] )
  
  
  #rawp <- get.rawp(mdat, design = d, model.params.list[['n.j']], model.params.list[['J']]) #vector length M
  #rawt.all[s,] <- rawt
  
  sd( samp$Y0_1 )
  sd( samp$Y1_1 )
  
  ss = samp %>% group_by( S.jk, S.k, D.k_1, X.jk_1 ) %>%
    summarise( Ybar0 = mean( Y0_1 ),
               Ybar1 = mean( Y1_1 ), .groups="drop" )
  nrow( ss )
  ggplot( ss, aes( X.jk_1, Ybar0, col=D.k_1 ) ) +
    facet_wrap( ~ S.k ) +
    geom_point()
    
  
  
  head( samp )
  ggplot( samp, aes( X.jk_1, Yobs_1, col = Tx ) ) +
    facet_wrap( ~ S.k ) +
    geom_jitter( alpha=0.4)
  
  ggplot( samp, aes( D.k_1, Yobs_1, col = Tx ) ) +
    geom_jitter( alpha=0.4)
  
  