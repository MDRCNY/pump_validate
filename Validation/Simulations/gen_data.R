# ------------------------------#
# generate simulation data
# ------------------------------#




#' generate a parameterized covariance matrix from the provided 3 blocks
#' 
#' @return 2M x 2M matrix for generating correlated pairs of random effects
gen_RE_cov_matrix = function( Sigma.w, Sigma.z, Sigma.wz ) {
  stopifnot( nrow(Sigma.w) == ncol(Sigma.w) )
  stopifnot( all( dim(Sigma.z) == dim( Sigma.wz ) ) )
  stopifnot( all( dim(Sigma.w) == dim( Sigma.wz ) ) )
  
  M = nrow(Sigma.w)
  
  # full covariance matrix
  Sigma.wz.full                                                      <- matrix(NA, 2*M, 2*M)
  Sigma.wz.full[1:M, 1:M]                                            <- Sigma.w
  Sigma.wz.full[(M+1):(2*M), (M+1):(2*M)]                            <- Sigma.z
  Sigma.wz.full[1:M, (M+1):(2*M)]                                    <- Sigma.wz
  Sigma.wz.full[(M+1):(2*M), 1:M]                                    <- t(Sigma.wz)
  
  Sigma.wz.full
}


#' generate 'science' table of full potential outcomes
#'
#' @param model.params.list list of DGP parameters
#' @param check boolean indicating whether to conduct checks
#'
#' @return list of: potential outcomes given control y0, treatment y1,
#'         covariates D.k, X.jk, C.ijk
#'         
#'         Each block has 1 column for each outcome (called "domain")
#'
#' @export
gen_full_data <- function(model.params.list, check = FALSE) {
  
  
  if(check){ print(model.params.list) }
  
  # ------------------------------#
  # setup: convert model params.list to variables
  # ------------------------------#
  
  M        <- model.params.list[['M']];       J       <- model.params.list[['J']];
  K        <- model.params.list[['K']];       n.j     <- model.params.list[['n.j']];
  S.jk     <- model.params.list[['S.jk']];    S.k     <- model.params.list[['S.k']];
  Xi0      <- model.params.list[['Xi0']];     Xi1     <- model.params.list[['Xi1']];
  rho.D    <- model.params.list[['rho.D']];   xi      <- model.params.list[['xi']];
  eta0.sq  <- model.params.list[['eta0.sq']]; eta1.sq <- model.params.list[['eta1.sq']];
  rho.w    <- model.params.list[['rho.w']];   rho.z   <- model.params.list[['rho.z']];
  theta.wz <- model.params.list[['theta.wz']];
  rho.X    <- model.params.list[['rho.X']];   delta   <- model.params.list[['delta']];
  ##-------temp
  psi      <- model.params.list[['psi']]
  if(is.null(psi)){ psi <- 0 }
  ##-------temp
  tau0.sq  <- model.params.list[['tau0.sq']]; tau1.sq <- model.params.list[['tau1.sq']];
  rho.u    <- model.params.list[['rho.u']];   rho.v   <- model.params.list[['rho.v']];
  theta.uv <- model.params.list[['theta.uv']];
  rho.C    <- model.params.list[['rho.C']];   gamma   <- model.params.list[['gamma']];
  rho.r    <- model.params.list[['rho.r']]
  
  has_level_three = FALSE
  if ( !is.null( K ) && K > 1 ) {
    has_level_three <- TRUE
  } else {
    K <- 1
  }
  
  # ------------------------------#
  # Generate school and district IDs
  # ------------------------------#
  # generates vector of school and district assignments, assuming equal sizes of everything
  if ( is.null( S.jk ) ) {
    assignments <- gen_simple_assignments( J, K, n.j )
    S.jk        <- assignments[['S.jk']]  # N-length vector of indiv school assignments i.e. (1,1,2,2,3,3)
    S.k         <- assignments[['S.k']]  # N-length vector of indiv district assignments i.e. (1,1,1,2,2,2)
  }
  N <- length( S.jk )
  
  
  # ------------------------------#
  # Districts: Level 3
  # ------------------------------#
  
  if ( has_level_three ) {
    
    # generate district covariates
    Sigma.D  <- gen_cov_matrix(M, rep(1, M), rep(1, M), rho.D)
    D        <- matrix(mvrnorm(K, mu = rep(0, M), Sigma = Sigma.D), K, M)
    
    # covariance of random district effects
    Sigma.w       <- gen_cov_matrix(M, eta0.sq, eta0.sq, rho.w)
    # covariance random district impacts
    Sigma.z       <- gen_cov_matrix(M, eta1.sq, eta1.sq, rho.z)
    # covariance between impacts and effects
    Sigma.wz      <- gen_cov_matrix(M, eta0.sq, eta1.sq, theta.wz)
    # full covariance matrix
    Sigma.wz.full <- gen_RE_cov_matrix( Sigma.w, Sigma.z, Sigma.wz )
    
    # generate full vector of district random effects and impacts
    wz.k <- mvrnorm(K, mu = rep(0,2*M), Sigma = Sigma.wz.full)
    w.k  <- wz.k[,1:M, drop = FALSE]
    z.k  <- wz.k[,(M+1):(2*M), drop = FALSE]
  } else {
    D = NULL
  }
  
  # ------------------------------#
  # Schools: Level 2
  # ------------------------------#
  
  # generate school covariates
  Sigma.X       <- gen_cov_matrix(M, rep(1, M), rep(1, M), rho.X)
  X             <- mvrnorm(J*K, mu = rep(0, M), Sigma = Sigma.X)
  
  # covariance of school random effects
  Sigma.u       <- gen_cov_matrix(M, tau0.sq, tau0.sq, rho.u)
  # covariance of school random impacts
  Sigma.v       <- gen_cov_matrix(M, tau1.sq, tau1.sq, rho.v)
  # covariance of school random effects and impacts
  Sigma.uv      <- gen_cov_matrix(M, tau0.sq, tau1.sq, theta.uv)
  # full covariance matrix
  Sigma.uv.full <- gen_RE_cov_matrix( Sigma.u, Sigma.v, Sigma.uv )
  
  # generate full vector of school random effects and impacts
  uv.jk <- mvrnorm(J*K, mu = rep(0, 2*M), Sigma = Sigma.uv.full)
  u.jk  <- uv.jk[,1:M, drop = FALSE]
  v.jk  <- uv.jk[,(M+1):(2*M), drop = FALSE]
  
  # ------------------------------#
  # Individuals: Level 1
  # ------------------------------#
  
  # generate individual covariates
  Sigma.C <- gen_cov_matrix(M, rep(1, M), rep(1, M), rho.C)
  C.ijk   <- matrix(mvrnorm(N, mu = rep(0, M), Sigma = Sigma.C), N, M)
  
  # generate individual residuals
  Sigma.r <- gen_cov_matrix(M, rep(1, M), rep(1, M), rho.r)
  r.ijk   <- matrix(mvrnorm(N, mu = rep(0,M), Sigma = Sigma.r), N, M)
  
  # ------------------------------#
  # reformat everything into N x M matrices
  # ------------------------------#
  
  # for example, D is K x M, now I populate D.k, which is N x M,
  # by filling in district information for each individual
  
  Xi0.ijk = D.k = w.ijk = z.ijk =
    X.jk = u.ijk = v.ijk = matrix(NA, N, M)
  
  # loop through each individual student
  for(i in 1:N)
  {
    if ( has_level_three ) {
      # fill in values from district level variables
      Xi0.ijk[i,] = Xi0[S.k[i]]
      D.k[i,]     = D[S.k[i],]
      w.ijk[i,]   = w.k[S.k[i],]
      z.ijk[i,]   = z.k[S.k[i],]
    }
    
    # fill in values from school level variables
    X.jk[i,]    = X[S.jk[i],]
    u.ijk[i,]   = u.jk[S.jk[i],]
    v.ijk[i,]   = v.jk[S.jk[i],]
  }
  
  # ------------------------------#
  # generate potential outcomes
  # ------------------------------#
  
  # district level
  if ( has_level_three ) {
    Gamma0.ijk <- Xi0        + xi * D.k + w.ijk
    Gamma1.ijk <- Xi1                   + z.ijk
  } else {
    Gamma0.ijk <- matrix(Xi0, nrow = N, ncol = M, byrow = TRUE)
    Gamma1.ijk <- matrix(Xi1, nrow = N, ncol = M, byrow = TRUE)
  }
  
  # school level
  mu.ijk     <- Gamma0.ijk + delta * X.jk + u.ijk
  
  ##-------temp
  # allow for school-level covariate to influence treatment
  beta.ijk   <- Gamma1.ijk + psi * X.jk + v.ijk
  # beta.ijk   <- Gamma1.ijk                 + v.ijk
  ##-------temp
  
  # individual level
  Y0.ijk     <- mu.ijk     + gamma * C.ijk + r.ijk
  Y1.ijk     <- Y0.ijk                     + beta.ijk
  
  colnames(Y0.ijk) <- colnames(Y1.ijk) <- paste0('m', 1:M)
  Y0.ijk <- data.frame(Y0.ijk)
  Y1.ijk <- data.frame(Y1.ijk)
  
  # some useful checks
  # TODO: update this better
  if (check) {
    
    # correlations between block-level means
    print(paste('Correlation of block-level means'))
    print(cor(mu.ijk))
    
    # variance of block-level means
    print(paste('Variance of block-level means'))
    print(apply(mu.ijk, 2, var))
    
    # grand means of block-level means (should be equal to Gamma.00)
    print(paste('Means of block-level means'))
    print(apply(mu.ijk, 2, mean))
    
    # calculated true coef on block-level covariates
    print(paste('True R2.1'))
    print(1 - Sigma.r[1,1]/(gamma^2 + Sigma.r[1,1]))
    
    # check of estimated R2.2, coef.R2.1 and var of resid
    print('Estimated R2.2')
    summary(lm(mu.ijk[,1]~X.jk[,1]))$r.squared
    
    # compute block-level means from indiv-level measures
    # agg.clustermean <- aggregate(output, by = list(output$cluster.id),FUN="mean")
    
    # variances of block-level outcomes under no treatment
    # apply(agg.clustermean,2,var)
    
    # check of effects
    # apply(agg.clustermean[,paste0("D1.M",1:M,".ij")] - agg.clustermean[,paste0("D0.M",1:M,".ij")],2,mean)
    
    # estimated ICC from lme
    # require(nlme)
    # lme.dat <- groupedData(D0.M1.ij~1|cluster.id,data=output)
    # lme.test <- lme(lme.dat)
    # varests <- as.numeric(VarCorr(lme.test)[,"Variance"])  # vector of variance estimates
    # varests
    # ICC.calc <- varests[1]/sum(varests)
    # ICC.calc
  }
  
  ID = data.frame( S.jk = S.jk, S.k = S.k )
  
  if ( has_level_three ) {
    return(list(Y0 = Y0.ijk, Y1 = Y1.ijk, D.k = D.k, X.jk = X.jk, C.ijk = C.ijk, ID = ID ))
  } else {
    ID$S.k <- NULL
    return(list(Y0 = Y0.ijk, Y1 = Y1.ijk, X.jk = X.jk, C.ijk = C.ijk, ID = ID ))
  }
}



#' Converts user-inputted parameters into relevant DGP parameters.
#' 
#' @param user.params.list list of DGP parameters
#'
#' @return model.params.list
#'
#' @export
convert.params <- function(user.params.list, check = FALSE) {
  
  if(check){ print(user.params.list) }
  
  ICC.2 = user.params.list[['ICC.2']]
  ICC.3 = user.params.list[['ICC.3']]
  
  stopifnot( ICC.2 + ICC.3 < 1 )
  
  R2.1 = user.params.list[['R2.1']]
  R2.2 = user.params.list[['R2.2']]
  R2.3 = user.params.list[['R2.3']]
  omega.2 = user.params.list[['omega.2']]
  omega.3 = user.params.list[['omega.3']]
  
  # If no district info, set district parameters to 0
  has_level_three = TRUE
  if ( is.null( ICC.3 ) ) {
    has_level_three = FALSE
    ICC.3 = 0
    R2.3 = 0
    omega.3 = 0
    K = 1
  }
  
  eta0.sq <- sqrt( ( ICC.3*(R2.3 - 1) )/( (ICC.2 + ICC.3 - 1)*(1-R2.1) ))
  tau0.sq <- sqrt( ( ICC.2*(R2.2 - 1) )/( (ICC.2 + ICC.3 - 1)*(1-R2.1) ))
  eta1.sq <- omega.3 * eta0.sq
  tau1.sq <- omega.2 * tau0.sq
  delta   <- sqrt( ( ICC.3*R2.2*(R2.2 - 1) )/( (ICC.2 + ICC.3 - 1)*(1-R2.2)*(1-R2.1) ))
  # psi     <- sqrt( ( ICC.3*R2.2*(R2.2 - 1) )/( (ICC.2 + ICC.3 - 1)*(1-R2.2)*(1-R2.1) ))
  xi      <- sqrt( ( ICC.3*R2.3*(R2.3 - 1) )/( (ICC.2 + ICC.3 - 1)*(1-R2.3)*(1-R2.1) ))
  gamma   <- sqrt( R2.1/(1-R2.1) )
  Xi1 <- user.params.list[['ATE_ES']] * sqrt(xi^2 + gamma^2 + delta^2 + eta0.sq + tau0.sq + 1)
  
  model.params.list <- list(
    M = user.params.list[['M']]                      # number of outcomes
    , J = user.params.list[['J']]                    # number of schools
    , K = user.params.list[['K']]                    # number of districts
    , n.j = user.params.list[['n.j']]                # number of individuals per school
    , Xi0 = user.params.list[['Xi0']]                # scalar grand mean outcome under no treatment
    , Xi1 = Xi1                                      # scalar grand mean impact
  )
  
  
  if ( has_level_three ) {
    model.params.list <- c( model.params.list, list( 
      # -------------------------------------------- level 3
      xi = xi                                          # M-vector of coefficient of district covariates
      , rho.D = user.params.list[['rho.D']]            # MxM correlation matrix of district covariates
      , eta0.sq = eta0.sq                              # M-vector of variances of district random effects
      , eta1.sq = eta1.sq                              # M-vector of variances of district impacts
      , rho.w = user.params.list[['rho.w']]            # MxM matrix of correlations for district random effects
      , rho.z = user.params.list[['rho.z']]            # MxM matrix of correlations for district impacts
      , theta.wz = user.params.list[['theta.wz']]      # MxM matrix of correlations between district random effects and impacts
    ) )
  }
  
  model.params.list <- c( model.params.list, list(
    # -------------------------------------------- level 2
    delta = delta                                    # M-vector of coefficients of school covariates
    #-------temp
    , psi = user.params.list[['psi']]                # coefficient of school covariate in treatment effect
    #-------temp
    , rho.X = user.params.list[['rho.X']]            # MxM correlation matrix of school covariates
    , tau0.sq = tau0.sq                              # M-vector of variances of school random effects
    , tau1.sq = tau1.sq                              # M-vector of variances of school impacts
    , rho.u = user.params.list[['rho.u']]            # MxM matrix of correlations for school random effects
    , rho.v = user.params.list[['rho.v']]            # MxM matrix of correlations for school impacts
    , theta.uv = user.params.list[['theta.uv']]      # MxM matrix of correlations between school random effects and impacts
    # -------------------------------------------- level 1
    , gamma = gamma                                  # M-vector of coefficients of individual covariates
    , rho.C = user.params.list[['rho.C']]            # MxM correlation matrix of individual covariates
    , rho.r = user.params.list[['rho.r']]            # MxM matrix of correlations for individual residuals
  ) )

  
  if(check){ print(model.params.list) }
  
  nulls = sapply( model.params.list, is.null )
  model.params.list = model.params.list[ !nulls ]
  
  return(model.params.list)
 
}


#' convert full potential outcomes to Yobs
#' 
#' @param full.data full dataset of potential outcoms
#' @param T.ijk N-vector of binary assignment to treat/contorl
#'
#' @return Yobs
#'
#' @export
gen_Yobs <- function(full.data, T.ijk) {
  Yobs = full.data$Y0
  Yobs[T.ijk == 1,] = full.data$Y1[T.ijk == 1,]
  return(Yobs)
}

