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
#'
#' @return list of: potential outcomes given control y0, treatment y1,
#'         covariates V.k, X.jk, C.ijk
#'         
#'         Each block has 1 column for each outcome (called "domain")
#'
#' @export
gen_full_data <- function(model.params.list) {
  
  # ------------------------------#
  # setup: convert model params.list to variables
  # ------------------------------#
  has.level.three <- model.params.list[['has.level.three']]
  M        <- model.params.list[['M']];       J       <- model.params.list[['J']];
  K        <- model.params.list[['K']];       nbar    <- model.params.list[['nbar']];
  S.id     <- model.params.list[['S.id']];    D.id    <- model.params.list[['D.id']];
  Xi0      <- model.params.list[['Xi0']];     Xi1     <- model.params.list[['Xi1']];
  rho.V    <- model.params.list[['rho.V']];   xi      <- model.params.list[['xi']];
  eta0.sq  <- model.params.list[['eta0.sq']]; eta1.sq <- model.params.list[['eta1.sq']];
  rho.w0   <- model.params.list[['rho.w0']];  rho.w1  <- model.params.list[['rho.w1']];
  kappa.w  <- model.params.list[['kappa.w']];
  rho.X    <- model.params.list[['rho.X']];   delta   <- model.params.list[['delta']];
  tau0.sq  <- model.params.list[['tau0.sq']]; tau1.sq <- model.params.list[['tau1.sq']];
  rho.u0   <- model.params.list[['rho.u0']];  rho.u1  <- model.params.list[['rho.u1']];
  kappa.u  <- model.params.list[['kappa.u']];
  rho.C    <- model.params.list[['rho.C']];   gamma   <- model.params.list[['gamma']];
  rho.r    <- model.params.list[['rho.r']]
  
  # ------------------------------#
  # Generate school and district IDs
  # ------------------------------#
  # generates vector of school and district assignments, assuming equal sizes of everything
  if ( is.null( S.id ) ) {
    assignments <- gen_simple_assignments( J, K, nbar )
    S.id        <- assignments[['S.id']]  # N-length vector of indiv school assignments i.e. (1,1,2,2,3,3)
    D.id        <- assignments[['D.id']]  # N-length vector of indiv district assignments i.e. (1,1,1,2,2,2)
  }
  N <- length( S.id )
  
  # ------------------------------#
  # Districts: Level 3
  # ------------------------------#
  
  gen.level.three.data = function(K, M, rho.V, sigma.V, eta0.sq, eta1.sq, rho.w0, rho.w1, kappa.w)
  {
    # generate district covariates
    Sigma.V  <- gen_cov_matrix(M, rep(1, M), rep(1, M), rho.V)
    V.k      <- matrix(mvrnorm(K, mu = rep(0, M), Sigma = Sigma.V), K, M)
    
    # covariance of random district effects
    Sigma.w0       <- gen_cov_matrix(M, eta0.sq, eta0.sq, rho.w0)
    # covariance random district impacts
    Sigma.w1       <- gen_cov_matrix(M, eta1.sq, eta1.sq, rho.w1)
    # covariance between impacts and effects
    Sigma.w        <- gen_cov_matrix(M, eta0.sq, eta1.sq, kappa.w)
    # full covariance matrix
    Sigma.w.full   <- gen_RE_cov_matrix( Sigma.w0, Sigma.w1, Sigma.w )
    
    # generate full vector of district random effects and impacts
    w01.k <- matrix(mvrnorm(K, mu = rep(0, 2*M), Sigma = Sigma.w.full), nrow = K, ncol = 2*M)
    w0.k  <- w01.k[,1:M, drop = FALSE]
    w1.k  <- w01.k[,(M+1):(2*M), drop = FALSE]
    return(list(V.k = V.k, w0.k = w0.k, w1.k = w1.k))
  }

  if ( has.level.three ) {
    level.three.data <- gen.level.three.data(K, M, rho.V, sigma.V, eta0.sq, eta1.sq, rho.w0, rho.w1, kappa.w) 
    V.k <- level.three.data[['V.k']]
    w0.k <- level.three.data[['w0.k']]
    w1.k <- level.three.data[['w1.k']]
  } else {
    V.k <- NULL
    w0.k <- NULL
    w1.k <- NULL
  }
  
  # ------------------------------#
  # Schools: Level 2
  # ------------------------------#
  
  gen.level.two.data = function(J, K, M, rho.X, sigma.X, tau0.sq, tau1.sq, rho.u0, rho.u1, kappa.u)
  {
    # generate school covariates
    Sigma.X       <- gen_cov_matrix(M, rep(1, M), rep(1, M), rho.X)
    X.jk          <- matrix(mvrnorm(J*K, mu = rep(0, M), Sigma = Sigma.X), nrow = J*K, ncol = M)
    
    # covariance of school random effects
    Sigma.u0       <- gen_cov_matrix(M, tau0.sq, tau0.sq, rho.u0)
    # covariance of school random impacts
    Sigma.u1       <- gen_cov_matrix(M, tau1.sq, tau1.sq, rho.u1)
    # covariance of school random effects and impacts
    Sigma.u        <- gen_cov_matrix(M, tau0.sq, tau1.sq, kappa.u)
    # full covariance matrix
    Sigma.u.full   <- gen_RE_cov_matrix( Sigma.u0, Sigma.u1, Sigma.u )
    
    # generate full vector of school random effects and impacts
    u01.jk <- matrix(mvrnorm(J*K, mu = rep(0, 2*M), Sigma = Sigma.u.full), nrow = J*K, ncol = 2*M)
    u0.jk  <- u01.jk[,1:M, drop = FALSE]
    u1.jk  <- u01.jk[,(M+1):(2*M), drop = FALSE]
    
    return(list(X.jk = X.jk, u0.jk = u0.jk, u1.jk = u1.jk))
  }
  
  level.two.data <- gen.level.two.data(J, K, M, rho.X, sigma.X, tau0.sq, tau1.sq, rho.u0, rho.u1, kappa.u) 
  X.jk  <- level.two.data[['X.jk']]
  u0.jk <- level.two.data[['u0.jk']]
  u1.jk <- level.two.data[['u1.jk']]
  
  # ------------------------------#
  # Individuals: Level 1
  # ------------------------------#
  
  gen.level.one.data = function(N, M, rho.C, sigma.C, rho.r, Sigma.r)
  {
    # generate individual covariates
    Sigma.C <- gen_cov_matrix(M, rep(1, M), rep(1, M), rho.C)
    C.ijk   <- matrix(mvrnorm(N, mu = rep(0, M), Sigma = Sigma.C), N, M)
    
    # generate individual residuals
    Sigma.r <- gen_cov_matrix(M, rep(1, M), rep(1, M), rho.r)
    r.ijk   <- matrix(mvrnorm(N, mu = rep(0,M), Sigma = Sigma.r), N, M)
    
    return(list(C.ijk = C.ijk, r.ijk = r.ijk))
  }
  
  level.one.data <- gen.level.one.data(N, M, rho.C, sigma.C, rho.r, Sigma.r) 
  C.ijk <- level.one.data[['C.ijk']]
  r.ijk <- level.one.data[['r.ijk']]
  
  # ------------------------------#
  # reformat everything into N x M matrices
  # ------------------------------#
  # for example, D is K x M, now I populate V.k, which is N x M,
  # by filling in district information for each individual
  
  V.ijk  <- w0.ijk <- w1.ijk <-
  X.ijk  <- u0.ijk <- u1.ijk <- matrix(NA, N, M)
  
  Xi0.ijk <- matrix(Xi0, nrow = N, ncol = M, byrow = TRUE) 
  Xi1.ijk <- matrix(Xi1, nrow = N, ncol = M, byrow = TRUE) 
  
  # loop through each individual student
  for(i in 1:N)
  {
    if ( has.level.three )
    {
      # fill in values from district level variables
      V.ijk[i,]    <- V.k[D.id[i],]
      w0.ijk[i,]   <- w0.k[D.id[i],]
      w1.ijk[i,]   <- w1.k[D.id[i],]
    }
    
    # fill in values from school level variables
    X.ijk[i,]    <- X.jk[S.id[i],]
    u0.ijk[i,]   <- u0.jk[S.id[i],]
    u1.ijk[i,]   <- u1.jk[S.id[i],]
  }
  
  # ------------------------------#
  # generate potential outcomes
  # ------------------------------#
  
  # district level
  if ( has.level.three ) {
    mu0.ijk <- Xi0.ijk  + xi * V.ijk + w0.ijk
    mu1.ijk <- Xi1.ijk               + w1.ijk
  } else {
    mu0.ijk <- Xi0.ijk
    mu1.ijk <- Xi1.ijk
  }
  # school level
  theta0.ijk <- mu0.ijk + delta * X.ijk + u0.ijk
  
  # treatment impact
  psi1.ijk   <- mu1.ijk                + u1.ijk
  
  # individual level
  Y0.ijk     <- theta0.ijk  + gamma * C.ijk + r.ijk
  Y1.ijk     <- Y0.ijk                      + psi1.ijk
  
  colnames(Y0.ijk) <- colnames(Y1.ijk) <- paste0('m', 1:M)
  Y0.ijk <- data.frame(Y0.ijk)
  Y1.ijk <- data.frame(Y1.ijk)
  
  ID <- data.frame( S.id = S.id, D.id = D.id )
  
  if ( has.level.three ) {
    return(list(Y0 = Y0.ijk, Y1 = Y1.ijk, V.k = V.ijk, X.jk = X.ijk, C.ijk = C.ijk, ID = ID ))
  } else {
    ID$D.id <- NULL
    return(list(Y0 = Y0.ijk, Y1 = Y1.ijk, V.k = NULL, X.jk = X.ijk, C.ijk = C.ijk, ID = ID ))
  }
}



#' Converts user-inputted parameters into relevant DGP parameters.
#' 
#' @param user.params.list list of DGP parameters
#'
#' @return model.params.list
#'
#' @export
convert.params <- function(user.params.list) {
  
  # save out useful paramters
  M <- user.params.list[['M']]
  ICC.2 <- user.params.list[['ICC.2']]
  ICC.3 <- user.params.list[['ICC.3']]
  R2.1 <- user.params.list[['R2.1']]
  R2.2 <- user.params.list[['R2.2']]
  R2.3 <- user.params.list[['R2.3']]
  omega.2 <- user.params.list[['omega.2']]
  omega.3 <- user.params.list[['omega.3']]
  
  # If no district info, set district parameters to 0
  has.level.three <- TRUE
  if ( is.null( ICC.3 ) ) {
    has.level.three <- FALSE
    ICC.3 <- rep(0, M)
    R2.3 <- rep(0, M)
    omega.3 <- 0
    K <- 1
  }
  
  # check ICC is valid
  if( ICC.2[1] + ICC.3[1] >= 1 )
  {
    stop(paste('ICC.2 + ICC.3 must be less than 1. ICC.2:', ICC.2, 'ICC3:', ICC.3))
  }
  
  # random intercepts variances
  tau0.sq <- ( (1 - R2.2) / (1- R2.1) ) * ( ICC.2 / (1 - ICC.2 - ICC.3) )
  eta0.sq <- ( (1 - R2.3) / (1- R2.1) ) * ( ICC.3 / (1 - ICC.2 - ICC.3) )
  # covariate coefficients
  delta   <- sqrt( (R2.2 / (1 - R2.1)) *  ( ICC.2 / (1 - ICC.2 - ICC.3) ) )
  xi      <- sqrt( (R2.3 / (1 - R2.1)) *  ( ICC.3 / (1 - ICC.2 - ICC.3) ) )
  gamma   <- sqrt( (R2.1 / (1 - R2.1)) )
  
  # random impacts variances
  tau1.sq <- omega.2 * (tau0.sq + delta^2)
  eta1.sq <- omega.3 * (eta0.sq + xi^2)
  
  # grand mean impact
  Xi1 <- user.params.list[['ATE_ES']] * sqrt(xi^2 + gamma^2 + delta^2 + eta0.sq + tau0.sq + 1)
  
  model.params.list <- list(
    has.level.three = has.level.three
    , M = user.params.list[['M']]                    # number of outcomes
    , J = user.params.list[['J']]                    # number of schools
    , K = user.params.list[['K']]                    # number of districts
    , nbar = user.params.list[['nbar']]              # number of individuals per school
    , Xi0 = user.params.list[['Xi0']]                # scalar grand mean outcome under no treatment
    , Xi1 = Xi1                                      # scalar grand mean impact
  )
  
  if ( has.level.three ) {
    model.params.list <- c( model.params.list, list( 
      # -------------------------------------------- level 3
      xi = xi                                          # M-vector of coefficient of district covariates
      , rho.V = user.params.list[['rho.V']]            # MxM correlation matrix of district covariates
      , eta0.sq = eta0.sq                              # M-vector of variances of district random effects
      , eta1.sq = eta1.sq                              # M-vector of variances of district impacts
      , rho.w0 = user.params.list[['rho.w0']]          # MxM matrix of correlations for district random effects
      , rho.w1 = user.params.list[['rho.w1']]          # MxM matrix of correlations for district impacts
      , kappa.w = user.params.list[['kappa.w']]        # MxM matrix of correlations between district random effects and impacts
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
    , rho.u0 = user.params.list[['rho.u0']]          # MxM matrix of correlations for school random effects
    , rho.u1 = user.params.list[['rho.u1']]          # MxM matrix of correlations for school impacts
    , kappa.u = user.params.list[['kappa.u']]        # MxM matrix of correlations between school random effects and impacts
    # -------------------------------------------- level 1
    , gamma = gamma                                  # M-vector of coefficients of individual covariates
    , rho.C = user.params.list[['rho.C']]            # MxM correlation matrix of individual covariates
    , rho.r = user.params.list[['rho.r']]            # MxM matrix of correlations for individual residuals
  ) )

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
gen_Yobs <- function(full.data, T.x) {
  Yobs = full.data$Y0
  Yobs[T.x == 1,] = full.data$Y1[T.x == 1,]
  return(Yobs)
}

