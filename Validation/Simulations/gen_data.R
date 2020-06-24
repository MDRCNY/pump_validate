################################
# generate simulation data
################################

################################
# generate 'science' table of full potential outcomes
################################
#' @param model.params.list list of DGP parameters
#' @param check boolean indicating whether to conduct checks
#'
#' @return list of: potential outcomes given control y0, treatment y1,
#'         covariates D.ijk, X.ijk, C.ijk
#'
#' @export
gen_full_data <- function(model.params.list, check = FALSE) {

  if(check){ print(model.params.list) }

  #######################
  # setup: conver model params.list to variables
  #######################

  M        <- model.params.list[['M']];       J       <- model.params.list[['J']];
  K        <- model.params.list[['K']];       n.jk    <- model.params.list[['n.jk']];
  N        <- model.params.list[['N']];
  S.j      <- model.params.list[['S.j']];     S.k     <- model.params.list[['S.k']];
  Xi0      <- model.params.list[['Xi0']];     Xi1     <- model.params.list[['Xi1']];
  rho.D    <- model.params.list[['rho.D']];   xi      <- model.params.list[['xi']];
  eta0.sq  <- model.params.list[['eta0.sq']]; eta1.sq <- model.params.list[['eta1.sq']];
  rho.w    <- model.params.list[['rho.w']];   rho.z   <- model.params.list[['rho.z']];
  theta.wz <- model.params.list[['theta.wz']];
  rho.X    <- model.params.list[['rho.X']];   delta   <- model.params.list[['delta']];
  tau0.sq  <- model.params.list[['tau0.sq']]; tau1.sq <- model.params.list[['tau1.sq']];
  rho.u    <- model.params.list[['rho.u']];   rho.v   <- model.params.list[['rho.v']];
  theta.uv <- model.params.list[['theta.uv']];
  rho.C    <- model.params.list[['rho.C']];   gamma   <- model.params.list[['gamma']];
  rho.r    <- model.params.list[['rho.r']]

  #######################
  # Districts: Level 3
  #######################

  # generate district covariates
  Sigma.D  <- gen.cov.matrix(M, rep(1, M), rep(1, M), rho.D)
  D.k      <- matrix(mvrnorm(K, mu = rep(0, M), Sigma = Sigma.D), K, M)

  # covariance of random district effects
  Sigma.w  <- gen.cov.matrix(M, eta0.sq, eta0.sq, rho.w)
  # covariance random district impacts
  Sigma.z  <- gen.cov.matrix(M, eta1.sq, eta1.sq, rho.z)
  # covariance between impacts and effects
  Sigma.wz <- gen.cov.matrix(M, eta0.sq, eta1.sq, theta.wz)
  # full covariance matrix
  Sigma.wz.full                                                      <- matrix(NA, 2*M, 2*M)
  Sigma.wz.full[1:M, 1:M]                                            <- Sigma.w
  Sigma.wz.full[(M+1):(2*M), (M+1):(2*M)]                            <- Sigma.z
  Sigma.wz.full[1:M, (M+1):(2*M)]                                    <- Sigma.wz
  Sigma.wz.full[(M+1):(2*M), 1:M]                                    <- t(Sigma.wz)

  # generate full vector of district random effects and impacts
  wz.k <- matrix(mvrnorm(K, mu = rep(0,2*M), Sigma = Sigma.wz.full), K, 2*M)
  w.k  <- wz.k[,1:M, drop = FALSE]
  z.k  <- wz.k[,(M+1):(2*M), drop = FALSE]

  #######################
  # Schools: Level 2
  #######################

  # generate school covariates
  Sigma.X         <- gen.cov.matrix(M, rep(1, M), rep(1, M), rho.X)
  X.jk            <- matrix(mvrnorm(J, mu = rep(0, M), Sigma = Sigma.X), J, M)

  # covariance of school random effects
  Sigma.u <- gen.cov.matrix(M, tau0.sq, tau0.sq, rho.u)
  # covariance of school random impacts
  Sigma.v <- gen.cov.matrix(M, tau1.sq, tau1.sq, rho.v)
  # covariance of school random effects and impacts
  Sigma.uv <- gen.cov.matrix(M, tau0.sq, tau1.sq, theta.uv)
  # full covariance matrix
  Sigma.uv.full                                                      <- matrix(NA, 2*M, 2*M)
  Sigma.uv.full[1:M, 1:M]                                            <- Sigma.u
  Sigma.uv.full[(M+1):(2*M), (M+1):(2*M)]                            <- Sigma.v
  Sigma.uv.full[1:M, (M+1):(2*M)]                                    <- Sigma.uv
  Sigma.uv.full[(M+1):(2*M), 1:M]                                    <- t(Sigma.uv)

  ### test that these are the same!
  # Sigma.uv.temp <- matrix(NA, 2*M, 2*M)
  # for (k in 1:M) {
  #   for (l in 1:M) {
  #     Sigma.uv.temp[k,l]                            <- rho.u[k,l] * sqrt(tau0.sq[k]) * sqrt(tau0.sq[l])
  #     Sigma.uv.temp[M+k,M+l]                        <- rho.v[k,l] * sqrt(tau1.sq[k]) * sqrt(tau1.sq[l])
  #     Sigma.uv.temp[M+k,l] <- Sigma.uv.temp[k,M+l]  <- theta[k,l] * sqrt(tau0.sq[k]) * sqrt(tau1.sq[l])
  #   }
  # }
  ### test that these are the same!

  # generate full vector of school random effects and impacts
  uv.jk <- matrix( mvrnorm(J, mu = rep(0, 2*M), Sigma = Sigma.uv.full), J, 2*M )
  u.jk  <- uv.jk[,1:M, drop = FALSE]
  v.jk  <- uv.jk[,(M+1):(2*M), drop = FALSE]

  #######################
  # Individuals: Level 1
  #######################

  # generate individual covariates
  Sigma.C <- gen.cov.matrix(M, rep(1, M), rep(1, M), rho.C)
  C.ijk   <- matrix(mvrnorm(N, mu = rep(0, M), Sigma = Sigma.C), N, M)

  # generate individual residuals
  Sigma.r <- gen.cov.matrix(M, rep(1, M), rep(1, M), rho.r)
  r.ijk   <- matrix(mvrnorm(N, mu = rep(0,M), Sigma = Sigma.r), N, M)

  #######################
  # reformat everything into N x M matrices
  #######################

  # for example, D.k is K x M, now I populate D.ijk, which is N x M,
  # by filling in district information for each individual

  Xi0.ijk = D.ijk = w.ijk = z.ijk =
            X.ijk = u.ijk = v.ijk = matrix(NA, N, M)

  # loop through each individual student
  for(i in 1:N)
  {
    # fill in values from district level variables
    Xi0.ijk[i,] = Xi0[S.k[i]]
    D.ijk[i,]   = D.k[S.k[i],]
    w.ijk[i,]   = w.k[S.k[i],]
    z.ijk[i,]   = z.k[S.k[i],]

    # fill in values from school level variables
    X.ijk[i,]   = X.jk[S.j[i],]
    u.ijk[i,]   = u.jk[S.j[i],]
    v.ijk[i,]   = v.jk[S.j[i],]
  }

  #######################
  # generate potential outcomes
  #######################

  # district level
  Gamma0.ijk <- Xi0        + xi * D.ijk    + w.ijk
  Gamma1.ijk <- Xi1                        + z.ijk
  # school level
  mu.ijk     <- Gamma0.ijk + delta * X.ijk + u.ijk
  beta.ijk   <- Gamma1.ijk                 + v.ijk
  # individual level
  Y0.ijk     <- mu.ijk     + gamma * C.ijk + r.ijk
  Y1.ijk     <- Y0.ijk                     + beta.ijk

  colnames(Y0.ijk) <- colnames(Y1.ijk) <- paste0('m', 1:M)
  Y0.ijk <- data.frame(Y0.ijk)
  Y1.ijk <- data.frame(Y1.ijk)

  # CHECKS
  if (check) {

    # correlations between block-level means
    cor(mu.j)

    # variance of block-level means
    apply(mu.j,2,var)

    # grand means of block-level means (should be equal to Gamma.00)
    apply(mu.j,2,mean)

    # calculated true coef on block-level covariates
    coef.R2.2

    # check of estimated R2.2, coef.R2.1 and var of resid
    summary(lm(mu.j[,1]~X.j[,1]))

    # correlation between between indiv levels of outcome under no treatment
    #cor(output[output$cluster.id==j,c("D.M1.ij","D.M2.ij")])

    # compute block-level means from indiv-level measures
    agg.clustermean <- aggregate(output,by=list(output$cluster.id),FUN="mean")

    # variances of block-level outcomes under no treatment
    apply(agg.clustermean,2,var)

    # check of effects
    apply(agg.clustermean[,paste0("D1.M",1:M,".ij")] - agg.clustermean[,paste0("D0.M",1:M,".ij")],2,mean)

    # estimated ICC from lme
    # require(nlme)
    lme.dat <- groupedData(D0.M1.ij~1|cluster.id,data=output)
    lme.test <- lme(lme.dat)
    varests <- as.numeric(VarCorr(lme.test)[,"Variance"])  # vector of variance estimates
    varests
    ICC.calc <- varests[1]/sum(varests)
    ICC.calc
  }

  return(list(Y0 = Y0.ijk, Y1 = Y1.ijk, D.ijk = D.ijk, X.ijk = X.ijk, C.ijk = C.ijk))
}



################################
# Converts user-inputted parameters into relevant DGP parameters
################################
#' @param user.params.list list of DGP parameters
#'
#' @return model.params.list
#'
#' @export
convert.params <- function(user.params.list, check = FALSE) {

  if(check){ print(user.params.list) }

  ICC.2 = user.params.list[['ICC.3']]
  ICC.3 = user.params.list[['ICC.3']]
  R2.1 = user.params.list[['R2.1']]
  R2.2 = user.params.list[['R2.2']]
  R2.3 = user.params.list[['R2.3']]

  eta0.sq <- sqrt( ( ICC.3*(R2.3 - 1) )/( (ICC.2 + ICC.3 - 1)*(1-R2.1) ))
  tau0.sq <- sqrt( ( ICC.2*(R2.2 - 1) )/( (ICC.2 + ICC.3 - 1)*(1-R2.1) ))
  eta1.sq <- user.params.list[['omega.3']] * eta0.sq
  tau1.sq <- user.params.list[['omega.2']] * tau0.sq
  delta   <- sqrt( ( ICC.3*R2.2*(R2.2 - 1) )/( (ICC.2 + ICC.3 - 1)*(1-R2.2)*(1-R2.1) ))
  xi      <- sqrt( ( ICC.3*R2.3*(R2.3 - 1) )/( (ICC.2 + ICC.3 - 1)*(1-R2.3)*(1-R2.1) ))
  gamma   <- sqrt( R2.1/(1-R2.1) )
  Xi1 <- user.params.list[['MDES']] * sqrt(xi^2 + gamma^2 + delta^2 + eta0.sq + tau0.sq + 1)

  model.params.list <- list(
      M = user.params.list[['M']]                    # number of outcomes
    , J = user.params.list[['J']]                    # number of schools
    , K = user.params.list[['K']]                    # number of districts
    , N = user.params.list[['N']]                    # number of individuals
    , n.j = user.params.list[['n.j']]               # number of individuals per school
    , S.j = user.params.list[['S.j']]                # N-length vector of indiv school assignments i.e. (1,1,2,2,3,3)
    , S.k = user.params.list[['S.k']]                # N-length vector of indiv district assignments i.e. (1,1,1,2,2,2)
    , Xi0 = user.params.list[['Xi0']]                # scalar grand mean outcome under no treatment
    , Xi1 = Xi1                                      # scalar grand mean impact
    , xi = xi                                        # M-vector of coefficient of district covariates
    , rho.D = user.params.list[['rho.D']]            # MxM correlation matrix of district covariates
    , eta0.sq = eta0.sq                              # M-vector of variances of district random effects
    , eta1.sq = eta1.sq                              # M-vector of variances of district impacts
    , rho.w = user.params.list[['rho.w']]            # MxM matrix of correlations for district random effects
    , rho.z = user.params.list[['rho.z']]            # MxM matrix of correlations for district impacts
    , theta.wz = user.params.list[['theta.wz']]      # MxM matrix of correlations between district random effects and impacts
    ################################################## level 2
    , delta = delta                                  # M-vector of coefficients of school covariates
    , rho.X = user.params.list[['rho.X']]            # MxM correlation matrix of school covariates
    , tau0.sq = tau0.sq                              # M-vector of variances of school random effects
    , tau1.sq = tau1.sq                              # M-vector of variances of school impacts
    , rho.u = user.params.list[['rho.u']]            # MxM matrix of correlations for school random effects
    , rho.v = user.params.list[['rho.v']]            #MxM matrix of correlations for school impacts
    , theta.uv = user.params.list[['theta.uv']]      # MxM matrix of correlations between school random effects and impacts
    ################################################## level 1
    , gamma = gamma                                  # M-vector of coefficients of individual covariates
    , rho.C = user.params.list[['rho.C']]            # MxM correlation matrix of individual covariates
    , rho.r = user.params.list[['rho.r']]            # MxM matrix of correlations for individual residuals
  )

  if(check){ print(model.params.list) }

  return(model.params.list)
}


################################
# convert full potential outcomes to Yobs
################################
#' @param full.data full dataset of potential outcoms
#' @param T.ijk N-vector of binary assignment to treat/contorl
#'
#' @return Yobs
#'
#' @export
gen_Yobs <- function(full.data, T.ijk) {
  Yobs = full.data$Y0
  Yobs[T.ijk == 1,] = full.data$Y1[T.ijk== 1,]
  return(Yobs)
}

