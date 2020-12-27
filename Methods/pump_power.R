# the if statement checks if we have a grab.pval function. If not, pull it from utils.R file.
if(!exists("grab.pval", mode = "function")) source(here::here("R", "utils.R"))

#' Helper function for Westfall Young Single Step
#'
#' The  function  comp.rawt.SS is  needed  to  implement  the  Westfall-Young single-step multiple
#' testing procedure (MTP). It operates on one row of null test statistics.
#'
#' @param abs.Zs.H0.1row A vector of permutated test statistics values under H0
#' @param abs.Zs.H1.1samp One sample of raw statistics
#' @param oo Order matrix of test statistics in descending order (Only used in Step Down)
#'
#' @return returns a vector of 1s and 0s with length of M outcomes
#'
#'
comp.rawt.SS <- function(abs.Zs.H0.1row, abs.Zs.H1.1samp, oo) {
  
  # getting the number of M outcomes from 1 row of H0
  M <- length(abs.Zs.H0.1row)
  # creating an empty vector of length M to save boolean values
  maxt <- rep(NA, M)
  
  for (m in 1:M) {
    
    # comparing the maximum of null test values of M outcomes to each of the alternative test raw sample values
    # saving each M boolean in maxt vector
    maxt[m] <- max(abs.Zs.H0.1row) > abs.Zs.H1.1samp[m]
    
  }
  return(as.integer(maxt))
}

#' Helper Functions for WestFallYoung Step down
#'
#' @param abs.Zs.H0.1row A vector of permutated test statistics values under H0
#' @param abs.Zs.H1.1samp One sample of raw statistics
#' @param oo Order matrix of test statistics in descending order
#' @return returns a vector of 1s and 0s with lengths of M outcomes
#' @export
#'
comp.rawt.SD <- function(abs.Zs.H0.1row, abs.Zs.H1.1samp, oo) {
  
  # getting M number of outcomes from 1 row of statistics
  M <- length(abs.Zs.H0.1row)
  # creating an empty vector of length M to save boolean values
  maxt <- rep(NA, M)
  # saving the null test statistics
  nullt.oo <- abs.Zs.H0.1row[oo]
  # saving the raw test statistics under H1
  rawt.oo <- abs.Zs.H1.1samp[oo]
  # saving the first boolean by comparing the max of null values with the first of raw test statistics
  maxt[1] <- max(nullt.oo) > rawt.oo[1]
  
  # Step-down comparison where the next max of null values is compared to the next raw test statistics
  for (h in 2:M) {
    maxt[h] <- max(nullt.oo[-(1:(h-1))]) > rawt.oo[h]
  } # end of for loop
  
  return(as.integer(maxt))
}

#' WestFallYoung Single Step Adjustment Function
#'
#' This adjustment function utilizes the comp.rawt.SS helper function to compare
#' each row of the matrix sample test statistics under
#' alternative hypothesis to all the rows in the matrix of the test statistics under the complete null (i.e think a distribution).
#' Furthermore, it carries out the comparison for all the samples of raw test statistics under the alternative.
#'
#' @param snum the number of samples for which test statistics under the alternative hypothesis
#' are compared with the distribution (matrix) of test statistics under the complete null (this distribution
#' is obtained through drawing test values under H0 with a default of 10,000)
#' @param abs.Zs.H0 a matrix of test statistics under the complete null
#' @param abs.Zs.H1 a matrix of raw test statistics under the alternative
#'
#' @return a matrix of adjusted test statistics values

adjust.allsamps.WYSS <- function(snum, abs.Zs.H0, abs.Zs.H1) {
  
  # creating the matrix to store the adjusted test values with the number of samples &
  # number of M outcomes
  adjp.WY <- matrix(NA, snum, ncol(abs.Zs.H0))
  # looping through all the samples of raw test statistics under the alternative hypothesis
  doWY <- for (s in 1:snum) {
    
    # using apply to compare the distribution of test statistics under H0 with 1 sample of the raw statistics under H1
    ind.B <- t(apply(abs.Zs.H0, 1, comp.rawt.SS, abs.Zs.H1.1samp = abs.Zs.H1[s,]))
    # calculating the p-value for each sample
    adjp.WY[s,] <- colMeans(ind.B)
    
  }
  return(adjp.WY)
}

#' Westfall Young Step Down Function
#'
#' This adjustment function utilizes the comp.rawt.SD helper function to compare
#' each row of the matrix sample test statistics under
#' alternative hypothesis to all the rows in the matrix of the test statistics under the complete null (i.e think a distribution).
#' Furthermore, it carries out the comparison for all the samples of raw test statistics under the alternative.
#'
#' @param snum the number of samples for which test statistics under the alternative hypothesis
#' are compared with the distribution (matrix) of test statistics under the complete null (this distribution
#' is obtained through drawing test values under H0 with a default of 10,000)
#' @param abs.Zs.H0 a matrix of test statistics under the complete null
#' @param abs.Zs.H1 a matrix of raw test statistics under the alternative
#' @param order.matrix Order matrix of test statistics in descending order
#' @param ncl number of clusters to be made for parallelization. The default is 2.
#'
#' @return a matrix of adjusted test statistics values

adjust.allsamps.WYSD <- function(snum, abs.Zs.H0, abs.Zs.H1, order.matrix, cl = NULL) {
  
  # getting M number of outcomes vector
  M <- ncol(abs.Zs.H0)
  # setting up the matrix to save the adjusted p values
  adjp.WY <- matrix(NA, snum, M)
  
  get.adjp.minp <- function(abs.Zs.H0, abs.Zs.H1, comp.rawt.H0, order.matrix, M, s)
  {
    # using apply to compare the distribution of test statistics under H0 with 1 sample of the raw statistics under H1
    ind.B <- t(apply(abs.Zs.H0, 1, comp.rawt.SD, abs.Zs.H1.1samp = abs.Zs.H1[s,], oo = order.matrix[s,]))
    
    pi.p.m <- colMeans(ind.B)
    
    # enforcing monotonicity
    adjp.minp <- numeric(M)
    adjp.minp[1] <- pi.p.m[1]
    
    for (h in 2:M) { adjp.minp[h] <- max(pi.p.m[h], adjp.minp[h-1]) }
    return(adjp.minp)
  }
  
  if(!is.null(cl))
  {
    # leveraging snow to run multiple cores for foreach loops
    doParallel::registerDoParallel(cl)
    # registering the comp.rawt.SD function in global enivronment of each node
    parallel::clusterExport(cl = cl, list('comp.rawt.SD'), envir = environment())
    
    # dopar is a special function that has to be explicitly called from the foreach package
    # dopar accepts only 2 parameters. The number of times to execute the parallelization and the
    # series of steps to execute
    # `%dopar%` <- foreach::`%dopar%`
    # making s a local variable to perpetuate across (created to bypass a package requirement)
    # s = 1:snum
    doWY <- foreach::foreach(s = 1:snum, .combine = rbind) %dopar% {
      adjp.minp <- get.adjp.minp(abs.Zs.H0, abs.Zs.H1, comp.rawt.H0, order.matrix, M, s)
      adjp.WY[s,] <- adjp.minp[order.matrix[s,]]
    }
  } else
  {
    doWY <- foreach::foreach(s = 1:snum, .combine = rbind) %do% {
      adjp.minp <- get.adjp.minp(abs.Zs.H0, abs.Zs.H1, comp.rawt.H0, order.matrix, M, s)
      adjp.WY[s,] <- adjp.minp[order.matrix[s,]]
    }
  }
  
  return(doWY)
}

#' The function calc.Q.m computes Qm, the standard error of the effect size estimate
#'
#' @param design RCT design (see list/naming convention)
#' @param MDES  a vector of length M corresponding to the minimum detectable effect sizes (MDESs) for the M outcomes
#' @param J the number of schools
#' @param K the number of districts
#' @param nbar the harmonic means of the number of units per block
#' @param R2.1 a vector of length M corresponding to R^2 for Level-1 covariates for M outcomes
#' @param R2.2 a vector of length M corresponding to R^2 for Level-2 covariates for M outcomes
#' @param R2.3 a vector of length M corresponding to R^2 for Level-3 covariates for M outcomes
#' @param ICC.2 a vector of length M of school intraclass correlation	
#' @param ICC.3 a vector of length M of district intraclass correlation	
#' @param omega.2 ratio of school effect size variability to random effects variability
#' @param omega.3 ratio of district effect size variability to random effects variability
#' @param Tbar the proportion of test statistics assigned to treatment within each block group
#'
#' @return mean of the test statistics under the joint alternative hypothesis

calc.Q.m <- function(design, J, K, nbar, R2.1, R2.2, R2.3, ICC.2, ICC.3, omega.2, omega.3, Tbar) {
  
  if(design %in% c('blocked_i1_2c', 'blocked_i1_2f'))
  {
    Q.m <- sqrt(1 - R2.1) / sqrt(Tbar * (1-Tbar) * J * nbar) 
  } else if (design == 'blocked_i1_2r')
  {
    Q.m <- sqrt( (ICC.2 * omega.2)/J + ((1 - ICC.2) * (1 - R2.1))/(Tbar * (1-Tbar) * J * nbar) )
  } else if (design == 'blocked_i1_3r')
  {
    Q.m <- sqrt( (ICC.3 * omega.3)/K + (ICC.2 * omega.2)/J*K + ((1 - ICC.2) * (1 - R2.1))/(Tbar * (1-Tbar) * J * K * nbar) )
  } else if (design == 'simple_c2_2r')
  {
    Q.m <- sqrt( (ICC.2 * (1 - R2.2))/Tbar * (1-Tbar) * J + (1 - ICC.2)*(1 - R2.1)/(Tbar * (1-Tbar) * J * nbar))
  } else if (design == 'simple_c3_3r')
  {
    Q.m <- sqrt( ICC.2 * (1 - R2.3)/Tbar * (1-Tbar) * K + (ICC.2 * (1 - R2.2))/Tbar * (1-Tbar) * J * K  + (1 - ICC.2) * (1 - R2.1)/Tbar * (1-Tbar) * J * K * nbar )
  } else if (design == 'blocked_c2_3f')
  {
    Q.m <- sqrt( ICC.2 * (1 - R2.2)/Tbar * (1-Tbar) * J + ((1 - ICC.2) * (1 - R2.1)) / Tbar * (1-Tbar) * J * nbar)
  } else if (design == 'blocked_c2_3r')
  {
    Q.m <- sqrt( ICC.3/K + ICC.2 * (1 - R2.2)/Tbar * (1-Tbar) * J * K + ((1 - ICC.2 - ICC.3) * (1 - R2.1))/Tbar * (1-Tbar) * J * K * nbar)
  }else
  {
    stop(paste('Design not implemented:', design))
  }
  return(Q.m)
}

#' This function calculates the degree of freedom for 2 level RCT block design
#' @param design RCT design (see list/naming convention)
#' @param J the number of schools
#' @param K the number of districts
#' @param nbar units per block
#' @param numCovar.1 number of Level 1 baseline covariates (not including block dummies)
#' @param numCovar.2 number of Level 2 baseline covariates
#' @param numCovar.3 number of Level 3 baseline covariates
#'
#' @return the degree of freedom

calc.df <- function(design, J, K, nbar, numCovar.1, numCovar.2, numCovar.3) {
  
  if(design == 'blocked_i1_2c')
  {
    df <- J*nbar - numCovar.1 - J - 1
  } else if (design == 'blocked_i1_2f')
  {
    df <- J*nbar - numCovar.1 - 2*J
  } else if (design == 'blocked_i1_2r')
  {
    df <- J - numCovar.1 - 1
  } else if (design == 'blocked_i1_3r')
  {
    df <- K - numCovar.3 - 1
  } else if (design == 'simple_c2_2r')
  {
    df <- J - numCovar.1 -2
  } else if (design == 'simple_c3_3r')
  {
    df <- K - numCovar.3 - 2
  } else if (design == 'blocked_c2_3f')
  {
    df <- K * (J - 2) - numCovar.2 - 2
  }else if (design == 'blocked_c2_3r')
  {
    df <- K - numCovar.3 - 1
  } else
  {
    stop(paste('Design not implemented:', design))
  }
  
  return(df)
}

#' Calculate power using PUMP method
#'
#' This functions calculates power for all definitions of power (individual, d-minimal, complete) for all the different MTPs
#' (Bonferroni, Holms, Bejamini-Hocheberg, Westfall-Young Single Step, Westfall-Young Step Down).

#' @param design RCT design (see list/naming convention)
#' @param MTP multiple adjustment procedures of interest such as Bonferroni, BH, Holms, WY_SS & WY_SD
#'              (we expect inputs in  such order)
#' @param MDES a vector of length M corresponding to the MDESs for the M outcomes
#' @param M the number of hypothesis tests (outcomes)
#' @param J the number of schools
#' @param K the number of districts
#' @param nbar the harmonic mean of the number of units per block
#' @param Tbar the proportion of samples that are assigned to the treatment
#' @param alpha the family wise error rate (FWER)
#' @param numCovar.1 number of Level 1 baseline covariates (not including block dummies)
#' @param numCovar.2 number of Level 2 baseline covariates (set to 0 for this design)
#' @param numCovar.3 number of Level 3 baseline covariates (set to 0 for this design)
#' @param R2.1 a vector of length M corresponding to R^2 for M outcomes of Level 1 (R^2 = variation in the data explained by the model)
#' @param R2.2 a vector of length M corresponding to R^2 for M outcomes of Level 2 (R^2 = variation in the data explained by the model)
#' @param ICC.2 school intraclass correlation
#' @param ICC.3 district intraclass correlation	
#' @param omega.2 ratio of school effect size variability to random effects variability
#' @param omega.3 ratio of district effect size variability to random effects variability
#' @param tnum the number of test statistics (samples) for all procedures other than Westfall-Young & number of permutations for WY. The default is set at 10,000
#' @param snum the number of samples for Westfall-Young. The default is set at 1,000.
#' @param cl clusters object to use for parallel processing.
#' @param rho correlation between outcomes
#' @param updateProgress the callback function to update the progress bar (User does not have to input anything)
#'
#' @importFrom multtest mt.rawp2adjp
#' @return power results across all definitions of power and MTP
#' @export
#'
#'
pump_power <- function(
  design, MTP, MDES, M, J, K = 1, nbar, Tbar, alpha, numCovar.1 = 0, numCovar.2 = 0,
  numCovar.3 = 0, R2.1, R2.2 = NULL, R2.3 = NULL, ICC.2, ICC.3 = NULL,
  rho, omega.2, omega.3 = NULL,
  tnum = 10000, snum = 1000, cl = NULL, updateProgress = NULL
)
{
  if(length(MDES) < M)
  {
    stop(paste('Please provide a vector of MDES values of length M. Current vector:', MDES, 'M =', M))
  }
  
  # Setting a default Sigma
  sigma <- matrix(rho, M, M)
  diag(sigma) <- 1
  
  # compute Q(m) for all false nulls. We are calculating the test statistics for when the alternative hypothesis is true.
  t.shift <- MDES/calc.Q.m(design, J, K, nbar, R2.1, R2.2, R2.3, ICC.2, ICC.3, omega.2, omega.3, Tbar)
  t.df <- calc.df(design, J, K, nbar, numCovar.1, numCovar.2, numCovar.3)
  
  t.shift.mat <- t(matrix(rep(t.shift, tnum), M, tnum)) # repeating shift.beta on every row
  
  # generate test statistics and p-values under null and alternative $s=\frac{1}{2}$
  # rmvt draws from a multivariate t-distribution
  Zs.H0 <- mvtnorm::rmvt(tnum, sigma = sigma, df = t.df, delta = rep(0, M), type = c("shifted", "Kshirsagar"))
  Zs.H1 <- Zs.H0 + t.shift.mat
  
  # calculates p values from quantiles and degrees of freedom
  pvals.H0 <- pt(-abs(Zs.H0), df = t.df) * 2
  pvals.H1 <- pt(-abs(Zs.H1), df = t.df) * 2
  
  # getting the absolute values of the test statistics
  abs.Zs.H0 <- abs(Zs.H0)
  abs.Zs.H1 <- abs(Zs.H1)
  
  # 1st call back to progress bar on progress of calculation: P values generation
  if (is.function(updateProgress) & !is.null(abs.Zs.H0)) {
    updateProgress(message = "P-values have been generated!")
  }
  
  # seperating out p values that are adjusted by Bonferroni, Holm and Benjamini-Hocheberg
  grab.pval <- function(...,proc) {return(...$adjp[order(...$index),proc])}
  
  rawp <- pvals.H1
  
  if (MTP == "Bonferroni"){
    
    adjp <- apply(pvals.H1, 1, multtest::mt.rawp2adjp, proc = "Bonferroni", alpha = alpha)
    adjp <- do.call(rbind, lapply(adjp, grab.pval, proc = "Bonferroni"))
    
  } else if (MTP == "Holm") {
    
    adjp <- apply(pvals.H1, 1, multtest::mt.rawp2adjp, proc = "Holm", alpha = alpha)
    adjp <- do.call(rbind, lapply(adjp, grab.pval, proc = "Holm"))
    
  } else if (MTP == "BH") {
    
    adjp <- apply(pvals.H1, 1, multtest::mt.rawp2adjp, proc = c("BH"), alpha = alpha)
    adjp <- do.call(rbind, lapply(adjp, grab.pval, proc = "BH"))
    
  } else if(MTP == "rawp") {
    adjp <- rawp
  } else if(MTP %in% c("WY-SS", "WY-SD"))
  {
    if(!is.null(cl))
    {
      clusterExport(
        cl,
        list("abs.Zs.H1"),
        envir = environment()
      )
      order.matrix <- t(parallel::parApply(cl, abs.Zs.H1, 1, order, decreasing = TRUE))
      
    } else
    {
      order.matrix <- t(apply(abs.Zs.H1, 1, order, decreasing = TRUE))
    }
    
    if (MTP == "WY-SS"){
      
      adjp <- adjust.allsamps.WYSS(snum, abs.Zs.H0, abs.Zs.H1)
      
    } else if (MTP == "WY-SD"){
      
      adjp <- adjust.allsamps.WYSD(snum, abs.Zs.H0, abs.Zs.H1, order.matrix, cl)
    }
  } else {
    stop(paste("Unknown MTP:", MTP))
  }
  
  if (is.function(updateProgress) & !is.null(adjp)){
    updateProgress(message = paste("Multiple adjustments done for", MTP))
  }
  
  adjp.each <- list(rawp, adjp)
  
  # for each MTP, get matrix of indicators for whether the adjusted p-value is less than alpha
  reject <- function(x) { as.matrix(1*(x < alpha)) }
  reject.each <- lapply(adjp.each, reject)
  
  # Helper function: In each row for each MTP matrix, count number of p-values less than 0.05,
  # in rows corresponding to false nulls
  lt.alpha <- function(x) { apply(as.matrix(x[,MDES > 0]), 1, sum) }
  lt.alpha.each <- lapply(reject.each, lt.alpha)
  
  # indiv power for WY-SS, WY-SD, BH, HO, BF is mean of columns of booleans of whether adjusted pvalues were less than alpha
  # in other words, the null has been rejected
  power.ind.fun <- function(x) { apply(x, 2, mean) }
  power.ind.each <- lapply(reject.each, power.ind.fun)
  power.ind.each.mat <- do.call(rbind, power.ind.each)
  
  # 3rd call back to progress bar: Individual power calculations are done
  if (is.function(updateProgress) & !is.null(power.ind.each.mat)) {
    updateProgress(message = "Individual power calculation is done.")
  }
  
  # Helper function: m-min powers for all MTPs (including complete power when m=M)
  power.min.fun <- function(x, M) {
    power.min<-numeric(M)
    for (m in 1:M) {
      power.min[m] <- mean(x >= m)
    }
    return(power.min)
  } # end of calculating d-minimal power
  
  # calculating d-minimal power
  power.min <- lapply(lt.alpha.each, power.min.fun, M = M)
  power.min.mat <- do.call(rbind, power.min)
  power.min0 <- lapply(lt.alpha.each, function(x){ mean(x > 0)})
  power.min0 <- do.call(rbind, power.min0)
  
  # complete power is the power to detect outcomes at least as large as the MDES on all outcomes
  # separating out complete power from d-minimal power by taking the last entry
  power.cmp <- rep(power.min.mat[1,M], length(power.min)) # should it be numfalse or M?
  
  # calculating average individual power
  mean.ind.power <- apply(as.matrix(power.ind.each.mat[,MDES>0]), 1, mean)
  
  # combine all power for all definitions
  all.power.results <- cbind(power.ind.each.mat, mean.ind.power, power.min0, power.min.mat[,-M], power.cmp)
  
  # setting the col and row names for all power results table
  if(M == 1)
  {
    colnames(all.power.results) = c(paste0("D", 1:M, "indiv"), "indiv.mean", "min", "complete")
  } else
  {
    colnames(all.power.results) = c(paste0("D", 1:M, "indiv"), "indiv.mean", "min", paste0("min",1:(M-1)), "complete")
  }
  rownames(all.power.results) <- c("rawp", MTP)
  
  if (is.function(updateProgress) & !is.null(all.power.results)) {
    updateProgress(message = paste0("All definitions of power calculation are done."))
  }
  
  return(all.power.results)
  
}

#' Midpoint function
#'
#' Calculating the midpoint between the lower and upper bound by calculating half the distance between the two
#' and adding the lower bound to it. The function is a helper function in determining the MDES that falls within
#' acceptable power range.
#'
#' @param lower lower bound
#' @param upper upper bound
#' @importFrom stats dist
#' @return returns midpoint value

midpoint <- function(lower, upper) {
  return(lower + dist(c(lower, upper))[[1]]/2)
}

# extract roots from quadratic curve based on given evaluated points
find_best <- function(test.pts, mdes.low, mdes.high, target.power, alternate = NA)
{
  # fit quadratic curve
  quad.mod <- lm( power ~ 1 + mdes + I(mdes^2), data = test.pts)
  # extract point where it crosses target power
  cc <- rev( coef( quad.mod ) )
  # Using x = [ b pm sqrt( b^2 - 4a(c-y) ) ] / [2a]
  # first check if root exists
  rt.check <- cc[2]^2 - 4 * cc[1] * (cc[3] - target.power)
  
  if ( rt.check > 0 ) {
    try.mdes <- ( -cc[2] + c(-1,1) * sqrt(rt.check) ) / (2 * cc[1] )
    hits <- (mdes.low <= try.mdes) & (try.mdes <= mdes.high) 
    if ( sum( hits ) == 1 ) {
      try.mdes <- try.mdes[hits]
    } else {
      # error
      cat( "Root concerns\n" )
      try.mdes <- alternate
    }
  } else {
    cat( "No roots\n" )
    # error
    try.mdes <- alternate
  }
  return(try.mdes)
}

scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}

#' MDES (minimum detectable effect size) function
#'
#' The minimum detectable effect size function calculates the most feasible minimum detectable effect size
#' for a given MTP, power and power definition. The goal is to find the MDES value that satisfies the tolerance
#' set in the parameter in the power value.
#'
#' @param design RCT design (see list/naming convention)
#' @param MTP multiple adjustment procedures of interest such as Bonferroni, BH, Holms, WY_SS & WY_SD
#'              (we expect inputs in  such order)
#' @param M the number of hypothesis tests (outcomes)
#' @param J the number of schools
#' @param K the number of districts
#' @param power required statistical power for the experiment
#' @param power.definition definition of statistical power from individual, d-minimal to complete power
#' @param tol the tolerance for MDES estimation based on targeted power value
#' @param nbar the harmonic mean of the number of units per block
#' @param Tbar the proportion of samples that are assigned to the treatment
#' @param alpha the family wise error rate (FWER)
#' @param numCovar.1 number of Level 1 baseline covariates (not including block dummies)
#' @param numCovar.2 number of Level 2 baseline covariates (set to 0 for this design)
#' @param numCovar.3 number of Level 3 baseline covariates (set to 0 for this design)
#' @param R2.1 a vector of length M corresponding to R^2 for M outcomes of Level 1 (R^2 = variation in the data explained by the model)
#' @param R2.2 a vector of length M corresponding to R^2 for M outcomes of Level 2 (R^2 = variation in the data explained by the model)
#' @param ICC.2 school intraclass correlation
#' @param ICC.3 district intraclass correlation	
#' @param omega.2 ratio of school effect size variability to random effects variability
#' @param omega.3 ratio of district effect size variability to random effects variability
#' @param tnum the number of test statistics (samples) for all procedures other than Westfall-Young & number of permutations for WY. The default is set at 10,000
#' @param snum the number of samples for Westfall-Young. The default is set at 1,000.
#' @param cl clusters object to use for parallel processing.
#' @param rho correlation between outcomes
#' @param updateProgress the callback function to update the progress bar (User does not have to input anything)
#'              
#' @importFrom stats qt
#' @return mdes results
#' @export
#'

pump_mdes <- function(
  design, MTP, M, J, K = 1,
  target.power, power.definition, tol,
  nbar, Tbar, alpha, numCovar.1 = 0, numCovar.2 = 0,
  numCovar.3 = 0, R2.1, R2.2 = NULL, R2.3 = NULL, ICC.2, ICC.3 = NULL,
  rho, omega.2, omega.3 = NULL,
  tnum = 10000, snum = 1000,
  max.steps = 20, max.cum.tnum = 5000, start.tnum = 200, max.tnum = 10000,
  cl = NULL, updateProgress = NULL
)
{
  # set some defaults 
  # max.tnum = 10000; start.tnum = 200; max.steps = 20; max.cum.tnum = 5000
  
  sigma <- matrix(rho, M, M)
  diag(sigma) <- 1
  
  message(paste("Estimating MDES for", MTP, "for target", power.definition, "power of", round(target.power, 4)))
  
  # Check to see if the MTP is Westfall Young and it has enough samples. Otherwise, enforce the requirement.
  if (MTP == "WY-SD" & snum < 1000){
    warning(paste("For the step-down Westfall-Young procedure, it is recommended that sample (snum) be at least 1000. Current snum:", snum))
  }
  
  # Compute Q.m
  Q.m <- calc.Q.m(design, J, K, nbar, R2.1, R2.2, R2.3, ICC.2, ICC.3, omega.2, omega.3, Tbar)
  t.df <- calc.df(design, J, K, nbar, numCovar.1, numCovar.2, numCovar.3)
  
  # For raw and BF, compute critical values
  crit.alpha <- qt(p = (1-alpha/2), df = t.df)
  crit.alphaxM <- qt(p = (1-alpha/M/2), df = t.df)
  
  # Compute raw and BF MDES for individual power
  crit.beta <- ifelse(target.power > 0.5, qt(target.power, df = t.df), qt(1 - target.power, df = t.df))
  mdes.raw  <- ifelse(target.power > 0.5, Q.m * (crit.alpha + crit.beta), Q.m * (crit.alpha - crit.beta))
  mdes.bf   <- ifelse(target.power > 0.5, Q.m * (crit.alphaxM + crit.beta), Q.m * (crit.alphaxM - crit.beta))
  
  # SETTING THE MDES BOUNDS FOR INDIVIDUAL AND OTHER TYPES OF POWER from using raw and bf mdes bounds #
  
  ### raw or bonferroni ###
  if (power.definition == "D1indiv") {
    
    if (MTP == "rawp"){
      
      # Attaching the MDES result to power results for tabular output
      mdes.results <- data.frame(MTP, mdes.raw, target.power) # transpose the MDES raw and power to have the results columnwise
      colnames(mdes.results) <- c("MTP", "Adjusted MDES", paste(power.definition, "power"))
      return (list(mdes.results = mdes.results, tries = NULL))
      
    } #Raw MDES if anybody ever asked for it
    
    if (MTP == "Bonferroni"){
      
      # Attaching the MDES result to power results for tabular output
      mdes.results <- data.frame(MTP, mdes.bf, target.power) #transpose the MDES raw and power to have the results columnwise
      colnames(mdes.results) <- c("MTP", "Adjusted MDES", paste(power.definition, "power"))
      return(list(mdes.results = mdes.results, tries = NULL))
      
    } # Bonferroni adjusted MDES for Individual Power
    
  } # if we are doing power for raw (i.e. unadjusted) and Bonferroni
  
  # For individual power, other MDES's will be between MDES.raw and MDES.BF, so make starting value the midpoint!
  # MDES for MTP that is not Bonferroni and for individual powers
  
  if (MTP %in% c("Holm", "BH", "WY-SS", "WY-SD") & power.definition == "D1indiv") {
    mdes.low <- mdes.raw
    mdes.high <- mdes.bf
  } else {
    stop('MDES search only implemented for individual power.')
  }
  
  # fit initial quadratic curve
  # generate a series of points to try
  current.tnum <- start.tnum
  test.pts <- data.frame(
    step = 0,
    mdes = seq(mdes.low, mdes.high, length.out = 5),
    power = NA,
    w = current.tnum,
    MTP = MTP,
    target.power = target.power
  )
  # generate power for all these points
  for(i in 1:nrow(test.pts))
  {
    pt.power.results <- pump_power(design, MTP = MTP,
                           MDES = rep(test.pts$mdes[i], M),
                           M = M, J = J, K = K,
                           nbar = nbar, Tbar = Tbar, alpha = alpha,
                           numCovar.1 = numCovar.1, numCovar.2 = numCovar.2, numCovar.3 = numCovar.3,
                           R2.1 = R2.1, R2.2 = R2.2, R2.3 = R2.3, ICC.2 = ICC.2, ICC.3 = ICC.3,
                           rho = rho, omega.2 = omega.2, omega.3 = omega.3, 
                           tnum = tnum, snum = snum, cl = cl)
    test.pts$power[i] <- pt.power.results[MTP, power.definition]
  }
  
  current.mdes <- find_best(test.pts, mdes.low, mdes.high, target.power, alternate = midpoint(mdes.low, mdes.high))
  
  current.power <- 0
  cum.tnum <- 0
  mdes.results <- data.frame(MTP, NA, NA)
  step <- 0
  
  while( (step < max.steps) & (abs( current.power - target.power ) > tol) )
  {
    step <- step + 1
    current.tnum <- pmin(max.cum.tnum, round(current.tnum * 1.1))
    cum.tnum <- cum.tnum + current.tnum
    current.power.results <- pump_power(
      MDES = rep(current.mdes, M),
      tnum = current.tnum, snum = snum,
      design = design, MTP = MTP, M = M, J = J, K = K,
      nbar = nbar, Tbar = Tbar, alpha = alpha,
      numCovar.1 = numCovar.1, numCovar.2 = numCovar.2, numCovar.3 = numCovar.3,
      R2.1 = R2.1, R2.2 = R2.2, R2.3 = R2.3, ICC.2 = ICC.2, ICC.3 = ICC.3,
      rho = rho, omega.2 = omega.2, omega.3 = omega.3, 
      cl = cl
    )
    current.power <- current.power.results[MTP, power.definition]
    
    if(abs(current.power - target.power) < tol) {
      check.power.tnum <- pmin(10 * current.tnum, max.cum.tnum)
      check.power.results <- pump_power(
        MDES = rep(current.mdes, M),
        tnum = check.power.tnum, snum = snum,
        design = design, MTP = MTP, M = M, J = J, K = K,
        nbar = nbar, Tbar = Tbar, alpha = alpha,
        numCovar.1 = numCovar.1, numCovar.2 = numCovar.2, numCovar.3 = numCovar.3,
        R2.1 = R2.1, R2.2 = R2.2, R2.3 = R2.3, ICC.2 = ICC.2, ICC.3 = ICC.3,
        rho = rho, omega.2 = omega.2, omega.3 = omega.3, 
        cl = cl
      )
      check.power <- check.power.results[MTP, power.definition]
      
      # cum.tnum <- cum.tnum + check.power.tnum
      # TODO: replace with weighted average?
      current.power <- check.power
      mdes.results <- data.frame(MTP, current.mdes, current.power)

      # If still good, go to our final check to see if we are winners!
      # TODO: && (test_pow_R < MAX_ITER)
      if(abs(current.power - target.power) < tol)
      {
        check.power.results <- pump_power(
          MDES = rep(current.mdes, M),
          tnum = max.tnum, snum = snum,
          design = design, MTP = MTP, M = M, J = J, K = K,
          nbar = nbar, Tbar = Tbar, alpha = alpha,
          numCovar.1 = numCovar.1, numCovar.2 = numCovar.2, numCovar.3 = numCovar.3,
          R2.1 = R2.1, R2.2 = R2.2, R2.3 = R2.3, ICC.2 = ICC.2, ICC.3 = ICC.3,
          rho = rho, omega.2 = omega.2, omega.3 = omega.3, 
          cl = cl
        )
      }
    } 
    iter.results <- data.frame(
      step = step, mdes = current.mdes, power = current.power, w = current.tnum,
      MTP = MTP, target.power = target.power
    )
    test.pts <- bind_rows(test.pts, iter.results)
    
    if(current.mdes < iter.results$mdes) {
      current.mdes <- find_best(test.pts, mdes.low, mdes.high, target.poewr, alternate = current.mdes + 0.10 * (mdes.high - try.mdes))
    } else {
      current.mdes <- find_best(test.pts, mdes.low, mdes.high, target.power, alternate = current.mdes - 0.10 * (current.mdes - mdes.low) )
    }
    # scat("%d\tNew best: %.2f \n", step, current.mdes)
  }
  
  # clean up return values
  colnames(mdes.results) <- c("MTP", "Adjusted MDES", paste(power.definition, "power"))
  
  if( (cum.tnum == max.cum.tnum | step == max.steps) & abs(current.power - target.power) > tol) {
    message("Reached maximum iterations without converging on MDES estimate within tolerance.")
  }

  return(list(mdes.results = mdes.results, tries = test.pts))

  # 
  # 
  # 
  # # Searching for the right MDES through a while loop
  # ii <- 0 # Iteration counter
  # target.power <- 0 # Initializing a target power
  # 
  # # While loop through until the iteration is past max iterations or
  # # we have met the target.power as we search for the right MDES
  # # within the tolerance we have specified.
  # 
  # # save out different tries
  # mdes.tries <- try.MDES
  # power.tries <- target.power
  # 
  # while (ii < max.iter & (target.power < power - tol | target.power > power + tol)) {
  #   
  #   if (is.function(updateProgress)) {
  #     text <- paste0("Optimal MDES is currently in the interval between ",round(lowhigh[1],4)," and ",round(lowhigh[2],4),". ")
  #     msg  <- paste0("Trying MDES of ",round(try.MDES,4)," ... ")
  #     updateProgress(message = msg, detail = text)
  #   }
  #   
  #   # Function to calculate the target power to check in with the pre-specified power in the loop
  #   runpower <- pump_power(design, MTP = MTP, MDES = rep(try.MDES, M), M = M, J = J, K = K,
  #                          nbar = nbar, Tbar = Tbar, alpha = alpha,
  #                          numCovar.1 = numCovar.1, numCovar.2 = numCovar.2, numCovar.3 = numCovar.3,
  #                          R2.1 = R2.1, R2.2 = R2.2, R2.3 = R2.3, ICC.2 = ICC.2, ICC.3 = ICC.3,
  #                          rho = rho, omega.2 = omega.2, omega.3 = omega.3, 
  #                          tnum = tnum, snum = snum, cl = cl)
  # 
  #   # Pull out the power value corresponding to the MTP and definition of power
  #   target.power <- runpower[MTP, power.definition]
  #   
  #   # Displaying the progress of mdes calculation via target power
  #   if (is.function(updateProgress)) {
  #     
  #     msg <- paste("Estimated power for this MDES is", round(target.power,4)) # Text for estimating power
  #     updateProgress(message = msg)
  #     
  #   } # checking on Progress Update for MDES
  #   
  #   # save out progress
  #   mdes.tries <- c(mdes.tries, try.MDES)
  #   power.tries <- c(power.tries, target.power)
  #   
  #   # If the calculated target.power is within the tolerance of the prescribed power, break and return the results
  #   if(target.power > power - tol & target.power < power + tol){
  #     
  #     mdes.results <- data.frame(MTP, try.MDES[1], target.power)
  #     colnames(mdes.results) <- c("MTP", "Adjusted MDES", paste(power.definition, "power"))
  #     tries = data.frame(
  #       MTP = MTP, iter = seq(1, length(mdes.tries)),
  #       mdes.tries = mdes.tries, power.tries = power.tries,
  #       power.goal = power
  #     )
  #     return(list(mdes.results = mdes.results, tries = tries))
  #     
  #   } # Return results if our targeted power is within a tolerance of the specified power
  #   
  #   # Check if the calculated target power is greater than the prescribed power
  #   is.over <- target.power > power
  #   
  #   # if we are overpowered, we can detect EVEN SMALLER effect size so we would shrink the effect range with the
  #   # high end of the bound being the current MDES. Else it would be the opposite.
  #   
  #   if(!is.over) {
  #     lowhigh[1] <- try.MDES
  #   }
  #   if(is.over) {
  #     lowhigh[2] <- try.MDES
  #   }
  #   
  #   # re-establish the midpoint and increase iteration
  #   try.MDES <- midpoint(lowhigh[1],lowhigh[2])
  #   ii <- ii + 1
  #   
  # } # end while
  # 
  # if (ii == max.iter & !(target.power > power - tol & target.power < power + tol)) {
  #   message("Reached maximum iterations without converging on MDES estimate within tolerance.")
  # }
  # mdes.results <- data.frame(MTP, NA, NA)
  # colnames(mdes.results) <- c("MTP", "Adjusted MDES", paste(power.definition, "power"))
  # tries = data.frame(
  #   MTP = MTP, iter = seq(1, length(mdes.tries)),
  #   mdes.tries = mdes.tries, power.tries = power.tries,
  #   power.goal = power
  # )
  # return(list(mdes.results = mdes.results, tries = tries))
  # 
  # 
} # MDES blockedRCT 2

#' Calculating Sample for Raw (Unadjusted)
#'
#' This is a Helper function for getting Sample Size when no adjustments has been made to the test statistics.
#' The function starts with PowerUp package function mrss.bira2cl but that function seems to have a bug -
#' Only works if we pass in numeric values and not if we pass in objects that hold those values.
#' Additionally, mrss.bira2cl only computes J, not nbar.
#'
#' @param J the number of blocks
#' @param nbar the harmonic mean of the number of units per block
#' @param J0 starting values for J0 to look for optimal J and nbar
#' @param nbar0 starting values for nbar0 to look for optimal J and nbar
#' @param whichSS which type of sample size to optimize for. J or nbar
#' @param MDES minimum detectable effect size
#' @param power required statistical power for the experiment
#' @param Tbar the proportion of samples that are assigned to the treatment
#' @param alpha the family wise error rate (FWER)
#' @param numCovar.1 number of Level 1 baseline covariates (not including block dummies)
#' @param numCovar.2 number of Level 2 baseline covariates (set to 0 for this design)
#' @param R2.1 a vector of length M corresponding to R^2 for M outcomes of Level 1 (R^2 = variation in the data explained by the model)
#' @param R2.2 a vector of length M corresponding to R^2 for M outcomes of Level 2 (R^2 = variation in the data explained by the model)
#' @param ICC intraclass correlation
#' @param omega NULL (parameter in development)
#' @param two.tailed a boolean value for whether we are looking at two-tailed distribution or not
#' @param max.iter number of iterations to look for sample size. The default is set at 100
#' @param tol tolerance from initializing sample values
#' @return raw sample returns
#' @export

sample_blocked_i1_2c_raw <- function(J, nbar, J0 = 10, nbar0 = 10,
                                     whichSS, MDES, power, Tbar,
                                     alpha, numCovar.1, numCovar.2 = 0,
                                     R2.1, R2.2, ICC,
                                     omega, two.tailed = TRUE, max.iter = 100, tol = 0.1) {
  
  
  i <- 0 # starting the iterator
  conv <- FALSE # boolean value for convergence
  
  while (i <= max.iter & conv == FALSE) {
    # checking which type of sample we are estimating
    if (whichSS =="J"){
      df <- J0 * (nbar - 1) - numCovar.1 - 1 # degree of freedom calculation
    }
    if (whichSS =="nbar") {
      df <- J * (nbar0 - 1) - numCovar.1 - 1
    }
    
    if (df < 0 | is.infinite(df)) {
      break
    }
    
    T1 <- ifelse(two.tailed == TRUE, abs(qt(alpha/2, df)),
                 abs(qt(alpha, df)))
    
    T2 <- abs(qt(power, df))
    
    MT <- ifelse(power >= 0.5, T1 + T2, T1 - T2)
    
    if (whichSS=="J") {
      
      J1 <- (MT/MDES)^2 * ((1 - R2.1)/(Tbar * (1 - Tbar) * nbar))
      
      if (abs(J1 - J0) < tol) {
        
        conv <- TRUE
      }
      J0 <- (J1 + J0)/2
    }
    if (whichSS=="nbar") {
      nbar1 <- (MT/MDES)^2 * ((1 - R2.1)/(Tbar * (1 - Tbar) * J))
      
      if (abs(nbar1 - nbar0) < tol) {
        conv <- TRUE
      }
      nbar0 <- (nbar1 + nbar0)/2
    }
    
    i <- i + 1
  }
  if (whichSS=="J") {
    J <- ifelse(df > 0, round(J0), NA)
    return(ceiling(J))
  }
  if (whichSS=="nbar") {
    nbar <- ifelse(df > 0, round(nbar0), NA)
    return(ceiling(nbar))
  }
  
}

#These currently only work if numFalse = M and if MDES is the same or all outcomes.

#' Sample Function
#'
#' @param M the number of hypothesis tests (outcomes)
#' @param typesample the type of the number of sample we would like to estimate: either block J or nbar (harmonic mean within block. For Shiny use)
#' @param J the number of blocks (set to NULL if you do not want to estimate this one)
#' @param nbar the harmonic mean of blocks (set to NULL if you do not want to estimate this one)
#' @param J0 the initial value for the sample number of blocks. The default is set at 10.
#' @param nbar0 the initial value for the harmonic mean for the number of samples within block. The default is set at 10.
#' @param MDES minimum detectable effet size
#' @param power required statistical power for the experiment
#' @param power.definition definition of statistical power from individual, d-minimal to complete power
#' @param MTP type of multiple testing procedure in use from Bonferroni, Benjamini-Hocheberg, Holms, Westfall-Young Single Step, Westfall-Young Step Down
#' @param tol the tolerance for MDES estimation based on targeted power value
#' @param Tbar the proportion of samples that are assigned to the treatment
#' @param alpha the family wise error rate (FWER)
#' @param numCovar.1 number of Level 1 baseline covariates (not including block dummies)
#' @param numCovar.2 number of Level 2 baseline covariates (set to 0 for this design)
#' @param R2.1 a vector of length M corresponding to R^2 for M outcomes of Level 1 (R^2 = variation in the data explained by the model)
#' @param R2.2 a vector of length M corresponding to R^2 for M outcomes of Level 2 (R^2 = variation in the data explained by the model)
#' @param ICC intraclass correlation
#' @param omega NULL (parameter in development)
#' @param tnum the number of test statistics (samples) for all procedures other than Westfall-Young & number of permutations for WY. The default is set at 10,000.
#' @param snum the number of samples for Westfall-Young. The default is set at 1,000.
#' @param cl cluster object to use for parallel processing.
#' @param max.iter the number of iterations to look for the optimal sample size. The default is set at 20
#' @param updateProgress a call back function for our internal use in our Shiny application
#' @param rho correlation between outcomes when sigma is generated
#'
#' @return Sample number returns
#' @export

sample_blocked_i1_2c <- function(M, typesample, J, nbar,
                                 J0 = 10, nbar0 = 10, MDES, power, power.definition,
                                 MTP, tol, Tbar, alpha, numCovar.1,
                                 numCovar.2 = 0, R2.1, R2.2, ICC,
                                 rho = 0.99, omega, tnum = 10000,
                                 snum = 2, cl = NULL,
                                 max.iter = 20, updateProgress = NULL) {
  
  # Checks on what we are estimating, sample size
  print(paste("Estimating sample size of type", typesample, "for", MTP, "for target", power.definition, "power of", round(power, 4)))
  
  # indicator for which sample to compute. J is for blocks. nbar is for harmonic mean of samples within block
  if(typesample == "J"){
    
    doJ <- TRUE
    donbar <- FALSE
    J = NULL
    nbar0 = NULL
    
  } else if (typesample == "nbar") {
    
    donbar <- TRUE
    doJ <- FALSE
    nbar = NULL
    J0 = NULL
    
  } # Sample within block
  
  ifelse(doJ,whichSS<-"J",whichSS<-"nbar")
  
  # Progress Message for the Type of Sample we are estimating, the type of power and the targeted power value
  if(is.function(updateProgress)){
    msg <- (paste("Estimating", whichSS, "for target", power.definition, "power of",round(power,4))) #msg to be displayed in the progress bar
    updateProgress(message = msg)
  } # For printing via update progress function
  
  
  # Compute J or nbar for raw and BF SS for INDIVIDUAL POWER. We are estimating bounds like we estimated MDES bounds.
  # for now assuming only two tailed tests
  if (doJ) {
    J.raw <- sample_blocked_i1_2c_raw(
      J = J, nbar, J0 = J0, nbar0 = nbar0, whichSS, MDES, power, Tbar, alpha, numCovar.1, numCovar.2 = 0,
      R2.1, R2.2, ICC, omega, max.iter = 100, tol = 0.1
    )
    J.BF <- sample_blocked_i1_2c_raw(
      J = J, nbar, J0 = J0, nbar0 = nbar0, whichSS, MDES, power, Tbar, alpha/M, numCovar.1, numCovar.2 = 0,
      R2.1, R2.2, ICC, omega, max.iter = 100, tol = 0.1
    )
  }
  
  if (donbar) {
    nbar.raw <- sample_blocked_i1_2c_raw(
      J, nbar = nbar, J0 = J0, nbar0 = nbar0, whichSS, MDES, power, Tbar, alpha, numCovar.1, numCovar.2 = 0,
      R2.1, R2.2, ICC, omega, max.iter = 100, tol = 0.1
    )
    nbar.BF <- sample_blocked_i1_2c_raw(
      J, nbar = nbar, J0 = J0, nbar0 = nbar0, whichSS, MDES, power, Tbar, alpha/M, numCovar.1, numCovar.2 = 0,
      R2.1, R2.2, ICC, omega, max.iter = 100, tol = 0.1
    )
  }
  
  # So below we focus on just one type of sample being estimated: Either block or samples within block
  if (doJ) {
    ss.raw <- J.raw
    ss.BF <- J.BF
  }
  
  if (donbar) {
    ss.raw <- nbar.raw
    ss.BF <- nbar.BF
  }
  
  ### INDIVIDUAL POWER for Raw and BF ###
  if (power.definition == "indiv") {
    
    if (MTP == "rawp"){
      
      # saving the sample estimates for Individual Power with the MTP type
      raw.ss <- data.frame(MTP, power.definition, ss.raw, power)
      colnames(raw.ss) <- c("MTP", "Type of Power", "Sample Size", "Target Power")
      
      # To check if the function is returning a table
      
      return(raw.ss)
      
    } #MTP raw
    
    if (MTP == "Bonferroni") {
      
      # saving the sample estimates for Individual Power with the MTP type
      ss.BF <- data.frame("Bonferroni", power.definition, ss.BF, power)
      colnames(ss.BF) <- c("MTP", "Type of Power", "Sample Size", "Target Power")
      
      return(ss.BF)
      
    } #MTP BF
    
  } # Individual power
  
  ### INDIVIDUAL POWER FOR NON BF MTPs ###
  
  # Like the MDES calculation, the sample size would be between raw and Bonferroni. There is no adjustment and there is very
  # conservative adjustment
  
  # For individual power, other J's or nbar's will be between raw and BF, so make starting value the midpoint
  if (MTP %in% c("Holm","BH","WY-SS","WY-SD") & power.definition == "indiv") {
    
    lowhigh <- c(ss.raw,ss.BF)
    try.ss <- midpoint(lowhigh[1],lowhigh[2])
    
  }
  # For minimal powers, makes starting value = raw
  if (power.definition != "indiv")  {
    
    lowhigh <- c(0,ss.BF)
    try.ss <- midpoint(lowhigh[1],lowhigh[2])
    
  }
  ii <- 0
  target.power <- 0
  while (ii < max.iter & (target.power < power - tol | target.power > power + tol) ) {
    
    if (is.function(updateProgress)) {
      
      #if statement to get the more apprpriate sample text for progress bar
      if (whichSS == "J"){
        typeofsample <- "Number of blocks"
      } else {
        typeofsample <- "Samples within blocks"
      }
      
      # Updating the progress of the search for sample
      text <- paste0(typeofsample, " is in the interval between ",round(lowhigh[1],4)," and ",round(lowhigh[2],4),".")
      msg <- paste0("Trying ",typeofsample," of ",round(try.ss,4), " .")
      updateProgress(message = msg, detail = text)
      
    }
    
    # We check the Power of the experimental set up with the given estimated sample size to see if it is
    # within the targeted power
    
    if (doJ) {
      
      runpower <- pump_power(M = M, MDES = rep(MDES, M), MTP = MTP, J = try.ss, nbar = nbar,
                             Tbar = Tbar, alpha = alpha, numCovar.1 = numCovar.1, numCovar.2 = 0,
                             R2.1 = R2.1, R2.2 = R2.2, ICC = ICC,
                             rho = rho, omega = omega,
                             tnum = tnum, snum = snum, cl = cl)
    }
    
    if (donbar) {
      
      runpower <- pump_power(M, MDES = rep(MDES, M), MTP = MTP, J = J, nbar = try.ss,
                             Tbar = Tbar, alpha = alpha, numCovar.1 = numCovar.1, numCovar.2 = 0,
                             R2.1 = R2.1, R2.2 = R2.2, ICC = ICC,
                             rho = rho, omega = omega,
                             tnum = tnum, snum = snum, cl = cl)
      
    }
    
    # Pulling the power result out from the table
    target.power <- runpower[MTP,power.definition]
    
    # Providing message on current targeted power
    if (is.function(updateProgress)){
      text <- paste("Estimated power for this", whichSS, "is", target.power)
      updateProgress(detail = text)
    }
    
    # checking if the estimation is over or not
    is.over <- target.power > power
    
    # if the target power is within the tolerance we have set, we will return the estimated sample
    if(target.power > power - tol & target.power < power + tol) {
      
      # estimated sample for a given MTP, type of power
      try.ss.numeric <- ceiling(as.numeric(try.ss))
      
      # The estimated sample table with MTP type, Power, Sample Size and the target power
      est.sample <- data.frame(MTP, power.definition, try.ss.numeric, target.power)
      colnames(est.sample) <- c("MTP", "Type of Power", "Sample Size", "Target Power")
      
      return(est.sample)
      
    }
    
    # if the test is underpowered or overpowered, repeat the loop with new bounds as below.
    
    if(!is.over) {
      p.off <- (power - target.power) / power
      lowhigh[1] <- try.ss
    }
    
    if(is.over) {
      lowhigh[2] <- try.ss
      p.off <- (target.power - power) / power
    }
    
    lowhigh.dist <- lowhigh[2]-lowhigh[1]
    try.ss <- ifelse(target.power < power, (try.ss + lowhigh[2])/2, (try.ss + lowhigh[1])/2) # midpoint
    ii <- ii + 1
  } # end while
  
  if (ii == max.iter & !(target.power > power - tol & target.power < power + tol)) {
    
    text <- paste0(
      "Reached maximum iterations without converging on MDES estimate within tolerance. Try increasing maximum number of iterations (max.iter)."
    )
    updateProgress(detail = text)
  }
}