#' Helper function for Westfall Young Single Step
#'
#' The  function  comp.rawt.SS is  needed  to  implement  the  Westfall-Young single-step multiple
#' testing procedure (MTP). It operates on one row of null test statistics.
#'
#' @param abs.Zs.H0.1row A vector of permuted test statistics values under H0
#' @param abs.Zs.H1.1samp One sample of raw statistics
#'
#' @return returns a vector of 1s and 0s with length of M outcomes
#'
#'
comp.rawt.ss <- function(nullt, rawt, rawt.order) {
  M <- length(nullt)
  maxt <- rep(NA, M)
  for (h in 1:M) {
    maxt[h] <- max(abs(nullt)) > abs(rawt)[rawt.order][h]
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
comp.rawt.sd <- function(nullt, rawt, rawt.order) {
  M <- length(nullt)
  maxt <- rep(NA, M)
  maxt[1] <- max(abs(nullt)) > abs(rawt)[rawt.order][1]
  for (h in 2:M) {
    maxt[h] <- max(abs(nullt)[rawt.order][-(1:(h-1))]) > abs(rawt)[rawt.order][h]
  }
  return(as.integer(maxt))
}

# enforce monotonicity in p-values
get.adjp.minp <- function(nullt, rawt, rawt.order)
{
  # abs.Zs.H1.row = abs.Zs.H1[1,]; oo = order.matrix[1,];
  
  # using apply to compare the distribution of test statistics under H0 with 1 sample of the raw statistics under H1
  ind.B <- apply(nullt, 1, comp.rawt.sd, rawt, rawt.order)
  
  # take means of dummies, these are already ordered (by r.m.r) but still need to enforce monotonicity
  pi.p.m <- rowMeans(ind.B)
  
  # enforce monotonicity (keep everything in same order as sorted RAW pvalues from original data)
  adjp.minp <- rep(NA, length(pi.p.m))
  adjp.minp[1] <- pi.p.m[1]
  for (h in 2:length(pi.p.m)) {
    adjp.minp[h] <- max(pi.p.m[h], adjp.minp[h-1])
  }
  
  # return back in original, non-ordered form
  out <- cbind(adjp.minp, rawt.order)
  colnames(out) <- c("WY", "test.num")
  out.oo <- out[order(out[, 'test.num']),"WY"]
  
  return(out.oo)
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

adjust.allsamps.WYSS <- function(snum, nullt, rawt, rawt.order) {
  
  # creating the matrix to store the adjusted test values with the number of samples &
  # number of M outcomes
  adjp.WY <- matrix(NA, snum, ncol(nullt))
  # looping through all the samples of raw test statistics under the alternative hypothesis
  doWY <- for (s in 1:snum) {
    
    # using apply to compare the distribution of test statistics under H0 with 1 sample of the raw statistics under H1
    ind.B <- t(apply(nullt, 1, comp.rawt.ss, rawt = rawt[s,]))
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

adjust.allsamps.WYSD <- function(snum, nullt, rawt.matrix, rawt.order.matrix, cl = NULL) {
  
  # getting M number of outcomes vector
  M <- ncol(nullt)
  # setting up the matrix to save the adjusted p values
  adjp.WY <- matrix(NA, snum, M)
  
  if(!is.null(cl))
  {
    # leveraging snow to run multiple cores for foreach loops
    doParallel::registerDoParallel(cl)
    # registering the comp.rawt.SD function in global enivronment of each node
    parallel::clusterExport(cl = cl, list('comp.rawt.sd', 'get.adjp.minp'), envir = environment())
    
    # dopar is a special function that has to be explicitly called from the foreach package
    # dopar accepts only 2 parameters. The number of times to execute the parallelization and the
    # series of steps to execute
    # `%dopar%` <- foreach::`%dopar%`
    # making s a local variable to perpetuate across (created to bypass a package requirement)
    # s = 1:snum
    doWY <- foreach::foreach(s = 1:snum, .combine = rbind) %dopar% {
      adjp.WY[s,] <- get.adjp.minp(nullt = nullt, rawt = rawt.matrix[s,], rawt.order = rawt.order.matrix[s,])
    }
  } else
  {
    doWY <- foreach::foreach(s = 1:snum, .combine = rbind) %do% {
      adjp.WY[s,] <- get.adjp.minp(nullt = nullt, rawt = rawt.matrix[s,], rawt.order = rawt.order.matrix[s,])
    }
  }
  
  return(doWY)
}