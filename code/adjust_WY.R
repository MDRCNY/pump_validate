
#' Adjust.WY: does WY adjustments for a single sample in parallel
#'
#' @param data A list of length M. Each element in the list is a data frame holding the m'th data set
#' @param rawp Vector of length M of the raw test statistics
#' @param S.id vector of school assignments
#' @param D.id vector of district assignments
#' @param d_m RCT d_m (see list/naming convention)
#' @param proc Single-Step (WY-SS) or Step-Down (WY-SD)
#' @param sim.params.list simulation parameters
#' @param cl clusters for parallel processing
#'
#' @return
#' @export
#'
#' @examples
#'
adjust_WY <- function(dat.all, rawp, S.id, D.id,
                      d_m, proc,
                      sim.params.list,
                      dgp.params.list,
                      cl = NULL) {

  # cl = NULL;
  B <- sim.params.list[['B']]
  Tbar <- sim.params.list[['Tbar']]
  nbar <- dgp.params.list[['nbar']]
  
  if(startsWith(d_m, 'd1.1'))
  {
    permT <- sapply(1:B, function(x) { randomizr::simple_ra(N = nbar, prob = Tbar) })
  } else if(startsWith(d_m, 'd2.1') | startsWith(d_m, 'd3.1'))
  {
    permT <- sapply(1:B, function(x) { randomizr::block_ra(blocks = S.id, prob = Tbar) })
  } else if(startsWith(d_m, 'd2.2'))
  { 
    permT <- sapply(1:B, function(x) { randomizr::cluster_ra(clusters = S.id, prob = Tbar) })
  } else if(startsWith(d_m, 'd3.3'))
  {
    permT <- sapply(1:B, function(x) { randomizr::cluster_ra(clusters = D.id, prob = Tbar) })
  } else if(startsWith(d_m, 'd3.2'))
  {
    permT <- sapply(1:B, function(x) { randomizr::block_and_cluster_ra( blocks = D.id, clusters = S.id, prob = Tbar ) })
  } else
  {
    stop(print(paste('d_m', d_m, 'not implemented yet')))
  }
  
  # generate null test-statistics
  if(!is.null(cl))
  {
    clusterExport(cl, list(
      "perm.regs", "make_model", "get_pval_tstat", "calc_df",
      "lmer", "interacted_linear_estimators", "isSingular"
    ), envir = environment())
    
    nullp <- t(parallel::parApply(
      cl, permT, 2, perm.regs, dat.all = dat.all,
      d_m = d_m, dgp.params.list = dgp.params.list
    ))
  } else
  {
    nullp <- t(apply(
      permT, 2, perm.regs, dat.all = dat.all,
      d_m = d_m, dgp.params.list = dgp.params.list
    ))
  }
  
  # now calculate WY p-values
  rawp.order <- order(rawp, decreasing = FALSE)
  if (proc == 'WY-SS') {
    ind.B <- t(apply(nullp, 1, comp_rawp_ss, rawp))
    adjp <- colMeans(ind.B)
  } else if (proc == 'WY-SD') {
    ind.B <- t(apply(nullp, 1, comp_rawp_sd, rawp, rawp.order))
    adjp <- get_adjp_minp(ind.B, rawp.order)
  }
  
  return(adjp)
}

#' Performs regression with permuted treatment indicator
#'
#' @param permT.vec matrix with nbar * J rows and B columns, contains all permutations of treatment indicator
#' @param dat.all data for all M domains
#' @param d_m the particular RCT d_m for an experiment: "Blocked_i1_2c", "Blocked_i1_2f", "Blocked_i1_2r","Simple_c2_2r"
#'
#' @return
#' @export
#'
#' @examples
perm.regs <- function(permT.vec, dat.all, d_m, dgp.params.list) {
  # permT.vec <- permT[,1];

  M <- length(dat.all)
  out <- numeric(M)
  for (m in 1:M) {
    dat.m <- dat.all[[m]]
    dat.m$T.x <- permT.vec
    mod <- make_model(dat.m, d_m)[['mod']]
    pval.tstat <- get_pval_tstat(mod, d_m, dgp.params.list)
    out[m] <- pval.tstat[['pval']]
  }
  return(out)
}


#' Helper function for Westfall Young Single Step
#'
#' Used for the Westfall-Young single-step multiple
#' testing procedure (MTP).
#' It compares whether any of the null values across outcomes
#' exceeds each raw value for each outcome
#'
#' @param nullp a vector of p values under H0
#' @param rawp a vector of raw p values under H1
#'
#' @return returns a vector of 1s and 0s with length of M outcomes
#' @keywords internal
comp_rawp_ss <- function(nullp, rawp) {
  M <- length(nullp)
  minp <- rep(NA, M)
  for (h in 1:M) {
    minp[h] <- min( nullp ) < rawp[h]
  }
  return(as.integer(minp))
}

#' Helper function for Westfall Young Step Down
#'
#' @param nullp a vector of p-values under H0
#' @param rawp a vector of p-values under H1
#' @param rawp.order order vector of raw p-values in ascending order
#'
#' @return returns a vector of 1s and 0s with lengths of M outcomes
#' @keywords internal
comp_rawp_sd <- function(nullp, rawp, rawp.order) {
  
  M <- length(nullp)
  
  # ordered version of raw and null values
  rawp.ordered <- rawp[rawp.order]
  nullp.ordered <- nullp[rawp.order]
  
  # compute successive minima
  qstar <- rep(NA, M)
  qstar[M] <- nullp.ordered[M]
  for (h in (M-1):1)
  {
    qstar[h] <- min(qstar[h + 1], nullp.ordered[h])
  }
  
  # calculate adjusted p-value
  minp <- rep(NA, M)
  for (h in 1:M) {
    minp[h] <- qstar[h] < rawp.ordered[h]
  }
  
  return(as.integer(minp))
}

#' Helper function for Westfall Young
#'
#' enforces monotonicity in p-values.
#'
#' @param ind.B matrix of indicator variables for 
#' if each raw test statistic exceeds
#' the null test statistics. 
#' dimensions: nrow = tnum, ncol = M.
#' @param rawp.order order of raw p-values in ascending order
#'
#' @return returns adjusted p-value matrix
#' @keywords internal
get_adjp_minp <- function(ind.B, rawp.order)
{
  # take means of dummies, these are already ordered but 
  # still need to enforce monotonicity
  pi.p.m <- colMeans(ind.B)
  
  # enforce monotonicity (keep everything in same order 
  # as sorted RAW pvalues from original data)
  adjp.minp <- rep(NA, length(pi.p.m))
  adjp.minp[1] <- pi.p.m[1]
  for (h in 2:length(pi.p.m)) {
    adjp.minp[h] <- max(pi.p.m[h], adjp.minp[h-1])
  }
  
  # return back in original, non-ordered form
  out <- data.frame(adjp = adjp.minp, rawp.order = rawp.order)
  out.oo <- out$adjp[order(out$rawp.order)]
  
  return(out.oo)
}

#' Westfall-Young Single Step Adjustment Function
#'
#' This adjustment function utilizes the comp_rawp_ss 
#' helper function to compare
#' each row of the matrix sample p-values under
#' alternative hypothesis to all the rows in the matrix of the p-values
#' under the complete null.
#'
#' @param rawp.mat a matrix of raw p-values under H1. 
#' dimension: nrow = tnum, ncol = M
#' @param B numer of WY permutations
#' @param Sigma correlation matrix of null p-values
#' @param t.df degrees of freedom of null p-values
#' @param two.tailed one or two-tailed test
#' @param verbose whether to print out messaging
#' @param updateProgress function to update progress bar 
#' (only used for PUMP shiny app)
#'
#' @return a matrix of adjusted p-values
#' @keywords internal
adjp_wyss <- function(rawp.mat, B, Sigma, t.df, two.tailed,
                      verbose = TRUE, updateProgress = NULL) {
  
  # creating the matrix to store the adjusted test values
  M <- ncol(rawp.mat)
  tnum <- nrow(rawp.mat)
  adjp <- matrix(NA, nrow = tnum, ncol = M)
  
  # looping through all the samples of raw test statistics
  for (t in 1:tnum) {
    
    if(t == 1){ start.time <- Sys.time() }
    
    # generate null t values and p values
    nullt.mat <- mvtnorm::rmvt(B, sigma = Sigma, df = t.df)
    nullp.mat <- calc_pval(nullt.mat, t.df, two.tailed)
    
    # compare the distribution of test statistics
    # under H0 with 1 sample of the raw statistics under H1
    ind.B <- t(apply(nullp.mat, 1, comp_rawp_ss, rawp = rawp.mat[t,]))
    
    # calculating the p-value for each sample
    adjp[t,] <- colMeans(ind.B)
    
    if(t == 10)
    {
      end.time <- Sys.time()
      iter.time <- difftime(end.time, start.time, 'secs')[[1]]/10
      finish.time <- round((iter.time * tnum)/60)
      msg <- paste('Estimated time to finish', tnum,
                   'WY iterations with B =', B, ':',
                   finish.time, 'minutes')
      if(verbose)
      {
        message(msg)
      }
      if (is.function(updateProgress))
      {
        updateProgress(message = msg)
      }
    }
  }
  return(adjp)
}

#' Westfall Young Step Down Function
#'
#' This adjustment function utilizes the comp_rawp_ss helper 
#' function to compare each row of the matrix sample p-values under
#' alternative hypothesis to all the rows in the matrix of the p-values
#' under the complete null.
#'
#' @inheritParams adjp_wyss
#' @param cl cluster object for parallel computing
#'
#' @return a matrix of adjusted p-values
#' @keywords internal
adjp_wysd <- function(rawp.mat, B, Sigma, t.df, 
                      two.tailed, cl = NULL,
                      verbose = TRUE, 
                      updateProgress = NULL) {
  
  # creating the matrix to store the adjusted test values
  M <- ncol(rawp.mat)
  tnum <- nrow(rawp.mat)
  adjp <- matrix(NA, nrow = tnum, ncol = M)
  
  if(!is.null(cl))
  {
    parallel::clusterExport(
      cl,
      list("rawp.mat"),
      envir = environment()
    )
    rawp.order.matrix <- 
      t(parallel::parApply(cl, rawp.mat, 1, order, decreasing = FALSE))
  } else
  {
    rawp.order.matrix <- 
      t(apply(rawp.mat, 1, order, decreasing = FALSE))
  }
  
  # looping through all the samples of raw test statistics
  for (t in 1:tnum) {
    if(t == 1){ start.time <- Sys.time() }
    
    # generate null t statistics and pvalues
    nullt.mat <- mvtnorm::rmvt(B, sigma = Sigma, df = t.df)
    nullp.mat <- calc_pval(nullt.mat, t.df, two.tailed)
    
    # compare to raw statistics
    ind.B <- t(apply(nullp.mat, 1, 
                     comp_rawp_sd, rawp = rawp.mat[t,], rawp.order = rawp.order.matrix[t,]))
    
    # calculate adjusted p value
    adjp[t,] <- get_adjp_minp(ind.B, rawp.order.matrix[t,])
    
    if(t == 10)
    {
      end.time <- Sys.time()
      iter.time <- difftime(end.time, start.time, 'secs')[[1]]/10
      finish.time <- round((iter.time * tnum)/60)
      msg <- paste('Estimated time to finish', tnum,
                   'WY iterations with B =', B, ':',
                   finish.time, 'minutes')
      if(verbose)
      {
        message(msg)
      }
      if (is.function(updateProgress))
      {
        updateProgress(message = msg)
      }
    }
  }
  return(adjp)
}
