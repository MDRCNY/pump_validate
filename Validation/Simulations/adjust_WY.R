# This script contains functions for carrying out Westfall-Young step-down corrections
# Last - updated March 7, 2019, Kristin Porter
# Last - edited March 23, 2020, Zarni Htet
# Last - edited July 29, 2020, Kristen Hunter


#' Adjust.WY: does WY adjustments for a single sample in parallel
#'
#' @param data A list of length M. Each element in the list is a data frame holding the m'th data set
#' @param rawp Vector of length M of the raw p-values
#' @param rawt Vector of lenght M of the raw test statistics
#' @param design RCT design (see list/naming convention)
#' @param proc Single-Step (WY-SS) or Step-Down (WY-SD)
#' @param sim.params.list simulation parameters
#' @param model.params.list model parameters
#' @param cl clusters for parallel processing
#'
#' @return
#' @export
#'
#' @examples
#'
adjust_WY <- function(dat.all, rawp, rawt, S.id, D.id,
                      design, proc,
                      sim.params.list, model.params.list,
                      cl = NULL) {
  
  # cl = NULL;
  
  B <- sim.params.list[['B']]
  maxT <- sim.params.list[['maxT']]
  Tbar <- sim.params.list[['Tbar']]
  
  # get ordering of raw p-values
  r.m.r <- order(rawp, decreasing = FALSE)
  
  # blocked designs
  if(design %in% c('blocked_i1_2c', 'blocked_i1_2f', 'blocked_i1_2r', 'blocked_i1_3r')) {
    permT <- sapply(1:B, function(x) { randomizr::block_ra(blocks = S.id, prob = Tbar) })
  # cluster designs
  } else if(design %in% c('simple_c2_2r'))  { 
    permT <- sapply(1:B, function(x) { randomizr::cluster_ra(clusters = S.id, prob = Tbar) })
  } else if(design %in% c('simple_c3_3r'))  {
    permT <- sapply(1:B, function(x) { randomizr::cluster_ra(clusters = D.id, prob = Tbar) })
  # blocked cluster designs
  } else if(design %in% c('blocked_c2_3f', 'blocked_c2_3r'))  {
    permT <- sapply(1:B, function(x) { randomizr::block_and_cluster_ra( blocks = D.id, clusters = S.id, prob = Tbar ) })
  } else
  {
    stop(print(paste('Design', design, 'not implemented yet')))
  }
  
  if(!is.null(cl))
  {
    clusterExport(cl, list(
      "perm.regs", "make.model", "get.tstat", "get.pval",
      "lmer", "interacted_linear_estimators"
    ), envir = environment())
    
    # get null p-values (if maxT=FALSE) or test-statistics (if maxT=TRUE) using permuted T's
    nullpt <- t(parallel::parApply(
      cl, permT, 2, perm.regs, dat.all = dat.all, maxT = maxT,
      design = design, user.params.list = user.params.list
    ))
  } else
  {
    nullpt <- t(apply(
      permT, 2, perm.regs, dat.all = dat.all, maxT = maxT,
      design = design, user.params.list = user.params.list
    ))
  }
  
  # create dummies for comparisons of null p-values to raw p-values
  if (maxT == FALSE & proc == 'WY-SD') {
    ind.B <- apply(nullpt, 1, comp.rawp.sd, rawp, r.m.r)
  } else if (maxT == FALSE & proc == 'WY-SS') {
    ind.B <- apply(nullpt, 1, comp.rawp.ss, rawp, r.m.r)
  } else if (maxT == TRUE & proc == 'WY-SD') {
    ind.B <- apply(nullpt, 1, comp.rawt.sd, rawt, r.m.r)
  } else if (maxT == TRUE & proc == 'WY-SS') {
    ind.B <- apply(nullpt, 1, comp.rawt.ss, rawt, r.m.r)
  }
  
  # take means of dummies, these are already ordered (by r.m.r) but still need to enforce monotonicity
  pi.p.m <- rowMeans(ind.B)
  
  # enforce monotonicity (keep everything in same order as sorted RAW pvalues from original data)
  adjp.minp <- rep(NA, user.params.list[['M']])
  adjp.minp[1] <- pi.p.m[1]
  for (h in 2:length(pi.p.m)) {
    adjp.minp[h] <- max(pi.p.m[h], adjp.minp[h-1])
  }
  
  # return back in original, non-ordered form
  out <- cbind(rawp[r.m.r], adjp.minp, r.m.r)
  colnames(out) <- c("rawp", "WY", "test.num")
  out.oo <- out[order(out[, 'test.num']),]
  return(out.oo)
}

#' Performs regression with permuted treatment indicator
#'
#' @param permT matrix with nbar * J rows and B columns, contains all permutations of treatment indicator
#' @param data data for all M domains
#' @param design the particular RCT design for an experiment: "Blocked_i1_2c", "Blocked_i1_2f", "Blocked_i1_2r","Simple_c2_2r"
#' @param S.id blocking variable
#' @param maxT TRUE if using maxT procedures
#' @param nbar individuals per block (assume same for all)
#' @param J number of blocks
#'
#' @return
#' @export
#'
#' @examples
perm.regs <- function(permT.vec, dat.all, design, maxT, user.params.list) {
  # permT.vec <- permT[,1];

  M <- length(dat.all)
  out <- numeric(M)
  for (m in 1:M) {
    dat.m <- dat.all[[m]]
    dat.m$T.x <- permT.vec
    mod <- make.model(dat.m, design)
    pval.tstat <- get.pval.tstat(mod, design, user.params.list)
    if(maxT)
    {
      out[m] <- pval.tstat[['tstat']]
    } else
    {
      out[m] <- pval.tstat[['pval']]
    }
  }
  return(out)
}

#' Functions to compare nullp distributions with raw distributions
#' comp.rawp.sd: makes rawp comparisons for step-down procedure
#' comp.rawp.ss: makes rawp comparisons for single-step procedure
#' comp.rawt.sd: makes rawt comparisons for step-down procedure
#' comp.rawt.ss: makes rawt comparisons for single-step procedure
#'
#' The length of nullprow, rawp, and r.m.r are the same. This function is performed on each row of nullp.mat
#'
#' @param nullprow OR null trow row of p-values or t stats
#' @param rawp OR rawt raw p-values or t stats
#' @param r.m.r vector of indices of rawp or rawt in order
#'
#' @return
#' @export
#'
#' @examples

comp.rawp.sd <- function(nullprow, rawp, r.m.r) {
  num.test <- length(nullprow)
  minp <- rep(NA, num.test)
  minp[1] <- min(nullprow) < rawp[r.m.r][1]
  for (h in 2:num.test) {
    minp[h] <- min(nullprow[r.m.r][-(1:(h-1))]) < rawp[r.m.r][h]
  }
  return(as.integer(minp))
}

comp.rawp.ss <- function(nullprow, rawp, r.m.r) {
  num.test <- length(nullprow)
  minp <- rep(NA, num.test)
  for (h in 1:num.test) {
    minp[h] <- min(nullprow) < rawp[r.m.r][h]
  }
  return(as.integer(minp))
}

comp.rawt.sd <- function(nulltrow, rawt, r.m.r) {
  num.test <- length(nulltrow)
  maxt <- rep(NA, num.test)
  maxt[1] <- max(abs(nulltrow)) > abs(rawt)[r.m.r][1]
  for (h in 2:num.test) {
    maxt[h] <- max(abs(nulltrow)[r.m.r][-(1:(h-1))]) > abs(rawt)[r.m.r][h]
  }
  return(as.integer(maxt))
}

comp.rawt.ss <- function(nulltrow, rawt, r.m.r) {
  # nulltrow = nullpt[1,]
  num.test <- length(nulltrow)
  maxt <- rep(NA, num.test)
  for (h in 1:num.test) {
    maxt[h] <- max(abs(nulltrow)) > abs(rawt)[r.m.r][h]
  }
  return(as.integer(maxt))
}
