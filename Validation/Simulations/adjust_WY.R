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
adjust_WY <- function(dat.all, rawt, S.id, D.id,
                      design, proc,
                      sim.params.list, model.params.list,
                      cl = NULL) {

  # cl = NULL;
  B <- sim.params.list[['B']]
  Tbar <- sim.params.list[['Tbar']]
  
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
  
  # generate null test-statistics
  if(!is.null(cl))
  {
    clusterExport(cl, list(
      "perm.regs", "make.model", "get.pval.tstat",
      "lmer", "interacted_linear_estimators", "isSingular",
      "calc.df"
    ), envir = environment())
    
    nullt <- t(parallel::parApply(
      cl, permT, 2, perm.regs, dat.all = dat.all,
      design = design, user.params.list = user.params.list
    ))
  } else
  {
    nullt <- t(apply(
      permT, 2, perm.regs, dat.all = dat.all,
      design = design, user.params.list = user.params.list
    ))
  }
  
  # now calculate WY p-values
  rawt.order <- order(rawt, decreasing = TRUE)
  if (proc == 'WY-SS') {
    ind.B <- t(apply(nullt, 1, comp.rawt.ss, rawt))
    adjp <- colMeans(ind.B)
  } else if (proc == 'WY-SD') {
    ind.B <- t(apply(nullt, 1, comp.rawt.sd, rawt, rawt.order))
    adjp <- get.adjp.minp(ind.B, rawt.order)
  }
  
  return(adjp)
}

#' Performs regression with permuted treatment indicator
#'
#' @param permT.vec matrix with nbar * J rows and B columns, contains all permutations of treatment indicator
#' @param dat.all data for all M domains
#' @param design the particular RCT design for an experiment: "Blocked_i1_2c", "Blocked_i1_2f", "Blocked_i1_2r","Simple_c2_2r"
#'
#' @return
#' @export
#'
#' @examples
perm.regs <- function(permT.vec, dat.all, design, user.params.list) {
  # permT.vec <- permT[,1];

  M <- length(dat.all)
  out <- numeric(M)
  for (m in 1:M) {
    dat.m <- dat.all[[m]]
    dat.m$T.x <- permT.vec
    mod <- make.model(dat.m, design)[['mod']]
    pval.tstat <- get.pval.tstat(mod, design, user.params.list)
    out[m] <- pval.tstat[['tstat']]
  }
  return(out)
}