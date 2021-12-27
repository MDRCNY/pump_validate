
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
      "perm.regs", "make.model", "get.pval.tstat",
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
    ind.B <- t(apply(nullp, 1, PUMP::comp_rawp_ss, rawp))
    adjp <- colMeans(ind.B)
  } else if (proc == 'WY-SD') {
    ind.B <- t(apply(nullp, 1, PUMP::comp_rawp_sd, rawp, rawp.order))
    adjp <- get.adjp.minp(ind.B, rawp.order)
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
    mod <- make.model(dat.m, d_m)[['mod']]
    pval.tstat <- get.pval.tstat(mod, d_m, dgp.params.list)
    out[m] <- pval.tstat[['pval']]
  }
  return(out)
}