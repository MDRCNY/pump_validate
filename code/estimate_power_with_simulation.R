# to install blkvar package:
# install.packages("remotes")
# remotes::install_github("lmiratrix/blkvar")
# library(blkvar)
library(lme4)

#'  Function: est_power_sim				                                       
#'  
#'  Function to estimate statistical power using simulations (on t-statistics)
#'
#' @param model.params.list List of user-supplied parameters
#' @param sim.params.list List of simulation parameters
#' @param d_m RCT d_m (see list/naming convention)
#' @param cl cluster object for parallel computing
est_power_sim <- function(model.params.list, sim.params.list, d_m, cl = NULL) {
  
  # convert user-inputted parameters into model parameters
  dgp.params.list <- PUMP::convert_params(model.params.list)
  
  # save out some commonly used variables
  M <- dgp.params.list[['M']]
  S <- sim.params.list[['S']]
  Tbar <- sim.params.list[['Tbar']]
  MTP <- sim.params.list[['MTP']]

  # list of adjustment procedures
  adjp.proc <- array(0, c(S, M, length(MTP) + 1))
  dimnames(adjp.proc) <- list(NULL, NULL, c("None", MTP))
  names(adjp.proc) <- c("None", MTP)
  
  # how often to print messages
  px <- 100
  
  # begin loop through all samples to be generated
  num.singular.raw <- 0
  num.failed.converge.raw <- 0
  t1 <- Sys.time()
  for (s in 1:S) {
    
    if (s %% px == 0){ message(paste0("Now processing sample ", s, " of ", S)) }
    
    # generate full, unobserved sample data
    samp.full <- PUMP::gen_full_data(dgp.params.list)
    S.id <- samp.full$ID$S.id
    D.id  <- samp.full$ID$D.id
    
    T.x <- PUMP::gen_T.x(d_m = d_m,
                         S.id = S.id, D.id = D.id,
                         nbar = dgp.params.list$nbar,
                         Tbar = sim.params.list$Tbar)
    

    # convert full data to observed data
    samp.obs <- samp.full
    samp.obs$Yobs <- PUMP::gen_Yobs(samp.full, T.x)
    
    dat.all <- makelist_samp(samp.obs, T.x) # list length M
    rawpt.out <- get_rawpt(dat.all, d_m = d_m, model.params.list = model.params.list)
    rawp <- sapply(rawpt.out[['rawpt']], function(s){ return(s[['pval']])})
    rawt <- sapply(rawpt.out[['rawpt']], function(s){ return(s[['tstat']])})
    
    # track how many failures occur
    num.singular.raw <- num.singular.raw + rawpt.out[['num.singular']]
    num.failed.converge.raw <- num.failed.converge.raw + rawpt.out[['num.failed.converge']]
    
    # loop through adjustment procedures (adding 'None' as default in all cases)
    for (p in 1:(length(MTP) + 1)) {
      if (p == 1) {
        pvals <- rawp
        proc <- "None"
      } else {
        t11 <- Sys.time()
        
        proc <- MTP[p-1]
        pvals <- get.adjp(
          proc = proc, rawp = rawp,
          dat.all = dat.all, S.id = S.id, D.id = S.id,
          sim.params.list = sim.params.list, dgp.params.list = dgp.params.list,
          d_m = d_m, cl = cl
        )
        
        t21 <- Sys.time()
        if (s == 1) { message(paste("One sample of", proc, "took", round(difftime(t21, t11, units = 'secs')[[1]], 4), 'seconds')) }
      }
      adjp.proc[s,,proc] = pvals
    }
    
    if (s == 10) {
      t2 <- Sys.time()
      message(paste(
        "Current time:", t2,
        "\nExpected time diff for simulation of", round(S*difftime(t2, t1, units = 'secs')[[1]]/(10*60), 2),
        "minutes.\nExpected finish for simulation at", t1 + (t2 - t1) * S,"for S =", S, sep =" ")
      )
    }
    else if (s %% px == 0) { message(paste('Progress: iteration', s, 'of', S, 'complete, running time:', difftime(t2, t1))) }
  } # end loop through samples
  
  message(paste('Number of singular fits:', num.singular.raw))
  message(paste('Number of failed convergence:', num.failed.converge.raw))
  return(adjp.proc)
}

# --------------------------------------------------------------------- #
#  Function: get.adjp 	Inputs: rawp, proc, alpha				                  #
#		a matrix nrow=S ncol=M of raw p-values							                  #
#  	a string for a single proc            							                  #
#  	a number, alpha, should be 0.05 in most cases	 			                  #
#	Outputs: MxS matrix of adjusted p-values for a single proc 	            #
# --------------------------------------------------------------------- #

get.adjp <- function(proc, rawp, dat.all, S.id, D.id,
                     sim.params.list, dgp.params.list, d_m, cl = NULL) {

  if(proc == "WY-SD" | proc == "WY-SS"){
    tw1 <- Sys.time()
    adjp.proc <- adjust_WY(
      dat.all = dat.all,
      rawp = rawp,
      S.id = S.id, D.id = D.id,
      proc = proc,
      sim.params.list = sim.params.list,
      dgp.params.list = dgp.params.list,
      d_m = d_m,
      cl = cl
    )
    tw2 <- Sys.time()
  }
  else {
    # return a matrix with m columns (domains) and b rows (samples)
    # this needs rawp to be a matrix with m columns and was d_med for all samples to be a row.
    if (proc == "BF"){
      adjp.proc <- p.adjust(rawp, method = "bonferroni")
    } else if (proc == "HO") {
      adjp.proc <- p.adjust(rawp, method = "holm")
    } else if (proc == "BH") {
      adjp.proc <- p.adjust(rawp, method = "hochberg")
    } else
    {
      stop("Unknown MTP")
    }
  }
  return(adjp.proc)
}

