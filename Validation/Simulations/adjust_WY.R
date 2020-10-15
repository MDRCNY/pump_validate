# This script contains functions for carrying out Westfall-Young step-down corrections
# Last - updated March 7, 2019, Kristin Porter
# Last - edited March 23, 2020, Zarni Htet
# Last - edited July 29, 2020, Kristen Hunter


#' Adjust.WY: does WY adjustments for a single sample in parallel
#'
#' Call to adjust.WY from get.adjp
#' adjust.WY(data=mdat, B=B, subgroup=NULL, which.mult="pooled",
#' incl.covar=TRUE, rawp=rawp, ncl=ncl, clustered=TRUE, blockby='block.id', funct)
#'
#' @param data A list of length M. Each element in the list is a data frame holding the m'th data set
#' @param B Number of permutations
#' @param rawp Vector of length M of the raw p-values
#' @param rawt Vector of lenght M of the raw test statistics
#' @param ncl Number of clusters for parallel processing
#' @param clustered True if we are handling a cluster design
#' @param blockby Variable that designates the clusters or blocks?
#' @param design the particular RCT design for an experiment: "Blocked_i1_2c", "Blocked_i1_2f", "Blocked_i1_2r","Simple_c2_2r"
#' @param maxT whether to adjust based on ordered rawp values or ordered rawT values
#'
#' @return
#' @export
#'
#' @examples
#'
adjust_WY <- function(data, rawp, rawt, design, sim.params.list, model.params.list,
                      clustered = TRUE, blockby = 'block.id') {
  
  # clustered = TRUE; blockby = 'block.id';
  # data = mdat;
  
  
  B <- sim.params.list[['B']]
  ncl <- sim.params.list[['ncl']]
  maxT <- sim.params.list[['maxT']]
  N <- model.params.list[['n.j']]*model.params.list[['J']]
  M <- model.params.list[['M']]
  J <- model.params.list[['J']]
  n.j <- model.params.list[['n.j']]
  p.j <- sim.params.list[['p.j']]
  
  # get order of raw p-values; returns ordered index for the vector "rawp"
  ifelse(maxT==FALSE, r.m.r <- order(rawp), r.m.r <- order(abs(rawt), decreasing=TRUE))
  
  assign.vec <- cbind(n.j,p.j)
  
  cl <- makeSOCKcluster(rep("localhost", ncl))
  clusterExport(
    cl,
    list("perm.regs", "data", "clustered", "blockby", "make.dummies", "make.model",
         "get.tstat.Level1", "get.tstat.Level2", "get.pval.Level1", "get.pval.Level2",
         "resamp.by.block", "fastLm", "lmer", "assign.vec", "J", "design"),
    envir = environment()
  )
  
  iter <- 0
  permT <- NULL
  while(is.null(permT) & iter < 5)
  {
    permT <- tryCatch(
      parallel::parSapply(
        cl, 1:B, function(x,...) { rep(resamp.by.block(assign.vec), J) }
      ),
      error = function(e)
      {
        print(paste('Error for permT. Error:', e, 'Iteration:', iter))
        permT <- NULL
      }
    )
    iter <- iter + 1
  }

  
  # get null p-values (if maxT=FALSE) or test-statistics (if maxT=TRUE) using permuted T's
  # revised KP
  iter <- 0
  nullpt <- NULL
  while(is.null(nullpt) & iter < 5)
  {
    nullpt <- tryCatch(
      parallel::parApply(
        cl, permT, 2, perm.regs, data = data, maxT = maxT, blockby = blockby,
        n.j = n.j, J = J, design = design
      ),
      error = function(e)
      {
        print(paste('Error for nullpt. Error:', e, 'Iteration:', iter))
        nullpt <- NULL
      }
    )
    iter <- iter + 1
  }

  
  stopCluster(cl)
  
  # turn nullpt into a matrix (B rows, ntest columns)
  nullpt.mat <- t(nullpt) # revised
  
  # create dummies for comparisions of null p-values to raw p-values
  if (maxT==FALSE) ind.B <- apply(nullpt.mat, 1, comp.rawp, rawp = rawp, r.m.r = r.m.r)
  if (maxT==TRUE)  ind.B <- apply(nullpt.mat, 1, comp.rawt, rawt = rawt, r.m.r = r.m.r)
  
  # take means of dummies, these are already ordered (by r.m.r) but still need to enforce monotonicity
  pi.p.m <- rowMeans(ind.B)
  
  # enforce monotonicity (keep everything in same order as sorted RAW pvalues from original data)
  adjp.minp <- numeric(M)
  adjp.minp[1] <- pi.p.m[1]
  for (h in 2:length(pi.p.m)) {
    # adjp.minp is a numeric vector of 0's, it will always be less than the values in pi.p.m, right?
    adjp.minp[h] <- max(pi.p.m[h], adjp.minp[h-1])
  }
  
  out <- cbind(rawp[r.m.r], adjp.minp, r.m.r)
  colnames(out) <- c("rawp", "WY", "test num")
  oo <- order(out[ ,"test num"])
  out.oo <- out[oo, ]
  return(out.oo)
}


### resamp.by.block function to help resample for fixed cases
resamp.by.block<-function(...) {
  tc<-numeric(...[1])
  tc[1:(...[1]*...[2])]<-1
  sample(tc,replace=FALSE)
}

#' Performs regression with permuted treatment indicator
#'
#' @param permT matrix with n.j * J rows and B columns, contains all permutations of treatment indicator
#' @param data data for all M domains
#' @param design the particular RCT design for an experiment: "Blocked_i1_2c", "Blocked_i1_2f", "Blocked_i1_2r","Simple_c2_2r"
#' @param blockby blocking variable
#' @param maxT TRUE if using maxT procedures
#' @param n.j individuals per block (assume same for all)
#' @param J number of blocks
#'
#' @return
#' @export
#'
#' @examples
perm.regs <- function(permT, data, design, blockby, maxT, n.j, J) {

  # for debug
  # permT <- permT[,1];
  
  M <- length(data)
  outpt <- numeric(M)
  for (m in 1:M) {
    # Mdata is a dataset for one domain (m) for one sample
    Mdata <- data[[m]]
    Mdata$Treat.ij <- permT
    mdum <- make.dummies(Mdata, blockby = blockby, n.j = n.j, J = J) # took this out of perm.reg so doing just once
    fit <- make.model(mdum$fixdat, mdum$dnames, design)

    if (design == "blocked_i1_2c" | design == "blocked_i1_2f" | design == "blocked_i1_2r") {

      ifelse(maxT, outpt[m] <- get.tstat.Level1(fit), outpt[m] <- get.pval.Level1(fit))

    } else if (design == "Simple_c2_2r"){

      ifelse(maxT, outpt[m] <- get.tstat.Level2(fit),outpt[m] <- get.pval.Level2(fit))
    }
  }
  return(outpt)
}

#' Comp.rawp: makes nullp and rawp comparisons and returns indicators
#'
#' The length of nullprow, rawp, and r.m.r are the same. This function is performed on each row of nullp.mat
#'
#' @param nullprow row of p-values
#' @param rawp raw p-values
#' @param r.m.r vector of indices of rawp in order
#'
#' @return
#' @export
#'
#' @examples
comp.rawp <- function(nullprow, rawp, r.m.r) {


  num.test <- length(nullprow)
  minp <- rep(NA, num.test)
  minp[1] <- min(nullprow[r.m.r]) < rawp[r.m.r][1]
  for (h in 2:num.test) {
    minp[h] <- min(nullprow[r.m.r][-(1:(h-1))]) < rawp[r.m.r][h]
  }
  return(as.integer(minp))
}

#' Comp.rawt: makes nullp and rawt comparisons and returns indicators
#'
#' @param nullptrow row of p-values
#' @param rawt raw t-values
#' @param r.m.r vector of indices of rawp in order
#'
#' @return
#' @export
#'
#' @examples
comp.rawt <- function(nullptrow, rawt, r.m.r) {

  num.test <- length(nullptrow)
  maxt <- rep(NA, num.test)
  maxt[1] <- max(abs(nullptrow)[r.m.r]) > abs(rawt)[r.m.r][1]
  for (h in 2:num.test) {
    maxt[h] <- max(abs(nullptrow)[r.m.r][-(1:(h-1))]) > abs(rawt)[r.m.r][h]
  }
  return(as.integer(maxt))
}




