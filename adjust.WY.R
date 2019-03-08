# This script contains functions for carrying out Westfall-Young step-down corrections
# Last - updated March 7, 2019, Kristin Porter

### resamp.by.block function to help resample for fixed cases
resamp.by.block<-function(...) {
  tc<-numeric(...[1])
  tc[1:(...[1]*...[2])]<-1
  sample(tc,replace=FALSE)  
}


### perm.regs: performs regressions with permuted treatment indicator

# permT = matrix with n.j * J rows and B columns, contains all permutations of treatment indicator
# data = data for all M domains
# blockby = blocking variable 
# funct = "constant", "fixed" or "random"
# maxT = TRUE if using maxT procedures, otherwise minp
# n.j = individuals per block (assume same for all)
# J = number of blocks
perm.regs <- function(permT,data,blockby,funct,maxT,n.j,J) {
  outpt<-numeric(length(data))
  M<-length(data)
  for (m in 1:M) {
    # Mdata is a dataset for one domain (m) for one sample
    Mdata<-data[[m]]
    Mdata$Treat.ij <- permT
    mdum <- make.dummies(Mdata,blockby=blockby,n.j=n.j,J=J) # took this out of perm.reg so doing just once
    fit <- make.model(mdum$fixdat, mdum$dnames, funct)
    ifelse(maxT,outpt[m]<-get.tstat(fit),outpt[m]<-get.pval(fit))
    }
  return(outpt)
}

### comp.rawp: makes nullp and rawp comparisons and returns indicators
# nullprow: row of p-values
# rawp: raw p-values
# r.m.r: vector of indices of rawp in order
# The length of nullprow, rawp, and r.m.r are the same. This function is performed on each row of nullp.mat

comp.rawp <- function(nullprow, rawp, r.m.r) {
  num.test <- length(nullprow)
  minp <- rep(NA, num.test)
  minp[1] <- min(nullprow[r.m.r]) < rawp[r.m.r][1]
  for (h in 2:num.test) {
    minp[h] <- min(nullprow[r.m.r][-(1:(h-1))]) < rawp[r.m.r][h]
  }
  return(as.integer(minp))
}
comp.rawt <- function(nullptrow, rawt, r.m.r) {
  num.test <- length(nullptrow)
  maxt <- rep(NA, num.test)
  maxt[1] <- max(abs(nullptrow)[r.m.r]) > abs(rawt)[r.m.r][1]
  for (h in 2:num.test) {
    maxt[h] <- max(abs(nullptrow)[r.m.r][-(1:(h-1))]) > abs(rawt)[r.m.r][h]
  }
  return(as.integer(maxt))
}
#call to adjust.WY from get.adjp
#adjust.WY(data=mdat, B=B, subgroup=NULL, which.mult="pooled", incl.covar=TRUE, rawp=rawp, ncl=ncl, clustered=TRUE, blockby='block.id', funct)
# adjust.WY: does WY adjustments for a single sample in parallel

# data        A list of length M. Each element in the list is a data frame holding the m'th data set
# B           Number of permutations
# rawp        Vector of length M of the raw p-values
# rawt        Vector of lenght M of the raw test statistics
# ncl         Number of clusters for parallel processing
# clustered   True if 
# blockby   Variable that designates the clusters
# funct       

adjust.WY<-function(data, B, rawp, rawt, ncl, clustered, blockby, funct, maxT) {
  
  # get number of tests
  ntests <- length(data)
  
  # get order of raw p-values; returns ordered index for the vector "rawp"
  ifelse(maxT==FALSE, r.m.r <- order(rawp), r.m.r <- order(abs(rawt), decreasing=TRUE))
  
  # permute all Treatment indicator B times - permutations are done by block
  permT<-matrix(NA,n.j*J,B)
  for (b in 1:B) {
    permT[,b]<-as.vector(apply(cbind(n.j,p.j),1,resamp.by.block))
    summary(permT[,b])
  }
  
  cl <- makeSOCKcluster(rep("localhost", ncl))
  clusterExport(cl, list("perm.regs", "data", "clustered", "blockby", "funct", "make.dummies", "make.model", "get.tstat", "get.pval", "p.j", "resamp.by.block", "fastLm", "lmer"), envir=environment())
  
  #print these to see what is assigned
  print(cl)
  print(ncl)
  
  # get null p-values (if maxT=FALSE) or test-statistics (if maxT=TRUE) using permuted T's
  nullpt <- parApply(cl,permT,2,perm.regs,data=data,blockby=blockby,funct=funct,maxT=maxT,n.j=n.j,J=J)   # revised KP
  
  stopCluster(cl)
  
  
  # turn nullpt into a matrix (B rows, ntest columns)
  nullpt.mat <- t(nullpt) # revised 
  
  # create dummies for comparisions of null p-values to raw p-values
 if (maxT==FALSE) ind.B <- apply(nullpt.mat, 1, comp.rawp, rawp=rawp, r.m.r=r.m.r)
 if (maxT==TRUE) ind.B <- apply(nullpt.mat, 1, comp.rawt, rawt=rawt, r.m.r=r.m.r)

  
  # take means of dummies, these are already ordered (by r.m.r) but still need to enforce monotonicity
  pi.p.m <- rowMeans(ind.B)
  
  # enforce monotonicity (keep everything in same order as sorted RAW pvalues from original data)
  adjp.minp <- numeric(ntests)
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


