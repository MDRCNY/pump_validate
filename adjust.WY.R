
### Functions
### trimmed to just perm.regs and adjust.WY for this project.

### resamp.by.block function to help resample for fixed cases

resamp.by.block<-function(...) {
  tc<-numeric(...[1])
  tc[1:(...[1]*...[2])]<-1
  sample(tc,replace=FALSE)  
}


### perm.regs: performs regressions with permuted treatment indicator
#Mdata is a dataset for one domain (m) for one sample, subgroups=NULL, which.mult="pooled", incl.covar-> unnecessary
#clustered=TRUE, clusterby='block.id', funct= 'random' or 'fixed'
perm.regs <- function(permT,data,clusterby,funct,maxT,n.j,J) {
  outpt<-numeric(length(data))
  M<-length(data)
  for (m in 1:M) {
    Mdata<-data[[m]]
    Mdata$Treat.ij <- permT
    mdum <- make.dummies(Mdata,clusterby=clusterby,n.j=n.j,J=J) # took this out of perm.reg so doing just once
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
#adjust.WY(data=mdat, B=B, subgroup=NULL, which.mult="pooled", incl.covar=TRUE, rawp=rawp, ncl=ncl, clustered=TRUE, clusterby='block.id', funct)
# adjust.WY: does WY adjustments for a single sample in parallel
adjust.WY<-function(data, B, rawp, rawt, ncl, clustered, clusterby, funct, maxT) {
  
  # get number of tests
  ntests <- length(data)
  
  # get order of raw p-values; returns ordered index for the vector "rawp"
  ifelse(maxT==FALSE, r.m.r <- order(rawp), r.m.r <- order(abs(rawt), decreasing=TRUE))
  
  # permute all Treatment indicator B times (outside of apply below so all outcomes have same permutation)
  permT<-matrix(NA,n.j*J,B)
  for (b in 1:B) {
    permT[,b]<-as.vector(apply(cbind(n.j,p.j),1,resamp.by.block))
    summary(permT[,b])
  }

  
  cl <- makeSOCKcluster(rep("localhost", ncl))
  clusterExport(cl, list("perm.regs", "data", "clustered", "clusterby", "funct", "make.dummies", "make.model", "get.tstat", "get.pval", "p.j", "resamp.by.block", "fastLm", "lmer"), envir=environment())
  
  # get null p-values (if maxT=FALSE) or test-statistics (if maxT=TRUE) using permuted T's
  nullpt <- parApply(cl,permT,2,perm.regs,data=data,clusterby=clusterby,funct=funct,maxT=maxT,n.j=n.j,J=J)   # revised KP

  stopCluster(cl)
  
#   # get B p-values for all tests
#   nullpt <- lapply(data, function(y) {
#  #   parSapply(cl, 1:B, function(x) { #in parallel
#     sapply(1:B, function(x) { #in serial
#       #perm.regs <- function(Mdata, Subgroups, which.mult, incl.covar, clustered, clusterby, funct) --> inputs for perm.regs
#       #perm.regs calls make.dummies, make.model, and get.pval
#       perm.regs(y, clustered=clustered, clusterby=clusterby, funct=funct, maxT=maxT) # y is dataset for one outcom
#     })
#   })
  
  # turn nullpt into a matrix (B rows, ntest columns)
  nullpt.mat <- t(nullpt) # revised KP
  #nullpt.mat <- do.call(cbind, nullpt)
  
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
