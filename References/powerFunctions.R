## ------------------------------------------------------------------------
comp.rawt.SS <- function(abs.Zs.H0.1row, abs.Zs.H1.1samp, oo) {
  M<-length(abs.Zs.H0.1row)
  maxt <- rep(NA, M)
  for (m in 1:M) {maxt[m] <- max(abs.Zs.H0.1row[m]) > abs.Zs.H1.1samp[m]}
  return(as.integer(maxt))
}

## ------------------------------------------------------------------------
comp.rawt.SD <- function(abs.Zs.H0.1row, abs.Zs.H1.1samp, oo) {
  M<-length(abs.Zs.H0.1row)
  maxt <- rep(NA, M)
  nullt.oo<-abs.Zs.H0.1row[oo]
  rawt.oo<-abs.Zs.H1.1samp[oo]
  maxt[1] <- max(nullt.oo) > rawt.oo[1]
  for (h in 2:M) {maxt[h] <- max(nullt.oo[-(1:(h-1))]) > rawt.oo[h]}
  return(as.integer(maxt))
}

## ------------------------------------------------------------------------
adjust.allsamps.WYSS<-function(snum,abs.Zs.H0,abs.Zs.H1) {
  adjp.WY<-matrix(NA,snum,ncol(abs.Zs.H0))
  doWY<-for (s in 1:snum) {
    ind.B<-t(apply(abs.Zs.H0, 1, comp.rawt.SS, abs.Zs.H1.1samp=abs.Zs.H1[s,]))
    adjp.WY[s,]<-colMeans(ind.B)
  }
  return(adjp.WY)
}

## ------------------------------------------------------------------------
adjust.allsamps.WYSD<-function(snum,abs.Zs.H0,abs.Zs.H1,order.matrix) {
  cl <- makeCluster(ncl)
  registerDoParallel(cl)
  clusterExport(cl=cl, list("comp.rawt.SD"))
  M<-ncol(abs.Zs.H0)
  adjp.WY<-matrix(NA,snum,M)

  doWY <- foreach(s=1:snum, .combine=rbind) %dopar% {
    ind.B<-t(apply(abs.Zs.H0, 1, comp.rawt.SD, abs.Zs.H1.1samp=abs.Zs.H1[s,], oo=order.matrix[s,]))
    pi.p.m <- colMeans(ind.B) 
  # enforcing monotonicity
    adjp.minp <- numeric(M)
    adjp.minp[1] <- pi.p.m[1]
    for (h in 2:M) {adjp.minp[h] <- max(pi.p.m[h], adjp.minp[h-1])}
    adjp.WY[s,] <- adjp.minp[order.matrix[s,]]
  }
  return(doWY)
  stopCluster(cl)
}

## ------------------------------------------------------------------------
t.mean.H1<-function(MDES,J,n.j,R2.1,p) { MDES * sqrt(p*(1-p)*J*n.j) / sqrt(1-R2.1) }

## ------------------------------------------------------------------------
df<-function(J,n.j,numCovar.1) {J*n.j - J - numCovar.1 - 1}

## ------------------------------------------------------------------------
power.blockedRCT.2<-function(M, MDES, J, n.j,
                             p, alpha, numCovar.1, numCovar.2=0, R2.1, R2.2, ICC, 
                             mod.type, sigma, omega,
                             tnum = 10000, snum=1000, ncl=2) {

# load required packages
  require(MASS); require(mvtnorm); require(multtest); require(doParallel)
  
# number of false nulls  
  numfalse<-sum(1*MDES>0)
 
# compute Q(m) for all false nulls
  t.shift<-t.mean.H1(MDES,J,n.j,R2.1,p)
  t.df<-df(J,n.j,numCovar.1)
  t.shift.mat<-t(matrix(rep(t.shift,tnum),M,tnum)) # repeating shift.beta on every row
    
# generate test statistics and p-values under null and alternative $s=\frac{1}{2}$
  Zs.H0<-rmvt(tnum, sigma = sigma, df = t.df, delta = rep(0,M),type = c("shifted", "Kshirsagar")) 
  Zs.H1 <- Zs.H0 + t.shift.mat
  pvals.H0<-2*pt(-abs(Zs.H0),df=t.df)
  pvals.H1<-2*pt(-abs(Zs.H1),df=t.df)    
  abs.Zs.H0 <- abs(Zs.H0)
  abs.Zs.H1 <- abs(Zs.H1)
  
# adjust p-values for all but Westfall-Young  
  adjp<-apply(pvals.H1,1,mt.rawp2adjp,proc=c("Bonferroni","Holm","BH"),alpha=alpha)

  grab.pval<-function(...,proc) {return(...$adjp[order(...$index),proc])}
  rawp<-do.call(rbind,lapply(adjp,grab.pval,proc="rawp"))
  adjp.BF<-do.call(rbind,lapply(adjp,grab.pval,proc="Bonferroni"))
  adjp.HO<-do.call(rbind,lapply(adjp,grab.pval,proc="Holm"))
  adjp.BH<-do.call(rbind,lapply(adjp,grab.pval,proc="BH"))
  
# adjust p-values for Westfall-Young (single-step and step-down)
  order.matrix<-t(apply(abs.Zs.H1,1,order,decreasing=TRUE))
  adjp.SS<-adjust.allsamps.WYSS(snum,abs.Zs.H0,abs.Zs.H1)
  adjp.WY<-adjust.allsamps.WYSD(snum,abs.Zs.H0,abs.Zs.H1,order.matrix)

# combine all adjusted p-values in list (each entry is matrix for given MTP)
  adjp.all<-list(rawp,adjp.BF,adjp.HO,adjp.BH,adjp.SS,adjp.WY)
  
# for each MTP, get matrix of indicators of whether adjusted p-value is less than alpha  
  reject<-function(x) {as.matrix(1*(x<alpha))}
  reject.all<-lapply(adjp.all,reject)

# in each row for each MTP matrix, count number of p-values less than 0.05, in rows corresponding to false nulls
  lt.alpha<-function(x) {apply(as.matrix(x[,MDES>0]),1,sum)}
  lt.alpha.all<-lapply(reject.all,lt.alpha)

# indiv power for WY, BH, and HO is mean of columns of dummies of whether adjusted pvalues were less than alpha
  power.ind.fun<-function(x) {apply(x,2,mean)}
  power.ind.all<-lapply(reject.all,power.ind.fun)
  power.ind.all.mat<-do.call(rbind,power.ind.all)

# m-min powers for all procs (including complete power when m=M)
  power.min.fun <- function(x,M) {
    power.min<-numeric(M)
    cnt<-0
    for (m in 1:M) {
      power.min[m]<-mean(x>cnt)
      cnt<-cnt+1
    }
    return(power.min)
    }
  power.min<-lapply(lt.alpha.all,power.min.fun,M=M)
  power.min.mat<-do.call(rbind,power.min)
  
# complete power is probability all false nulls rejected when p-values not adjusted 
  # this is row 1 and column number = numfalse
  power.cmp<-rep(power.min.mat[1,M],length(power.min)) # should it be numfalse or M?
  
# combine all power for all definitions
  all.power.results<-cbind(power.ind.all.mat,power.min.mat[,-M],power.cmp)

# take mean of all individual power estimates
  mean.ind.power <- apply(as.matrix(all.power.results[,1:M][,MDES>0]),1,mean)

# revise final matrix to report this mean individual power and return results  
  all.power.results<-cbind(mean.ind.power,all.power.results)
  colnames(all.power.results)<-c("indiv",paste0("indiv",1:M),paste0("min",1:(M-1)),"complete")
  rownames(all.power.results)<-c("rawp","BF","HO","BH","WY-SS","WY-SD")
  return(all.power.results)
}

## ----power.blockedRCT.2_Example, eval=TRUE, message=FALSE, warning=FALSE----
  ncl<-2
  rho<-0.5
  M <- 3
  sigma<-matrix(rep(rho,M*M),nrow=M,ncol=M)
  diag(sigma)<-1
  MDES<-c(rep(0.185,M))
#  MDES<-c(rep(0.3,M*2/3),rep(0,M/3))
  p <- 0.5
  R2.1 <- 0
  R2.2 <- 0
  numCovar.1 <- 0
  numCovar.2 <- 0
  ICC <- 0
  J <- 50
  n.j <- 20
  alpha = 0.05

power.blockedRCT.2(M, MDES, J, n.j,
                    p, alpha, numCovar.1, numCovar.2, R2.1, R2.2, ICC, 
                    mod.type = "constant", sigma, omega = NULL,
                    tnum=10000, snum=2, ncl)
  


## ----MDESfun, message=FALSE, warning=FALSE-------------------------------

MDES.blockedRCT.2<-function(M, numFalse, J, n.j, power, power.definition, MTP, marginError,
                            p, alpha, numCovar.1, numCovar.2=0, R2.1, R2.2, ICC, 
                            mod.type, sigma, omega,
                            tnum = 10000, snum=2, ncl=2) {

# We don't need snum high if not WY  
  if (!(MTP %in% c("WY-SS","WY-SD"))) snum <- 2
    
# Compute Q(m)
  Q.m<-sqrt( (1-R2.1) / (p*(1-p)*J*n.j) )
  t.df<-df(J,n.j,numCovar.1)
  
# For raw and BF, compute critical values 
  crit.alpha <- qt(p=(1-alpha/2),df=t.df)  
  crit.alphaxM <- qt(p=(1-alpha/M/2),df=t.df)  
  
# Compute raw and BF MDES for INDIVIDUAL POWER
  crit.beta <- qt(power,df=t.df)
  MDES.raw <- ifelse(power > 0.5, Q.m * (crit.alpha + crit.beta), Q.m * (crit.alpha - crit.beta)) 
  MDES.BF <- ifelse(power > 0.5, Q.m * (crit.alphaxM + crit.beta), Q.m * (crit.alphaxM - crit.beta)) 
  
### INDIVIDUAL POWER ###
if (power.definition=="indiv") {  
  if (MTP == "raw") return (c(MDES.raw,power))
  if (MTP == "BF")  return (c(MDES.BF,power)) 
}
    
# For individual power, other MDES's will be between MDES.raw and MDES.BF, so make starting value the midpoint
  if (MTP %in% c("HO","BH","WY-SS","WY-SD") & power.definition == "indiv") {
    lowhigh <- c(MDES.raw,MDES.BF)
    try.MDES <- (MDES.raw + MDES.BF) / 2
  }

# For minimal powers, makes starting value = MDES.raw
  if (power.definition != "indiv")  {
    lowhigh <- c(0,1)
    try.MDES <- MDES.raw
  }
    target.power <- 0 
    ii <- 0
    while (ii < 10 & (target.power < power - marginError | target.power > power + marginError)) {
      print(lowhigh)
      print(target.power)
      print(try.MDES)
      runpower <- power.blockedRCT.2(M, try.MDES, J, n.j,
                                     p, alpha, numCovar.1, numCovar.2=0, R2.1, R2.2, ICC, 
                                     mod.type, sigma, omega,
                                     tnum, snum, ncl)
      #print(runpower)
      target.power <- runpower[MTP,power.definition]
      is.over <- target.power > power
      if(target.power > power - marginError & target.power < power + marginError) return(c(try.MDES,target.power))
      if(!is.over) {
        p.off <- (power - target.power) / power
        lowhigh[1] <- try.MDES
      }
      if(is.over) {
        lowhigh[2] <- try.MDES
        p.off <- (target.power - power) / (1 - power)
      }
      lowhigh.dist <- lowhigh[2]-lowhigh[1]
#      try.MDES <- ifelse(is.over, lowhigh[2] - p.off * lowhigh.dist, lowhigh[1] + p.off*lowhigh.dist)
      try.MDES <- ifelse(target.power < power, (try.MDES + lowhigh[2])/2, (try.MDES + lowhigh[1])/2) # midpoint
      ii <- ii + 1
      } # end while
}

test <- MDES.blockedRCT.2(M=M, numFalse = M, J=50, n.j=20, power=0.80, power.definition = "min2", MTP = "BF", marginError = 0.005,
                          p=0.5, alpha=0.05, numCovar.1=0, numCovar.2=0, R2.1=0, R2.2=0, ICC=0, 
                          mod.type="constant", sigma=sigma, omega=NULL,
                          tnum = 10000, snum=2, ncl=2)


print(test)

