#' ---
#' title: "Power Under Multiplicity Functions"
#' author: "Kristin Porter"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: html_notebook
#' ---
#' 
#' 
# load required packages
library(MASS); library(mvtnorm); library(multtest); library(doParallel);
#' ## Functions for Computing Power for Westfall-Young
#' 
#' This function operates on one row of null test statistics. It compares $max_{1 \leq l \leq M} {T_l}$ to $|t_m|$ for all $m$.  
#' 
## ------------------------------------------------------------------------

comp.rawt.SS <- function(abs.Zs.H0.1row, abs.Zs.H1.1samp, oo) {
  
  #browser()
  M<-length(abs.Zs.H0.1row)
  #browser()
  maxt <- rep(NA, M)
  #browser()
  #which one is the unpermuted raw test statisics here in abs.Zs.H0.1row and abs.Zs.H1.1samp??
  for (m in 1:M) {maxt[m] <- max(abs.Zs.H0.1row[m]) > abs.Zs.H1.1samp[m]}
  
  return(as.integer(maxt))
}

#' 
#' 
#' This function operates on each row of null test statistics. It first orders the test statistics such that $t_{s_1} \geq t_{s_2} \geq \dots \geq t_{s_M}$, and then compares $max{|T_{s_1}|,\dots,|T_{s_m}|}$ to $|t_{s_1}|$, $max{|T_{s_2}|,\dots,|T_{s_m}|}$ to $|t_{s_2}|$ and so on, until one compares $max{|T_{s_m}|}$ to ${t_{s_m}$. 
#'     
#' In this function, $abs.Zs.H0.1row$ is one row of test statistics under the joint null hypothesis; 
#' $abs.Zs.H1.1samp$ are the raw test statistics when the null is false (I believe this is only when the null is TRUE?), for one simulated sample, 
#' and oo is the ordering of these raw test statistics.  
#' 
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

#' t
#' In adjust.allsamps.WYSS, we do this multiple times, where snum is number of samples do compute WY pvals to estimate WY power. Here, 
#'     # ind.B is a matrix of whether ordered absolute pvalue was greater than ordered null test statistics
#'     # pi.p.m # adjusted pvalues before enforcing monotonicity
#' 
## ------------------------------------------------------------------------
adjust.allsamps.WYSS<-function(snum,abs.Zs.H0,abs.Zs.H1) {
  
  #browser()
  adjp.WY<-matrix(NA,snum,ncol(abs.Zs.H0))
  #browser()
  doWY<-for (s in 1:snum) {
    
    #browser()
    ind.B<-t(apply(abs.Zs.H0, 1, comp.rawt.SS, abs.Zs.H1.1samp=abs.Zs.H1[s,]))
    #browser()
    adjp.WY[s,]<-colMeans(ind.B)
    #browser()
    #i <- s+1
    #print(i)
  }
  return(adjp.WY)
}

#' 
## ------------------------------------------------------------------------
adjust.allsamps.WYSD<-function(snum,abs.Zs.H0,abs.Zs.H1,order.matrix) {
  cl <- makeCluster(ncl=2) # temporary measure
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

#' 
#' ## Helper functions
#' Calculates the means of the test statistics under the joint alternative hypothesis
#' Recall that $t(m)$ has a $t$-distribution with mean $MDES(m)/Q(m)$, where 
#' $$ Q(m) = \sqrt{ \frac{(1-R^2(m))}{p(1-p)Jn_j} } $$
#' p is the probability of treatment assignment: So how does this mechanism help us generate permutation exposures?
#' We are only looking at the mean alternative hypothesis (the mean of the test statistics, whichever measure that we are intersted in
#' Couldn't MDES vary across multiple outcomes too?
## ------------------------------------------------------------------------

t.mean.H1<-function(MDES,J,n.j,R2.1,p) { MDES * sqrt(p*(1-p)*J*n.j) / sqrt(1-R2.1) }

#' 
#' $t(m)$ also has degrees of freedom, $df = Jn_j - J - g^*(m)-1$, where $g^*(m)$ is the toal number of baseline covariates included in the model for the $m^th$ outcome, including the block indicators so that $g^*(m)=J+k(m)$.
#' 
## ------------------------------------------------------------------------
df<-function(J,n.j,numCovar.1) {J*n.j - J - numCovar.1 - 1}

#' 
#' 
#'  *power.blocked.RCT.2: for 2 level blocked RCT*
#'     # estimates power, MDES or sample size (either number of blocks or number of units per block)
#'     # for either constant, fixed or random effects
#' 
#'     # M is the number of hypothesis tests (outcomes)
#'     # MDES = vector of MDES's of length M - this is to detect the minimum detectable effect size. 0 if you think there is no false null (i.e the null is true)
#'     # J = number of blocks
#'     # n.j = units per block
#'     # p = the proportion of samples that are assigned the treatment
#'     # alpha = significance level
#'     # numCovar.1 = number of Level 1 baseline covariates (not including block dummies)
#'     # numCovar.2 = number of Level 2 baseline covariates
#'     # R2.1 = a vector of length M corresponding to R^2 for M outcomes of Level 1 (R^2 = variation in the data explained by the model)
#'     # R2.2 = a vector of length M corresponding to R^2 for M outcomes of Level 2 
#'     # ICC = intraclass correlation
#'     # sigma = correlation matrix for correlations between test statistics (this needs to flexible across multiple test statistics. Now, it's set to be at 1.
#'     # omega = NULL
#'     # mod. type = "c" for constant effects, "f" for fixed effects, "r" for random effects
#'     # tnum = number of test statistics (samples) for all procedures other than WY & number of permutations for WY (i.e permutation samples) WY has permutation which is reassignment of treatment variables.
#'     # tnum (continued) - We do not see the treatment reassignment for permutation explicitly here.
#'     # snum = number of samples for WY (i.e are they less than the other tests. If so, why?)
#'     # ncl = the number of clusters to use for parallel processing. It has a default of 2.
#' 
## ------------------------------------------------------------------------
power.blockedRCT.2<-function(M, MDES, J, n.j,rho,
                             p, alpha, numCovar.1, numCovar.2, ICC, 
                             omega = NULL,
                             tnum, snum, ncl = 2) {
  
  #setting the sigma
  sigma <-matrix(0.99,M,M)
  
  #Setting R2.1 in the function
  R2.1 <- 0.5
  
  # number of false nulls  
  numfalse<-sum(1*MDES>0)
  
  # compute Q(m) for all false nulls
  # this should be only one number as we have only one inputs of test statistics under alternative
  # The shift is the means by which the alternative is different from the null. 
  # The 0.5 is where the random chance happens!
  # In other words, on average, under the assumption that the null is true, the raw test statistics would be shifted by t.shift amount
  # Then, we have the test statistics under null generated 10,000 times then, the shift is added to get 1 unpermuted actual test statistics (i.e right since this has
  # the MDES, J, n.j, R2.1, p)
  # How can we claim that it is permutation? We don't have a treatment indicator changing hands that leads to different outcome results.
  # What we have generated is test statistics under the null from a multivariate t-distribution. 
  
  t.shift<-t.mean.H1(MDES,J,n.j,R2.1,p)
  
  #browser()
  
  t.df<-df(J,n.j,numCovar.1)
  
  #browser()
  
  t.shift.mat<-t(matrix(rep(t.shift,tnum),M,tnum)) # repeating shift.beta on every row
  
  #browser()
  
  # generate test statistics and p-values under null and alternative $s=\frac{1}{2}$
  # simulating student t distribution. Kshirsagar is for multiple contrasts. Is this why we can consider it the permutation with different treatment
  # effect is already taken care of?
  
  Zs.H0<-mvtnorm::rmvt(tnum, sigma = sigma, df = t.df, delta = rep(0,M),type = c("shifted", "Kshirsagar")) 
  
  #browser()
  
  Zs.H1 <- Zs.H0 + t.shift.mat
  
  #browser()
  
  pvals.H0<-2*pt(-abs(Zs.H0),df=t.df)
  
  #browser()
  
  pvals.H1<-2*pt(-abs(Zs.H1),df=t.df)    
  
  #browser()
  
  abs.Zs.H0 <- abs(Zs.H0)
  
  #browser()
  
  abs.Zs.H1 <- abs(Zs.H1)

  #browser()
  
  # adjust p-values for all but Westfall-Young! ERROR !
  # document when M = 1 and error
  #browser()
  #adjp<-apply(pvals.H1,1,mt.rawp2adjp,proc=c("Bonferroni","Holm","BH"),alpha=alpha)
  
  adjp<-mt.rawp2adjp(pvals.H1,proc=c("Bonferroni","Holm","BH"),alpha=alpha)
  #browser()

  grab.pval<-function(...,proc) {return(...$adjp[order(...$index),proc])}

  #rawp<-do.call(rbind,lapply(adjp,grab.pval,proc="rawp"))
  rawp <- grab.pval(adjp, proc = "rawp")
  
  #browser()
  
  #adjp.BF<-do.call(rbind,lapply(adjp,grab.pval,proc="Bonferroni"))
  adjp.BF <- grab.pval(adjp, proc = "Bonferroni")
  
  #adjp.HO<-do.call(rbind,lapply(adjp,grab.pval,proc="Holm"))
  adjp.HO <- grab.pval(adjp, proc = "Holm")
  
  #adjp.BH<-do.call(rbind,lapply(adjp,grab.pval,proc="BH"))
  adjp.BH <- grab.pval(adjp, proc = "BH")
  
  #browser()
  
  # adjust p-values for Westfall-Young (single-step and step-down)
  # ordering each row of p-values by descending order
  order.matrix<-t(apply(abs.Zs.H1,1,order,decreasing=TRUE))
  
  adjp.SS<-adjust.allsamps.WYSS(snum,abs.Zs.H0,abs.Zs.H1)
  #adjp.WY<-adjust.allsamps.WYSD(snum,abs.Zs.H0,abs.Zs.H1,order.matrix)

  # combine all adjusted p-values in list (each entry is matrix for given MTP)
  adjp.all<-list(rawp,adjp.BF,adjp.HO,adjp.BH,adjp.SS)
  #browser()
  
  # for each MTP, get matrix of indicators of whether adjusted p-value is less than alpha  
  reject<-function(x) {as.matrix(1*(x<alpha))}
  reject.all<-lapply(adjp.all,reject)
  #browser()
  
  
  # in each row for each MTP matrix, count number of p-values less than 0.05, in rows corresponding to false nulls
  lt.alpha<-function(x) {apply(as.matrix(x[,MDES>0]),1,sum)}
  lt.alpha.all<-lapply(reject.all,lt.alpha)
  # browser()
  
  # indiv power for WY, BH, and HO is mean of columns of dummies of whether adjusted pvalues were less than alpha
  power.ind.fun<-function(x) {apply(x,2,mean)}
  power.ind.all<-lapply(reject.all,power.ind.fun)
  # browser()
  power.ind.all.mat<-do.call(rbind,power.ind.all)
  # browser()
  
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
  #browser()
  power.min.mat<-do.call(rbind,power.min)
  #browser()
  
  # complete power is probability all false nulls rejected when p-values not adjusted 
  # this is row 1 and column number = numfalse
  power.cmp<-rep(power.min.mat[1,M],length(power.min)) # should it be numfalse or M?
  #browser()
  
  # combine all power for all definitions
  all.power.results<-cbind(power.ind.all.mat,power.min.mat[,-M],power.cmp)
  #browser()
  # take mean of all individual power estimates
  mean.ind.power <- apply(as.matrix(all.power.results[,1:M][,MDES>0]),1,mean)
  #browser()
  
  # revise final matrix to report this mean individual power and return results  
  all.power.results<-cbind(mean.ind.power,all.power.results)
  #browser()
  colnames(all.power.results)<-c("indiv",paste0("indiv",1:M),paste0("min",1:(M-1)),"complete")
  #browser()
  rownames(all.power.results)<-c("rawp","BF","HO","BH","WY-SS")
  #browser()
  return(all.power.results)
}

