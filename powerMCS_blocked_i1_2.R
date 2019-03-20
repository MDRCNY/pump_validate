# Functions for estimating statistical power by Monte Carlo simulations, 
# relying on data generation done by gen.data_blocked_i1_2() in gen.data_blocked_i1_2.R

#	Contents: 	make.dummies; make.model; get.pval; get.rawp		            
#							get.output; get.adjp; get.rejects; est.power					                  


###########################################################################
#	Function: make.dummies		Inputs:	dat, clusterby, n.j, J	              #
#		a dataset (dat), 								                                      #
#		a column name as a string to cluster by (clusterby), 		              #
#		n.j, and J 									                                          #
#	Outputs: dummies (column names), lmedat.fixed (data.frame)		          #
###########################################################################

make.dummies <- function(dat, clusterby, n.j, J){
  block.rep<-matrix(data=rep(dat[,clusterby],n.j),
                    nrow=length(dat[,clusterby]),ncol=J)
  colnum<-seq(1:J)
  block.dum.fn<-function(...) {1*(...==colnum)}
  block.dum<-t(apply(block.rep,1,block.dum.fn))
  colnames(block.dum)<-paste("block",1:J,sep="")
  lmedat.fixed<-cbind(dat,block.dum)
  dummies<-paste(colnames(block.dum[,-1]),collapse="+")
  
  return(list("dnames"=dummies, "fixdat"=lmedat.fixed))
}


###########################################################################
#  Function: make.model	Inputs:dat,dummies,funct	                        #
#		a reshaped dataset (dat)-->data for one m 			                      #
#		dummies, string of dummy names in formula, if function needs          #
#		funct, the model type to use as string				                        #
#         fixfastLm, fixlmer, or random                                   #
#	Outputs: model object of type function					                        #
#	Notes: dummies can be output of make.dummies $dnames			              #
###########################################################################

make.model <- function(dat, dummies, funct){
    if (funct == 'constant') {
      form <- as.formula(paste0("D~Treat.ij+Covar.j+Covar.ij+", dummies))
      mod <- fastLm(form, data=dat)
    }
    if (funct == 'fixed') { 
      form <- as.formula("D~Treat.ij+Covar.j+Covar.ij+(1|block.id)")
      mod <- lmer(form, data=dat)
    }
    if (funct == 'random') {
      form <- as.formula(paste0("D~Treat.ij+Covar.j+Covar.ij+(Treat.ij|block.id)"))   
      suppressMessages(mod <- lmer(form, data=dat))
      }
    return(mod)
}

###########################################################################
#	Function: get.pval 	Inputs: mod						                              #
#		a model object, mod							                                      #
#	Outputs: pvalue 									                                      #
###########################################################################

get.pval <- function(mod) {
  if(class(mod)=="lmerMod") {
     pval <-(1-pnorm(abs(summary(mod)$coefficients["Treat.ij","t value"])))*2
  } else { #if class(mod) is "fastLm"
    pval <- summary(mod)$coef["Treat.ij", "Pr(>|t|)"]
  }
  return(pval)
}

get.tstat <- function(mod) {
  if(class(mod)=="lmerMod") {
    tstat <-summary(mod)$coefficients["Treat.ij","t value"]
  } else { #if class(mod) is "fastLm"
    tstat <- summary(mod)$coef["Treat.ij", "t value"]
  }
  return(tstat)
}


###########################################################################
#	Function: get.rawp	Inputs: mdat, funct, n.j, J      	                  #                                       			 
#   mdat, a single dataset from list of S datasets as a list length M     #
#		funct, a string "random", "fixfastLm", or "fixlmer"					          #
#		n.j and J, number of obs at a site and number of sites, respectively	#
#												                                                  #
# Calls: make.dummies, make.model, get.pval                               #
#	Outputs: matrix of raw p-values for a single sample		                  #
#	Notes: gets raw p-vals for a single dataset and funct at a time	        #
###########################################################################

get.rawp <- function(mdat, funct, n.j, J) {
  if (funct == "random") {mods = lapply(mdat, function(m) make.model(m, NULL, funct))}
  else { #if funct == "fixfastLm" or "fixlmer"
    mdums = lapply(mdat, function(m) make.dummies(m, "block.id", n.j, J))
    mods = lapply(mdums, function(m) make.model(m$fixdat, m$dnames, funct))
  }
  #mods is a list of m models
  rawp = sapply(mods, function(x) get.pval(x))
  return(rawp)
}

get.rawt <- function(mdat, funct, n.j, J) {
  if (funct == "random") {mods = lapply(mdat, function(m) make.model(m, NULL, funct))}
  else { #if funct == "fixfastLm" or "fixlmer"
    mdums = lapply(mdat, function(m) make.dummies(m, "block.id", n.j, J))
    mods = lapply(mdums, function(m) make.model(m$fixdat, m$dnames, funct))
  }
  #mods is a list of m models
  rawt = sapply(mods, function(x) get.tstat(x))
  return(rawt)
}



###########################################################################
#  Function: makelist.samp  Inputs:   M, samp                             #
#		    M, number of domains                                              #
#		    samp, a single sample of data                                     #
#                                                                         #
#	Outputs: list length M of data by domain                 				        #
#	Notes:                                                        	        #
###########################################################################

makelist.samp <-function(M, samp) {
  #a list length M for a sample of data, each entry is a dataset for a single domain
  mdat <- lapply(1:M, function(m) samp[,c("block.id", "Treat.ij", grep(as.character(m), names(samp), value=TRUE))])
  mdat <- lapply(mdat, function(x) 
    data.frame(D=x[,grep("D.M", names(x), value=TRUE)], #outcome
               Covar.j = apply(x[,grep("X[0-9].j", names(x), value=TRUE)], 1, function(y) y[1]*y[2]), #site level covariate
               Covar.ij = apply(x[,grep("X[0-9].ij", names(x), value=TRUE)], 1, function(h) h[1]*h[2]), #individual level covariate
               Treat.ij = x[,"Treat.ij"],
               block.id = x[,"block.id"]))
  return(mdat)
}


###########################################################################
#	Function: est.power			
#												                                                  #
# input: 
#   procs: a vector of strings for adjustment procedures
#   M: number of tests/domains/outcomes
#   MDES: desired minimum detectable effect size, number 
#   n.j: number of observations per block 
#   J: number of blocks
#   rho.0_lev1: MxM matrix of correlations for Level 1 residuals in models of outcomes under no treatment
#   rho.0_lev2: MxM matrix of correlations for Level 2 residuals in models of outcomes under no treatment
#   rho.1_lev2: MxM matrix of correlations for Level 2 effects        
#   theta: MxM matrix of correlations between residuals in Level 2 model outcomes under no treatment and Level 2 effects 
#   ICC: a number, intraclass correlation; 0 if fixed model 
#   alpha: the significance level, 0.05 usually 
#   Gamma.00: grand mean outcome w/o treat, held 0, vector length M
#   sig.sq: vector length M, held at 1 for now
#   p.j.range: vector of minimum and maximum probabilities of being assigned to treatment, across all sites
#   R2.1: R squared for mth level 1 outcome by mth level 1 covar  
#   R2.2: R squared for mth level 2 outcome by mth level 1 covar  
#   check: 
#   omega: effect size variability, between 0 and 1, 0 if no variation in effects across blocks, vector length M
#   funct: a string "random", "fixfastLm", or "fixlmer" 
#   S: number of samples for power calc      
#   ncl: number of clusters set at 24 because that is the max cpus on datalab
#   B: number of permutations for WY       
#   maxT: 
#   all other inputs for gen.blocked.data      
# Calls:  get.adjp, get.rejects, gen.blocked.data, makelist.samp,         
#         get.rawp                                                        
#	Outputs:  a matrix of power calculations                                
###########################################################################

est.power <- function(procs, M,DMDES,n.j,J,rho.0_lev1, rho.0_lev2,rho.1_lev2,theta,ICC,alpha, Gamma.00,sig.sq, p.j.range, R2.1, R2.2, check, omega, funct, S, ncl, B, maxT) {
 
  if (M==1) {
    print("Multiple testing corrections are not needed when M=1")
    procs <- NULL
  }

  power.results <- matrix(NA, nrow=(length(procs)+1), ncol=M+5)
  colnames(power.results) = c(paste0("D", 1:M, "indiv"), "min", "1/3", "1/2","2/3", "full")
  rownames(power.results) = c("rawp", procs)
  
  nulls <-which(DMDES==0)
  alts <-which(DMDES!=0)
  
  adjp.proc <- array(0, c(S, M, length(procs)+1))
  dimnames(adjp.proc) <- list(NULL, NULL, c("rawp", procs))
  
  names(adjp.proc) <- c("rawp", procs)
  
  px<-S/100
  rawt.all<-matrix(NA,S,M)
  for (s in 1:S) {
    t1<-Sys.time()
    if (s%%px==0){print(paste0("Now processing sample ", s, " of ", S))}
    samp <- gen.data_blocked_i1_2(M,DMDES,n.j,J,rho.0_lev1, rho.0_lev2,rho.1_lev2,theta,ICC,alpha,Gamma.00,sig.sq,p.j.range,R2.1,R2.2,check,omega)
    #if (s==1) print(c(sd(samp$D0.M1.ij),sd(samp$D.M1.ij)))
    mdat <- makelist.samp(M, samp) #list length M
    rawp <- get.rawp(mdat, funct, n.j, J) #vector length M
    rawt <- get.rawt(mdat, funct, n.j, J) #vector length M
    rawt.all[s,]<-rawt
    for (p in 1:(length(procs)+1)) {
      if (p==1) {
        pvals <-rawp
        proc <- "rawp"
      } else {
        t11<-Sys.time()
        proc <- procs[p-1]
        pvals <- get.adjp(rawp, rawt, proc, alpha, B, ncl, mdat, maxT)
        t21<-Sys.time()
        if (s==1) {print(paste("One sample of ", proc, " took ", t21-t11))}
      }
      adjp.proc[s,,proc] = pvals
    }
    t2<-Sys.time()
   if (s==1) {print(paste("Expected time diff of", (t2-t1)*S, "and expected finish at", t1+(t2-t1)*S,"for S =", S, sep=" "))}
   else if (s%%px==0) {print(difftime(t2, t1))}
  }
  for (p in 1:(length(procs)+1)) {
    if (M==1) {power.results[p,1:M]<-mean(adjp.proc[,,p]<alpha)}
    else {power.results[p, 1:M] <- apply(adjp.proc[,,p], 2, function(x) mean(x<alpha))}
    rejects <- get.rejects(adjp.proc[,,p], alpha)
    if (M==1) {
      if (alts!=0) num.t.pos=rejects
    }
    else if (length(alts)==1) {num.t.pos<-rejects[,alts]}
    else {num.t.pos <- apply(rejects[,alts], 1, sum)}
    power.results[p, "min"] <- mean(1*(num.t.pos>0))
    power.results[p, "1/3"] <- mean(1*(num.t.pos>=(1/3)*M))
    power.results[p, "1/2"] <- mean(1*(num.t.pos>=(1/2)*M))
    power.results[p, "2/3"] <- mean(1*(num.t.pos>=(2/3)*M))
    power.results[p, "full"] <- mean(1*(num.t.pos==M))
  }
  return(power.results)
}


###########################################################################
#  Function: get.adjp 	Inputs: rawp, proc, alpha				                  #
#		a matrix nrow=S ncol=M of raw p-values							                  #
#  	a string for a single proc            							                  #
#  	a number, alpha, should be 0.05 in most cases	 			                  #
#	Outputs: MxS matrix of adjusted p-values for a single proc 	            #
###########################################################################

get.adjp <- function(rawp, rawt, proc, alpha, B, ncl, mdat, maxT) {
  if(proc=="WY"){
    tw1 <- Sys.time()
    adjp.proc <- adjust.WY(data=mdat, B=B, rawp=rawp, rawt=rawt, ncl=ncl, clustered=TRUE, clusterby='block.id', funct, maxT)[,"WY"]
    tw2 <- Sys.time()
   # print(difftime(tw2, tw1))
  }
  else {
    #return a matrix with m columns (domains) and b rows (samples)
    #this needs rawp to be a matrix with m columns and was designed for all samples to be a row.
    mt.out <-mt.rawp2adjp(rawp, proc, alpha)
    adjp.proc <- mt.out$adjp[order(mt.out$index), proc]
      #mt.rawp2adjp(rawp, proc, alpha)$adjp[,proc]
  }
  return(adjp.proc)
}

###########################################################################
#  Function: get.rejects   Inputs: adjp, alpha  				                  #
#		a matrix nrow=S ncol=M of adjusted p-values, from get.adjp			      #
#  	a number, alpha, should be 0.05 in most cases	 			                  #
#	Outputs: MxS matrix of 1 and 0 indicating rejecting the null            #
###########################################################################

get.rejects <- function(adjp, alpha) {
  #return a matrix of 1 and 0 (for true/false <alpha)
  rejects <- 1*(adjp<alpha)
}


