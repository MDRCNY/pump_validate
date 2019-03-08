
#  Function: gen.blocked.data  
#
#   M             number of tests/domains/outcomes                                   
#   MDES          desired minimum detectable effect size, number                
#   n.j           number of observations per block                                 
#   J             number of blocks  
#   eff.assump    assumption of whether "constant", "fixed" or "random" effects
#   rho.0_lev1    MxM correlation matrix for Level 1 residuals in models of outcomes under no treatment                       
#   rho.0_lev2    MxM correlation matrix for Level 2 residuals in models of outcomes under no treatment                       
#   rho.1_lev2    MxM correlation matrix for Level 2 effects            
#   theta         MxM matrix of correlations between residuals in Level 2 model of outcomes under no treatment and Level 2 effects                
#   ICC           value of intraclass correlation; 0 if constant or fixed effects model               
#   alpha         the significance level, 0.05 usually                           
#   Gamma.00      grand mean outcome w/o treat, held 0, vector length M       
#   sig.sq        vector length M, held at 1 for now                            
#   p.j.range     vector of minimum and maximum probabilities  of being assigned to treatment, across all sites                    
#   R2.1          R squared for mth level 1 outcome by mth level 1 covar          
#   R2.2          R squared for mth level 2 outcome by mth level 1 covar          
#   omega         vector length M, effect size variability, betw 0 and 1, 0 if no variation in effects across blocks                    
#   check         logical, if true performs checks section                       

#	Outputs: dataset with n.j*J rows, columns of D0.ij, D1.ij, D.ij, 				
#           X.ij, and X.j for each M, block.id, Treat.ij                  

#   D0.ij         potential outcomes under no treatment within each site
#   Treat.ij      assign to treatment or control group 



gen.data_blocked_i1_2<-function(M,MDES,n.j,J,rho.0_lev1,rho.0_lev2,rho.1_lev2,theta,
                           ICC,alpha,Gamma.00,sig.sq,p.j.range,R2.1,R2.2,check,omega) {
  
  if(check) cat("M", M, "MDES", MDES, "n.j", n.j, "J", J, "eff.assump", eff.assump, "rho", rho, "ICC", ICC, alpha, "Gamma.00", Gamma.00, "sig.sq", sig.sq, p.j.range, R2.1, R2.2, check, omega)
  require(MASS)
  
  ## Level 2 (J blocks)
  
  if (sig.sq!=1) tau00.sq<-(ICC*sig.sq*(1-R2.2))/((1-R2.1)*(1-ICC))
  if (sig.sq==1) tau00.sq<-ICC/(1-ICC) 
  # Compute tau11.sq based on specified omega and tau00.sq
  tau11.sq <- tau00.sq*omega
  Sigma.u.v.j <- matrix(0, 2*M, 2*M)
  for (k in 1:M) {
    for (l in 1:M) {
      Sigma.u.v.j[k,l]<-rho.0_lev2[k,l]*sqrt(tau00.sq[k])*sqrt(tau00.sq[l]) 
      Sigma.u.v.j[M+k,M+l]<-rho.1_lev2[k,l]*sqrt(tau11.sq[k])*sqrt(tau11.sq[l])
      Sigma.u.v.j[M+k,l]<-Sigma.u.v.j[k,M+l]<-theta[k,l]*sqrt(tau00.sq[k])*sqrt(tau11.sq[l])
    }
  }
  u.v.j<-mvrnorm(J, mu=rep(0,2*M), Sigma=Sigma.u.v.j)
  u.j<-u.v.j[,1:M]
  v.j<-u.v.j[,(M+1):(2*M)]
  
  # Generate block-level covariates (X.j[,m]) - 1 for each outcome domain. All are standardized with mean 0 and var 1. 
  Sigma.z.j<- rho.0_lev2
  diag(Sigma.z.j)<-1
  z.j<-mvrnorm(J, mu=rep(0,M), Sigma=Sigma.z.j)
  X.j<-rep(0,M)+z.j
  
  # Compute coeficient in front of covariate using specified R2, ICC and sig.sq
  num<- tau00.sq*R2.2  # assuming sig.sq=1
  den<-(1-R2.2)        # assuming sig.sq=1
  coef.R2.2<-sqrt(num/den)
  
  # Finally, mu.j are block level means
  mu.j<-Gamma.00 + coef.R2.2*X.j + u.j
  
  # Prepare block-level covar so have for all obs
  X.j<-as.data.frame(X.j)
  X.j[,M+1]<-1:J
  colnames(X.j)<-c(paste0("X",1:M,".j"),"block.id")
  
  ## Level 1 (n.j individuals in each of J blocks)
  
  # Here we generate potential outcomes (D0.ij) under no treatment within each site. Within each site, D0.ij have a multi-variate normal distribution 
  # with M means mu.j[j,], variation sig.sq and covariance = rho[k,l]*sig.sq[k]*sig.sq[l] - where k and l are the row/col in the cov matrix.
  
  # set up empty matrices & vectors
  D0.ij<-matrix(NA, nrow=(n.j*J),ncol=M+1)
  colnames(D0.ij)<-c(paste0("D0.M",1:M,".ij"),"block.id")
  D0.ij[,"block.id"]<-rep(1:J,each=n.j)
  D.ij<-D1.ij<-X.ij<-as.matrix(D0.ij[,1:M])
  colnames(D1.ij)<-paste0("D1.M",1:M,".ij")
  colnames(D.ij)<-paste0("D.M",1:M,".ij")
  colnames(X.ij)<-paste0("X",1:M,".ij")
  Treat.ij<-numeric(n.j*J)
  
  # within each site generate potential outcomes under no treatment (D0.ij), back out baseline covar (X.ij), assign to treatment or control group (Treat.ij)
  # and generate impacts (B.ij)
  assign('p.j',runif(J,p.j.range[1],p.j.range[2]), envir=.GlobalEnv) #this creates the variable p.j from p.j.range and assigns it to the global environment for later use
  for (j in 1:J) {
    
    wj<-which(D0.ij[,"block.id"]==j)
    
    # Generate error term r.ij
    # Create cov matrix for potential outcomes under no treatment - note resid var same for all clusters but can vary by domain/outcome
    Sigma.r.ij = matrix(0, M, M)
    for (k in 1:M) {
      for (l in 1:M) {
        Sigma.r.ij[k,l]<-rho.0_lev1[k,l]*sqrt(sig.sq[k])*sqrt(sig.sq[l])
      }
    }
    diag(Sigma.r.ij)=sig.sq
    r.ij=mvrnorm(n.j, rep(0,M), Sigma=Sigma.r.ij)
    
    # Generate individual-level covariates (X.ij) - 1 for each outcome domain. All are standardized with mean 0 and var 1. Correlation betw all pairs is rho.
    Sigma.z.ij<- rho.0_lev1
    diag(Sigma.z.ij)<-1
    z.ij<-mvrnorm(n.j, mu=rep(0,M), Sigma=Sigma.z.ij)
    X.ij[wj,]<-rep(0,M)+z.ij
    
    # Compute coeficient in front of covariate using specified R2.1, ICC and sig.sq
     den<-(1-R2.1)
     coef.R2.1<-sqrt(R2.1/den)
#    coef.R2.1<-(1-R2.1*(coef.R2.2+tau00.sq+1))/R2.1

    # Generate D0.ij
    mu.j.ij<-t(matrix(rep(mu.j[j,],n.j,),M,n.j))
    D0.ij[wj,1:M]<-mu.j.ij+coef.R2.1*X.ij[wj,]+r.ij
    
    # assign to treatment
    ww<-sample(1:n.j,n.j*p.j[j],replace=FALSE)
    Treat.ij[wj][ww]<-1
    
  } # end loop thru block
  
  
  # Next we generate effects - which may vary by outcome but for each outcome are the same across all indviduals in all blocks.
  # We then generate potential outcomes under treatment and then determine which potential outcome is observed (D.ij) based on treatment assignment (Treat.ij)
  
  Gamma.11 <- rep(0, M)
  for(m in 1:M) {
    
    Gamma.11[m]<-MDES[m]*sd(D0.ij[,m])
#    Gamma.11[m]<-MDES[m]*(sig.sq) #+tau00.sq)
#    Gamma.11[m]<-MDES[m]
    
    beta.j<-Gamma.11[m] + as.matrix(v.j)[,m] 
    beta.j.expand<-rep(beta.j,each=n.j)
    
    D1.ij[,m]=D0.ij[,m]+beta.j.expand
    D.ij[,m] <- D0.ij[,m] 
    D.ij[Treat.ij==1,m] <- D1.ij[Treat.ij==1,m]
  }
  
  #dummies for including X.j and X.ij, 
  #add a column of dummies for each M since R2.1 and R2.2 are vectors length M
  incX.j = matrix(0, nrow=dim(X.j)[1], ncol=M)
  colnames(incX.j) = paste0("incX",1:M,".j")
  incX.ij = matrix(0, nrow=dim(X.ij)[1], ncol=M)
  colnames(incX.ij) = paste0("incX",1:M, ".ij")
  for (m in 1:M) {
    incX.j[,m] = 1*(R2.2[m]>0)
    incX.ij[,m] = 1*(R2.1[m]>0)
  }
  X.j <- cbind(X.j, incX.j)
  X.ij <- cbind(X.ij, incX.ij)
  
  #save and name simulated data output
  output.temp<- data.frame(D0.ij,D1.ij,D.ij,Treat.ij,X.ij)
  output<-merge(output.temp,X.j,by="block.id")
  
  #  filename = paste0("gendata_M", M,"_MDES", MDES, "_J", J, "_nj", n.j, "_ICC", ICC, "_rho", rho, ".csv")
  
  # CHECKS
  if (check) {
    
    # correlations between block-level means
    cor(mu.j)
    
    # variance of block-level means
    apply(mu.j,2,var)
    
    # grand means of block-level means (should be equal to Gamma.00)
    apply(mu.j,2,mean)
    
    # calculated true coef on block-level covariates
    coef.R2.2
    
    # check of estimated R2.2, coef.R2.1 and var of resid
    summary(lm(mu.j[,1]~X.j[,1]))
    
    # correlation between between indiv levels of outcome under no treatment
    #cor(output[output$block.id==j,c("D.M1.ij","D.M2.ij")])
    
    # compute block-level means from indiv-level measures
    agg.block.mean<-aggregate(output,by=list(output$block.id),FUN="mean")
    
    # variances of block-level outcomes under no treatment
    apply(agg.block.mean,2,var)
    
    # check of effects
    apply(agg.block.mean[,paste0("D1.M",1:M,".ij")]-agg.block.mean[,paste0("D0.M",1:M,".ij")],2,mean)
    
    # estimated ICC from lme
    require(nlme)
    lme.dat<-groupedData(D0.M1.ij~1|block.id,data=output)
    lme.test<-lme(lme.dat)
    varests <- as.numeric(VarCorr(lme.test)[,"Variance"])  # vector of variance estimates
    varests
    ICC.calc <- varests[1]/sum(varests)
    ICC.calc
  }
  
  return(output)
}