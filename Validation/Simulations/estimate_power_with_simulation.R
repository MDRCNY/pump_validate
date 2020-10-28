#'  Function: est_power_sim				                                       
#'  
#'  Function to estimate statistical power using simulations (on t-statistics)
#'
#' @param user.params.list List of user-supplied parameters
#' @param sim.params.list List of simulation parameters
#' @param design RCT design (see list/naming convention)
#' @param cl cluster object for parallel computing
est_power_sim <- function(user.params.list, sim.params.list, design, cl = NULL) {
  
  # convert user-inputted parameters into model parameters
  model.params.list <- convert.params(user.params.list)
  if(sim.params.list[['check']]){ print(model.params.list) }
  
  # save out some commonly used variables
  M <- model.params.list[['M']]
  S <- sim.params.list[['S']]
  J <- model.params.list[['J']]
  p.j <- sim.params.list[['p.j']]
  alpha <- sim.params.list[['alpha']]
  procs <- sim.params.list[['procs']]

  if(M == 1) {
    print("Multiple testing corrections are not needed when M=1")
    procs <- "Bonferroni"
  }

  # list of adjustment procedures
  adjp.proc <- array(0, c(S, M, length(procs) + 1))
  dimnames(adjp.proc) <- list(NULL, NULL, c("rawp", procs))
  names(adjp.proc) <- c("rawp", procs)
  
  px <- 100
  rawt.all <- matrix(NA, S, M)
  # begin loop through all samples to be generated
  for (s in 1:S) {
    
    t1 <- Sys.time()
    if (s %% px == 0){ message(paste0("Now processing sample ", s, " of ", S)) }
    
    # generate full, unobserved sample data
    samp.full <- gen_full_data(model.params.list, check = sim.params.list[['check']])
    S.jk <- samp.full$ID$S.jk
    S.k  <- samp.full$ID$S.k
    
    # blocked designs
    if(design %in% c('blocked_i1_2c', 'blocked_i1_2f', 'blocked_i1_2r')) {
      T.ijk <- randomizr::block_ra( S.jk, prob = p.j )
    # cluster designs
    } else if(design %in% c('simple_c2_2r'))  { 
      T.ijk <- randomizr::cluster_ra( S.jk, prob = p.j )
    } else {
      stop(print(paste('Design', design, 'not implemented yet')))
    }
    
    # convert full data to observed data
    samp.obs = samp.full
    samp.obs$Yobs = gen_Yobs(samp.full, T.ijk)
    
    mdat <- makelist.samp(M, samp.obs, T.ijk, model.params.list, design = design) # list length M
    rawp <- get.rawp(mdat, design = design, n.j = model.params.list[['n.j']], J = J) # vector length M
    rawt <- get.rawt(mdat, design = design, n.j = model.params.list[['n.j']], J = J) # vector length M
    rawt.all[s,] <- rawt
    
    # loop through adjustment procedures (adding 'rawp' as default in all cases)
    for (p in 1:(length(procs) + 1)) {
      if (p == 1) {
        pvals <-rawp
        proc <- "rawp"
      } else {
        t11 <- Sys.time()
        
        proc <- procs[p-1]
        pvals <- get.adjp(proc, rawp, rawt, mdat, sim.params.list, model.params.list, design, cl)
        
        t21 <- Sys.time()
        if (s == 1) { message(paste("One sample of", proc, "took", difftime(t21, t11, units = 'secs'))) }
      }
      adjp.proc[s,,proc] = pvals
    }
    
    t2 <- Sys.time()
    if (s == 1) {
      message(paste(
        "Current time:", t2,
        "\nExpected time diff for simulation of", round(S*(difftime(t2, t1, units = 'secs')[[1]])/60,2),
        "minutes.\nExpected finish for simulation at", t1 + (t2 - t1) * S,"for S =", S, sep =" ")
      )
    }
    else if (s %% px == 0) { message(paste('Progress: iteration', s, 'of', S, 'complete, running time:', difftime(t2, t1))) }
  } # end loop through samples
  
  return(adjp.proc)
}

#'  Function: calc_power                                       
#'  
#'  Calculates power based on adjusted p values
#'
#' @param adjp.proc adjusted p values for all procedures
#' @param user.params.list
#' @param sim.params.list
#' 
#' @return formatted simulation results

calc_power <- function(adjp.proc, user.params.list, sim.params.list)
{
  # save out some commonly used variables
  M <- user.params.list[['M']]
  S <- sim.params.list[['S']]
  alpha <- sim.params.list[['alpha']]
  procs <- sim.params.list[['procs']]
  
  power.results <- matrix(NA, nrow = (length(procs) + 1), ncol = M + 5)
  colnames(power.results) = c(paste0("D", 1:M, "indiv"), "min", "1/3", "1/2","2/3", "full")
  rownames(power.results) = c("rawp", procs)
  se.power <- CI.lower.power <- CI.upper.power <- power.results
  
  alts <- which(user.params.list[['ATE_ES']] != 0)
  
  for (p in 1:(length(procs) + 1)) {
    if (M == 1) {
      mn.lt.alpha <- mean(adjp.proc[,,p]<alpha)
      power.results[p,1:M] <- mn.lt.alpha
      se.power[p,1:M] <- sqrt(mn.lt.alpha * (1 - mn.lt.alpha)/S)
    }
    else {
      power.results[p, 1:M] <- apply(adjp.proc[,,p, drop = FALSE], 2, function(x) mean(x < alpha))
      # se.power[p, 1:M] <- apply(adjp.proc[,,p], 2, function(x) {
      #   sqrt(0.25/S) 
      # })
      # se.power[p, 1:M] <- apply(adjp.proc[,,p], 2, function(x) {
      #   sqrt(mean(x < alpha)*(1 - mean(x < alpha))/S) 
      # })
    }
    rejects <- get.rejects(adjp.proc[, , p, drop = FALSE], alpha)
    if (M == 1) {
      if (alts != 0) num.t.pos = rejects
    } else if (length(alts) == 1) {
      num.t.pos <- rejects[, alts, , drop = FALSE]
    } else {
      num.t.pos <- apply(rejects[,alts, , drop = FALSE], 1, sum)
    }
    power.results[p, "min"] <- mean(1 * (num.t.pos > 0))
    # se.power[p, "min"] <- sqrt(mean(1 * (num.t.pos > 0)) * (1 - mean(1 * (num.t.pos > 0)))/S)
    power.results[p, "1/3"] <- mean(1 * (num.t.pos >= (1/3) * M))
    # se.power[p, "1/3"] <- sqrt(mean(1 * (num.t.pos >= (1/3) * M)) * (1 - mean(1 * (num.t.pos >= (1/3) * M)))/S)
    power.results[p, "1/2"] <- mean(1 * (num.t.pos >= (1/2) * M))
    # se.power[p, "1/2"] <- sqrt(mean(1 * (num.t.pos >= (1/2) * M)) * (1 - mean(1 * (num.t.pos >= (1/2) * M)))/S)
    power.results[p, "2/3"] <- mean(1 * (num.t.pos >= (2/3) * M))
    # se.power[p, "2/3"] <- sqrt(mean(1 * (num.t.pos >= (2/3) * M)) * (1 - mean(1 * (num.t.pos >= (2/3) * M)))/S)
    power.results[p, "full"] <- mean(1 * (num.t.pos == M))
    # se.power[p, "full"] <- sqrt(mean(1 * (num.t.pos == M)) * (1 - mean(1 * (num.t.pos == M)))/S)
    
    se.power[p,] <- sqrt(0.25/S) 
  }
  CI.lower.power <- power.results - 1.96 * (se.power)
  CI.upper.power <- power.results + 1.96 * (se.power)
  
  adj_power <- list(power.results, CI.lower.power, CI.upper.power)
  names(adj_power) <- c("adjusted_power", "ci_lower", "ci_upper")
  
  # reformat this for easier use
  sim_results <- format_sim_results(adj_power)
  return(sim_results)
}

# --------------------------------------------------------------------- #
#  Function: make.model	Inputs:dat,dummies      	                        #
#		a reshaped dataset (dat)-->data for one m 			                      #
#		dummies, string of dummy names in formula, if function needs          #
#		funct, the model type to use as string				                        #
#         fixfastLm, fixlmer, or random                                   #
#	Outputs: model object of type function					                        #
#	Notes: dummies can be output of make.dummies $dnames			              #
# --------------------------------------------------------------------- #

make.model<-function(dat, dummies, design) {

  if (design == "blocked_i1_2c") {
    mmat <- cbind(dat[,c("Treat.ij", "Covar.j", "Covar.ij")],
                  dat[,grep("block[0-9]", colnames(dat))])
    mod <- fastLm(mmat, dat[,"D"])
  } else if (design == "blocked_i1_2f") {
    form <- as.formula("D~1+Treat.ij+Covar.j+Covar.ij+(1|block.id)")
    mod <- pkgcond::suppress_messages(lmer(form, data = dat))
  } else if (design == "blocked_i1_2r") {
    form <- as.formula(paste0("D~1+Treat.ij+Covar.j+Covar.ij+(1+Treat.ij|block.id)"))
    mod <- pkgcond::suppress_messages(lmer(form, data = dat))
  } else if (design == "simple_c2_2r") {
    form <- as.formula(paste0("D~Treat.j+Covar.j+Covar.ij+(1|cluster.id)"))
    #    form <- as.formula(paste0("D~Treat.j+Covar.j+(1+Covar.ij|cluster.id)"))
    #    mod <- lmer(form, data=dat, control = lmerControl(optimizer="bobyqa",calc.derivs = FALSE))
    mod <- lmer(form, data=dat, control = lmerControl(optimizer="nloptwrap",calc.derivs = FALSE))
  } else {
    stop(paste('Unknown design:', design)) 
  }
  return(mod)
}

# --------------------------------------------------------------------- #
#	Function: make.dummies		Inputs:	dat, clusterby, n.j, J
#		a dataset (dat),
#		a column name as a string to cluster by (clusterby), ZH: Can this be blockby?    
#		n.j, and J 									                                                     
#	Outputs: dummies (column names), lmedat.fixed (data.frame)		                    
# --------------------------------------------------------------------- #

make.dummies <- function(dat, blockby, n.j, J){

  # dat = mdat[[1]]; blockby= "block.id"

  block.rep <- matrix(
    data = rep(dat[,blockby], n.j),
    nrow = length(dat[,blockby]), ncol = J
  )
  colnum<-seq(1:J)
  block.dum.fn<-function(...) {1*(...==colnum)}
  
  block.dum <- t(apply(block.rep, 1, block.dum.fn))
  colnames(block.dum)<-paste("block",1:J,sep="")
  lmedat.fixed<-cbind(dat,block.dum)
  dummies<-paste(colnames(block.dum[,-1]),collapse="+")

  return(list("dnames"=dummies, "fixdat"=lmedat.fixed))
}


# --------------------------------------------------------------------- #
#	Function: get.pval 	Inputs: mod						                              #
#		a model object, mod							                                      #
#	Outputs: pvalue 									                                      #
# --------------------------------------------------------------------- #

get.pval.Level1 <- function(mod) {

  if(class(mod)=="lmerMod") {
    pval <-(1-pnorm(abs(summary(mod)$coefficients["Treat.ij","t value"])))*2
  }
  if (class(mod)=="fastLm") {
    pval <- summary(mod)$coef["Treat.ij", "Pr(>|t|)"]
  }
  return(pval)
}

get.tstat.Level1 <- function(mod) {

  if(class(mod)=="lmerMod") {
    tstat <- summary(mod)$coefficients["Treat.ij","t value"]
  }
  if (class(mod)=="fastLm") {
    tstat <- summary(mod)$coef["Treat.ij", "t value"]
  }
  return(tstat)
}

get.pval.Level2 <- function(mod) {

  if(class(mod)=="lmerMod") {
    pval <- (1-pnorm(abs(summary(mod)$coefficients["Treat.j","t value"])))*2
  }
  if (class(mod)=="fastLm") {
    pval <- summary(mod)$coef["Treat.j", "Pr(>|t|)"]
  }
  return(pval)
}

get.tstat.Level2 <- function(mod) {

  if(class(mod)=="lmerMod") {
    tstat <-summary(mod)$coefficients["Treat.j","t value"]
  }
  if (class(mod)=="fastLm") {
    tstat <- summary(mod)$coef["Treat.j", "t value"]
  }
  return(tstat)
}

# --------------------------------------------------------------------- #
#	Function: get.rawp	Inputs: mdat, design, n.j, J      	                  #
#   mdat, a single dataset from list of S datasets as a list length M     #
#		p, a string "random", "fixfastLm", or "fixlmer"					          #
#		n.j and J, number of obs at a site and number of sites, respectively	#
#												                                                  #
# Calls: make.dummies, make.model, get.pval                               #
#	Outputs: matrix of raw p-values for a single sample		                  #
#	Notes: gets raw p-vals for a single dataset and funct at a time	        #
# --------------------------------------------------------------------- #

get.rawp <- function(mdat, design, n.j, J) {
  
  # n.j = model.params.list[['n.j']]

  if (design %in% c("blocked_i1_2c","blocked_i1_2f")) {
    mdums = lapply(mdat, function(m) make.dummies(m, "block.id", n.j, J))
    mods = lapply(mdums, function(m) make.model(m$fixdat, m$dnames, design))
    rawp = sapply(mods, function(x) get.pval.Level1(x))
  }
  if (design == "blocked_i1_2r") {
    mods = lapply(mdat, function(m) make.model(m,NULL,design))
    rawp = sapply(mods, function(x) get.pval.Level1(x))
  }
  if (design == "simple_c2_2r") {
    mods = lapply(mdat, function(m) make.model(m, NULL, design))
    rawp = sapply(mods, function(x) get.pval.Level2(x))
  }

  return(rawp)
}

get.rawt <- function(mdat, design, n.j, J) {

  if (design %in% c("blocked_i1_2c","blocked_i1_2f")) {
    mdums = lapply(mdat, function(m) make.dummies(m, "block.id", n.j, J))
    mods = lapply(mdums, function(m) make.model(m$fixdat, m$dnames, design))
    rawt = sapply(mods, function(x) get.tstat.Level1(x))
  } else if (design == "blocked_i1_2r") {
    mods = lapply(mdat, function(m) make.model(m,NULL,design))
    rawt = sapply(mods, function(x) get.tstat.Level1(x))
  } else if (design == "simple_c2_2r") {
    mods = lapply(mdat, function(m) make.model(m,NULL,design))
    rawt = sapply(mods, function(x) get.tstat.Level2(x))
  } else {
    stop(paste('Error: unknown design', design))
  }

  return(rawt)
}

# --------------------------------------------------------------------- #
#  Function: makelist.samp
# Inputs 
#		    M, number of domains                                              #
#		    samp, a single sample of data                                     #
#                                                                         #
#	Outputs: list length M of data by domain                                #
#          each entry is a dataset for a single domain               			#
#	Notes:                                                        	        #
# --------------------------------------------------------------------- #

makelist.samp <-function(M, samp.obs, T.ijk, model.params.list, design) {

  if (design %in% c("blocked_i1_2c", "blocked_i1_2f", "blocked_i1_2r")) {

    mdat.rn <- NULL
    for(m in 1:M)
    {
      mdat.rn[[m]] <- data.frame(
        D        = samp.obs[['Yobs']][,m],
        Covar.j  = samp.obs[['X.jk']][,m],
        Covar.ij = samp.obs[['C.ijk']][,m],
        Treat.ij = T.ijk,
        block.id = as.factor(samp.obs$ID$S.jk)
      )
    }

  } else if (design %in% c("simple_c2_2r")){

    mdat.rn <- NULL
    for(m in 1:M)
    {
      mdat.rn[[m]] <- data.frame(
        D          = samp.obs[['Yobs']][,m],
        Covar.j    = samp.obs[['X.jk']][,m],
        Covar.ij   = samp.obs[['C.ijk']][,m],
        Treat.ijk  = T.ijk,
        cluster.id = samp.obs$ID$S.jk
      )
    }
  } else {
    stop(paste('Error: unknown design', design))
  }
  return(mdat.rn)
}



# --------------------------------------------------------------------- #
#  Function: get.adjp 	Inputs: rawp, proc, alpha				                  #
#		a matrix nrow=S ncol=M of raw p-values							                  #
#  	a string for a single proc            							                  #
#  	a number, alpha, should be 0.05 in most cases	 			                  #
#	Outputs: MxS matrix of adjusted p-values for a single proc 	            #
# --------------------------------------------------------------------- #

get.adjp <- function(proc, rawp, rawt, mdat, sim.params.list, model.params.list, design, cl = NULL) {

  if(proc == "WY-SD" | proc == "WY-SS"){
    #print(paste0("working on ", proc, " with ", B, " permutations"))
    tw1 <- Sys.time()
    adjp.proc <- adjust_WY(
      data = mdat, rawp = rawp, rawt = rawt,
      proc = proc,
      clustered = TRUE, blockby = 'block.id',
      sim.params.list = sim.params.list,
      model.params.list = model.params.list,
      design = design,
      cl = cl
    )[,"WY"]
    tw2 <- Sys.time()
    # print(difftime(tw2, tw1))
  }
  else {
    # return a matrix with m columns (domains) and b rows (samples)
    # this needs rawp to be a matrix with m columns and was designed for all samples to be a row.
    mt.out <- mt.rawp2adjp(rawp, proc, sim.params.list[['alpha']])
    adjp.proc <- mt.out$adjp[order(mt.out$index), proc]
  }
  return(adjp.proc)
}

# --------------------------------------------------------------------- #
#  Function: get.rejects   Inputs: adjp, alpha  				                  #
#		a matrix nrow=S ncol=M of adjusted p-values, from get.adjp			      #
#  	a number, alpha, should be 0.05 in most cases	 			                  #
#	Outputs: MxS matrix of 1 and 0 indicating rejecting the null            #
# --------------------------------------------------------------------- #

get.rejects <- function(adjp, alpha) {
  # return a matrix of 1 and 0 (for true/false <alpha)
  rejects <- 1*(adjp<alpha)
}
