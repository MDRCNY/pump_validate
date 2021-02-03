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
  
  # save out some commonly used variables
  M <- model.params.list[['M']]
  S <- sim.params.list[['S']]
  Tbar <- sim.params.list[['Tbar']]
  procs <- sim.params.list[['procs']]

  if(M == 1) {
    print("Multiple testing corrections are not needed when M = 1")
    procs <- "Bonferroni"
  }

  # list of adjustment procedures
  adjp.proc <- array(0, c(S, M, length(procs) + 1))
  dimnames(adjp.proc) <- list(NULL, NULL, c("rawp", procs))
  names(adjp.proc) <- c("rawp", procs)
  
  # how often to print error messages
  px <- 100
  
  # begin loop through all samples to be generated
  num.singular.raw <- 0
  t1 <- Sys.time()
  for (s in 1:S) {
    
    if (s %% px == 0){ message(paste0("Now processing sample ", s, " of ", S)) }
    
    # generate full, unobserved sample data
    samp.full <- gen_full_data(model.params.list)
    S.id <- samp.full$ID$S.id
    D.id  <- samp.full$ID$D.id
    
    # blocked designs
    if(design %in% c('blocked_i1_2c', 'blocked_i1_2f', 'blocked_i1_2r', 'blocked_i1_3r'))
    {
      T.x <- randomizr::block_ra( S.id, prob = Tbar )
    # cluster designs
    } else if(design %in% c('simple_c2_2r'))
    { 
      T.x <- randomizr::cluster_ra( S.id, prob = Tbar )
    } else if(design %in% c('simple_c3_3r'))
    {
      T.x <- randomizr::cluster_ra( D.id, prob = Tbar )
    # blocked cluster designs
    } else if(design %in% c('blocked_c2_3f', 'blocked_c2_3r'))
    {
      T.x <- randomizr::block_and_cluster_ra( blocks = D.id, clusters = S.id, prob = Tbar )
    } else
    {
      stop(print(paste('Design', design, 'not implemented yet')))
    }
    
    # convert full data to observed data
    samp.obs <- samp.full
    samp.obs$Yobs <- gen_Yobs(samp.full, T.x)
    
    dat.all <- makelist.samp(samp.obs, T.x) # list length M
    rawpt.out <- get.rawpt(dat.all, design = design, user.params.list = user.params.list)
    rawp <- sapply(rawpt.out[['rawpt']], function(s){ return(s[['pval']])})
    rawt <- sapply(rawpt.out[['rawpt']], function(s){ return(s[['tstat']])})
    
    # track how many warnings occur
    num.singular.raw.s <- rawpt.out[['num.singular']]
    num.singular.raw <- num.singular.raw + num.singular.raw.s
    
    # loop through adjustment procedures (adding 'rawp' as default in all cases)
    for (p in 1:(length(procs) + 1)) {
      if (p == 1) {
        pvals <- rawp
        proc <- "rawp"
      } else {
        t11 <- Sys.time()
        
        proc <- procs[p-1]
        pvals <- get.adjp(
          proc = proc, rawp = rawp, rawt = rawt,
          dat.all = dat.all, S.id = S.id, D.id = S.id,
          sim.params.list = sim.params.list, model.params.list = model.params.list,
          design = design, cl = cl
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
  return(adjp.proc)
}

#'  Function: calc_power                                       
#'  
#'  Calculates power based on adjusted p values
#'
#' @param adjp.proc adjusted p values for all procedures
#' @param alpha
#' 
#' @return formatted simulation results

calc_power <- function(adjp.proc, alpha)
{

  procs <- dimnames(adjp.proc)[[3]]
  S <- dim(adjp.proc)[1]
  M <- dim(adjp.proc)[2]
  
  power.results <- matrix(NA, nrow = length(procs), ncol = M + M + 2)
  if(M == 1)
  {
    colnames(power.results) = c(paste0("D", 1:M, "indiv"), "indiv.mean", "min", "complete")
  } else
  {
    colnames(power.results) = c(paste0("D", 1:M, "indiv"), "indiv.mean", "min", paste0("min",1:(M-1)), "complete")
  }
  rownames(power.results) = procs
  
  alts <- which(user.params.list[['ATE_ES']] != 0)
  
  for (p in 1:length(procs)) {
    # calculate d-minimal power
    power.results[p, 1:M] <- apply(adjp.proc[,,p,drop = FALSE], 2, function(x) mean(x < alpha))

    # calculate min, min1, min2, etc. and complete power
    rejects <- get.rejects(adjp.proc[, , p], alpha)
    rawp.rejects <- get.rejects(adjp.proc[, , 1], alpha)

    num.t.pos <- apply(rejects[, alts, drop = FALSE], 1, sum)
    num.t.pos.rawp <- apply(rawp.rejects[, alts, drop = FALSE], 1, sum)

    power.results[p, "min"] <- mean(1 * (num.t.pos > 0))
    if(M > 1)
    {
      for(m in 1:(M-1))
      {
        power.results[p, paste0("min", m)] <- mean(1 * (num.t.pos >= m))
      }
    }
    power.results[p, "complete"] <- mean(1 * (num.t.pos.rawp == M))
    
    # se.power[p, 1:M] <- apply(adjp.proc[,,p], 2, function(x) {
    #   sqrt(mean(x < alpha)*(1 - mean(x < alpha))/S) 
    # })
    # se.power[p, "min"] <- sqrt(mean(1 * (num.t.pos > 0)) * (1 - mean(1 * (num.t.pos > 0)))/S)
    # power.results[p, "1/3"] <- mean(1 * (num.t.pos >= (1/3) * M))
    # # se.power[p, "1/3"] <- sqrt(mean(1 * (num.t.pos >= (1/3) * M)) * (1 - mean(1 * (num.t.pos >= (1/3) * M)))/S)
    # power.results[p, "1/2"] <- mean(1 * (num.t.pos >= (1/2) * M))
    # # se.power[p, "1/2"] <- sqrt(mean(1 * (num.t.pos >= (1/2) * M)) * (1 - mean(1 * (num.t.pos >= (1/2) * M)))/S)
    # power.results[p, "2/3"] <- mean(1 * (num.t.pos >= (2/3) * M))
    # # se.power[p, "2/3"] <- sqrt(mean(1 * (num.t.pos >= (2/3) * M)) * (1 - mean(1 * (num.t.pos >= (2/3) * M)))/S)
    # power.results[p, "full"] <- mean(1 * (num.t.pos.rawp == M))
    # # se.power[p, "full"] <- sqrt(mean(1 * (num.t.pos == M)) * (1 - mean(1 * (num.t.pos == M)))/S)
  }
  
  # calculate mean power across all individual powers
  power.results[,"indiv.mean"] <- apply(as.matrix(power.results[,1:M, drop = FALSE][,alts, drop = FALSE]), 1, mean)
  
  # confidenceintervals
  se.power <- sqrt(0.25/S) 
  CI.lower.power <- power.results - 1.96 * se.power
  CI.upper.power <- power.results + 1.96 * se.power
  # make sure that intervals do not produce power below 0 or above 1
  for(i in 1:nrow(CI.lower.power))
  {
    for(j in 1:ncol(CI.lower.power))
    {
      CI.lower.power[i,j] <- max(CI.lower.power[i,j], 0)
      CI.lower.power[i,j] <- min(CI.lower.power[i,j], 1)
      CI.upper.power[i,j] <- max(CI.upper.power[i,j], 0)
      CI.upper.power[i,j] <- min(CI.upper.power[i,j], 1)
    }
  }
  
  adj_power <- list(power.results, CI.lower.power, CI.upper.power)
  names(adj_power) <- c("adjusted_power", "ci_lower", "ci_upper")
  
  get_sim_results_melt = function(adj_power)
  {
    sim_results <- data.frame(adj_power)
    sim_results$MTP <- rownames(sim_results)
    sim_results_melt <- melt(sim_results, id.vars = 'MTP')
    sim_results_melt$method = 'sim'
    return(sim_results_melt)
  }
  
  # apply for means and upper and lower bounds of confidence interval
  sim_results_melt_full <- list.rbind(lapply(adj_power, get_sim_results_melt))
  sim_results_melt_full$value.type <- sapply(rownames(sim_results_melt_full), function(x){strsplit(x, '\\.')[[1]][1]})
  
  return(sim_results_melt_full)
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

make.model <- function(dat.m, design) {
  # dat.m = dat.all[[1]];
  
  singular <- FALSE
  dat.m$S.id <- as.factor(dat.m$S.id)
  if(!is.null(dat.m$D.id)){ dat.m$D.id <- as.factor(dat.m$D.id) }

  if (design == "blocked_i1_2c") {
    form <- as.formula("Yobs ~ 1 + T.x + C.ijk + S.id")
    mod <- lm(form, data = dat.m)
  } else if (design == "blocked_i1_2f") {
    mod <- interacted_linear_estimators(Yobs = Yobs, Z = T.x, B = S.id, data = dat.m, control_formula = "C.ijk", use.lmer = FALSE)
  } else if (design == "blocked_i1_2r") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + X.jk + C.ijk + (1 + T.x | S.id)"))
    mod <- lmer(form, data = dat.m)
    singular <- isSingular(mod)
  } else if (design == "blocked_i1_3r") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + V.k + X.jk + C.ijk + (1 + T.x | S.id) + (1 + T.x | D.id)"))
    mod <- lmer(form, data = dat.m)
    singular <- isSingular(mod)
  } else if (design == "simple_c2_2r") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + X.jk + C.ijk + (1 | S.id)"))
    mod <- lmer(form, data = dat.m)
    singular <- isSingular(mod)
  } else if (design == "simple_c3_3r") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + V.k + X.jk + C.ijk + (1 | S.id) + (1 | D.id)"))
    mod <- lmer(form, data = dat.m)
    singular <- isSingular(mod)
  } else if (design == "blocked_c2_3f") {
    mod <- interacted_linear_estimators(Yobs = Yobs, Z = T.x, B = D.id, data = dat.m, control_formula = "X.jk + C.ijk + (1 | S.id)", use.lmer = TRUE)
  } else if (design == "blocked_c2_3r") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + V.k + X.jk + C.ijk + (1 | S.id) + (1 + T.x | D.id)"))
    mod <- lmer(form, data = dat.m)
    singular <- isSingular(mod)
  } else {
    stop(paste('Unknown design:', design)) 
  }
  return(list(mod = mod, singular = singular))
}

# --------------------------------------------------------------------- #
#	Function: make.dummies		Inputs:	dat, clusterby, nbar, J
#		a dataset (dat),
#		a column name as a string to make dummy variables for   
#		nbar, and J 									                                                     
#	Outputs: dummies (column names), lmedat.fixed (data.frame)		                    
# --------------------------------------------------------------------- #

make.dummies <- function(dat, dummy.vars, nbar, J){

  # dat = mdat[[1]]; dummy.vars = c("D.id","S.id"); var = dummy.vars[1]

  all.dum <- NULL
  for(i in 1:length(dummy.vars))
  {
    block.rep <- matrix(
      data = rep(dat[,var], nbar),
      nrow = length(dat[,var]), ncol = J
    )
    colnum <- seq(1:J)
    block.dum <- t(apply(block.rep, 1, function(x) { 1*(x==colnum) }))
    colnames(block.dum) <- paste("dummy", i, ".", 1:J, sep = "")
    all.dum <- cbind(all.dum, block.dum)
  }

  lmedat.fixed <- cbind(dat, all.dum)
  dummies <- paste(colnames(all.dum[,-1]), collapse = "+")

  return(list("dnames" = dummies, "fixdat" = lmedat.fixed))
}


# --------------------------------------------------------------------- #
#	Function: get.pval 	Inputs: mod						                              #
#		a model object, mod							                                      #
#	Outputs: pvalue 									                                      #
# --------------------------------------------------------------------- #

get.pval.tstat <- function(mod, design, user.params.list) {

  if(class(mod) == "lm") {
    tstat <- summary(mod)$coefficients["T.x","t value"]
    pval <- summary(mod)$coefficients["T.x","Pr(>|t|)"]
  } else if(class(mod) == "lmerMod") {
    df <- calc.df(design, user.params.list[['J']], user.params.list[['K']],
                  user.params.list[['nbar']], numCovar.1 = 1, numCovar.2 = 1, numCovar.3 = 1)
    tstat <- summary(mod)$coefficients["T.x","t value"]
    pval <- (1 - pt(abs(tstat), df = df))*2
  } else if (class(mod) == "fastLm") {
    pval <- summary(mod)$coef["T.x", "Pr(>|t|)"]
  } else if (class(mod) == "data.frame") {
    # fixed effects models
    df <- calc.df(design, user.params.list[['J']], user.params.list[['K']],
                  user.params.list[['nbar']], numCovar.1 = 1, numCovar.2 = 1, numCovar.3 = 1)
    tstat <- mod$ATE_hat[1]/mod$SE[1]
    pval <- (1 - pt(abs(tstat), df = df))*2
  } else
  {
    stop('Unknown model type')
  }
  return(list(tstat = tstat, pval = pval))
}

# --------------------------------------------------------------------- #
#	Function: get.rawp	Inputs: dat.all, design, nbar, J      	            #
#   dat.all, a single dataset from list of S datasets as a list length M  #
#												                                                  #
# Calls: make.dummies, make.model, get.pval                               #
#	Outputs: matrix of raw p-values for a single sample		                  #
#	Notes: gets raw p-vals for a single dataset and funct at a time	        #
# --------------------------------------------------------------------- #

get.rawpt <- function(dat.all, design, user.params.list) {
  mods.out = lapply(dat.all, function(m) make.model(m, design))
  mods = lapply(mods.out, function(m){ return(m[['mod']]) })
  num.singular = sapply(mods.out, function(m){ return(m[['singular']]) })
  rawpt = lapply(mods, function(x) get.pval.tstat(x, design, user.params.list))
  return(list(rawpt = rawpt, num.singular = sum(num.singular)))
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

makelist.samp <-function(samp.obs, T.x) {

  mdat.rn <- NULL
  for(m in 1:ncol(samp.obs$Yobs))
  {
    # level 3
    if(!is.null(samp.obs[['V.k']]))
    {
      mdat.rn[[m]] <- data.frame(
        Yobs        = samp.obs[['Yobs']][,m],
        V.k         = samp.obs[['V.k']][,m],
        X.jk        = samp.obs[['X.jk']][,m],
        C.ijk       = samp.obs[['C.ijk']][,m],
        T.x         = T.x,
        S.id        = as.factor(samp.obs$ID$S.id),
        D.id        = as.factor(samp.obs$ID$D.id)
      )
    } else
    # level 2
    {
      mdat.rn[[m]] <- data.frame(
        Yobs        = samp.obs[['Yobs']][,m],
        X.jk        = samp.obs[['X.jk']][,m],
        C.ijk       = samp.obs[['C.ijk']][,m],
        T.x       = T.x,
        S.id        = as.factor(samp.obs$ID$S.id)
      )
    }
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

get.adjp <- function(proc, rawp, rawt, dat.all, S.id, D.id, sim.params.list, model.params.list, design, cl = NULL) {

  if(proc == "WY-SD" | proc == "WY-SS"){
    tw1 <- Sys.time()
    adjp.proc <- adjust_WY(
      dat.all = dat.all,
      rawp = rawp, rawt = rawt,
      S.id = S.id, D.id = D.id,
      proc = proc,
      sim.params.list = sim.params.list,
      model.params.list = model.params.list,
      design = design,
      cl = cl
    )[,"WY"]
    tw2 <- Sys.time()
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
  rejects <- as.matrix(1*(adjp < alpha), nrow = nrow(adjp), ncol = ncol(adjp))
}



#' Interacted linear regression models
#' https://github.com/lmiratrix/blkvar/blob/master/R/linear_model_method.R
#'
#' These linear models have block by treatment interaction terms.  The final ATE
#' estimates are then weighted average of the block (site) specific ATE
#' estimates.
#'
#'#' If siteID passed, it will weight the RA blocks within site and then average
#' these site estimates.
#'
#' SEs come from the overall variance-covariance matrix.
#'
#' @inheritParams linear_model_estimators
#'
#' @return Dataframe of the different versions of this estimator (person and
#'   site weighted)
#' @family linear model estimators
#' @export
interacted_linear_estimators <- function(Yobs, Z, B, siteID = NULL, data = NULL,
                                         control_formula = NULL, use.lmer = FALSE) {
  # siteID = NULL;
  # Yobs = dat.m$Yobs; Z = dat.m$T.x; B = dat.m$D.id; data = dat.m; control_formula = "X.jk + C.ijk + (1 | S.id)"; use.lmer = TRUE
  # Yobs = dat.m$Yobs; Z = dat.m$T.x; B = dat.m$S.id; data = dat.m; control_formula = "C.ijk"; use.lmer = FALSE
  
  # This code block takes the parameters of
  # Yobs, Z, B, siteID = NULL, data=NULL, ...
  # and makes a dataframe with canonical Yobs, Z, B, and siteID columns.
  d2 <- data
  d2$Yobs <- eval(substitute(Yobs), data)
  d2$Z <- eval(substitute(Z), data)
  d2$B <- eval(substitute(B), data)
  data <- d2
  rm(d2)

  data$B <- droplevels(as.factor(data$B))
  J <- length(unique(data$B))
  nj <- table(data$B)
  n <- nrow(data)
  
  formula <- as.formula(sprintf( "%s ~ 0 + %s * %s - %s + %s", "Yobs", "Z", "B", "Z", control_formula))
  
  if(use.lmer)
  {
    M0.int <- lmer(formula, data = data)
    ids <- grep( "Z:", rownames(summary(M0.int)$coefficients))
    stopifnot(length(ids) == J)
  } else
  {
    M0.int <- lm(formula, data = data)
    ids <- grep( "Z:", names(coef(M0.int)))
    stopifnot(length(ids) == J)
  }

  VC <- as.matrix(vcov(M0.int))
  ATE_hats <- summary(M0.int)$coefficients[ids,1]
  wts <- rep(1 / J, J)
  
  # the block SEs from our linear model
  SE_hat <- diag(VC)[ids]

  ATE_hat_site <- weighted.mean(ATE_hats, wts)

  # Calculate SE for ATE_hat_site
  SE_site <- sqrt(sum(wts ^ 2 * SE_hat))

  interactModels <- data.frame(method = c("FE-Int-Sites"), ATE_hat = c(ATE_hat_site), SE = c(SE_site), stringsAsFactors = FALSE)
  if (!is.null(control_formula)) {
    interactModels$method <- paste0(interactModels$method, "-adj")
  }
  return(interactModels)
}
