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

#'  Function: make.model		                                       
#'  
#'  function to generate a model for simulated data
#'
#' @param dat.m a data frame for a single outcome
#' @param d_m design/model
make_model <- function(dat.m, d_m) {
  # dat.m = dat.all[[1]];
  
  singular <- FALSE
  failed.converge <- FALSE
  
  dat.m$S.id <- as.factor(dat.m$S.id)
  if(!is.null(dat.m$D.id)){ dat.m$D.id <- as.factor(dat.m$D.id) }

  if (d_m == "d1.1_m2cc") {
    form <- as.formula("Yobs ~ 1 + T.x + C.ijk")
    mod <- lm(form, data = dat.m)
  } else if (d_m == "d2.1_m2fc") {
    form <- as.formula("Yobs ~ 1 + T.x + C.ijk + S.id")
    mod <- lm(form, data = dat.m)
  } else if (d_m == "d2.1_m2ff") {
    mod.out <- interacted_linear_estimators(
      Yobs = Yobs, Z = T.x, B = S.id, data = dat.m,
      control_formula = "C.ijk", use.lmer = FALSE
    )
    singular <- mod.out[['singular']]
    mod <- mod.out[['mod']]
  } else if (d_m == "d2.1_m2fr") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + X.jk + C.ijk + (1 + T.x | S.id)"))
    mod <- suppressMessages(lmer(form, data = dat.m))
    singular <- isSingular(mod)
    failed.converge <- ifelse(!is.null(mod@optinfo$conv$lme4$code), TRUE, FALSE)
  } else if (d_m == "d3.1_m3rr2rr") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + V.k + X.jk + C.ijk + (1 + T.x | S.id) + (1 + T.x | D.id)"))
    mod <- suppressMessages(lmer(form, data = dat.m))
    singular <- isSingular(mod)
    failed.converge <- ifelse(!is.null(mod@optinfo$conv$lme4$code), TRUE, FALSE)
  } else if (d_m == "d2.2_m2rc") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + X.jk + C.ijk + (1 | S.id)"))
    mod <- suppressMessages(lmer(form, data = dat.m))
    singular <- isSingular(mod)
    failed.converge <- ifelse(!is.null(mod@optinfo$conv$lme4$code), TRUE, FALSE)
  } else if (d_m == "d3.3_m3rc2rc") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + V.k + X.jk + C.ijk + (1 | S.id) + (1 | D.id)"))
    mod <- suppressMessages(lmer(form, data = dat.m))
    singular <- isSingular(mod)
    failed.converge <- ifelse(!is.null(mod@optinfo$conv$lme4$code), TRUE, FALSE)
  } else if (d_m == "d3.2_m3ff2rc") {
    mod.out <- interacted_linear_estimators(
      Yobs = Yobs, Z = T.x, B = D.id, data = dat.m,
      control_formula = "X.jk + C.ijk + (1 | S.id)",
      use.lmer = TRUE
    )
    singular <- mod.out[['singular']]
    mod <- mod.out[['mod']]
  } else if (d_m == "d3.2_m3rr2rc") {
    form <- as.formula(paste0("Yobs ~ 1 + T.x + V.k + X.jk + C.ijk + (1 | S.id) + (1 + T.x | D.id)"))
    mod <- suppressMessages(lmer(form, data = dat.m))
    singular <- isSingular(mod)
    failed.converge <- ifelse(!is.null(mod@optinfo$conv$lme4$code), TRUE, FALSE)
  } else {
    stop(paste('Unknown d_m:', d_m)) 
  }

  return(list(mod = mod, singular = singular, failed.converge = failed.converge))
}


#'  Function: get_pval_tstat	                                       
#'  
#' extracts p-value and t statistics from a given model
#'
#' @param mod model object
#' @param d_m design/model
#' @param model.params.list list of model parameters
get_pval_tstat <- function(mod, d_m, model.params.list) {
  if(class(mod) == "lm") {
    tstat <- summary(mod)$coefficients["T.x","t value"]
    pval <- summary(mod)$coefficients["T.x","Pr(>|t|)"]
  } else if(class(mod) == "lmerMod") {
    df <- PUMP::calc_df(d_m, model.params.list[['J']], model.params.list[['K']],
                        model.params.list[['nbar']],
                        numCovar.1 = 1, numCovar.2 = 1, numCovar.3 = 1)
    tstat <- summary(mod)$coefficients["T.x","t value"]
    pval <- (1 - pt(abs(tstat), df = df))*2
  } else if (class(mod) == "data.frame") {
    # fixed effects models
    df <- PUMP::calc_df(d_m, model.params.list[['J']], model.params.list[['K']],
                  model.params.list[['nbar']], numCovar.1 = 1, numCovar.2 = 1, numCovar.3 = 1)
    tstat <- mod$ATE_hat[1]/mod$SE[1]
    pval <- 2*(1 - pt(abs(tstat), df = df))
  } else
  {
    stop('Unknown model type')
  }
  return(list(tstat = tstat, pval = pval))
}

#' Function: get_rawpt                                      
#'  
#' fits models and extracts p values and t statistics
#'
#' @param dat.all list of datasets
#' @param d_m design/model
#' @param model.params.list list of model parameters
get_rawpt <- function(dat.all, d_m, model.params.list) {
  mods.out <- lapply(dat.all, function(m) make_model(m, d_m))
  mods <- lapply(mods.out, function(m){ return(m[['mod']]) })
  singular <- sapply(mods.out, function(m){ return(m[['singular']]) })
  failed.converge <- sapply(mods.out, function(m){ return(m[['failed.converge']]) })
  rawpt <- lapply(mods, function(x) get_pval_tstat(x, d_m, model.params.list))
  return(list(rawpt = rawpt, num.singular = sum(singular), num.failed.converge = sum(failed.converge)))
}

#' takes in multi-outcome data and returns a list for each outcome
#'
#' @param samp.obs a single iteration of observed data
#' @param T.x vector of treatment assignments
makelist_samp <-function(samp.obs, T.x) {

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

#' Interacted linear regression models
#' https://github.com/lmiratrix/blkvar/blob_master/R/linear_model_method.R
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

  # keep track of singularity
  singular <- FALSE

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
  } else
  {
    M0.int <- lm(formula, data = data)
    ids <- grep( "Z:", names(coef(M0.int)))
  }

  if(length(ids) != J)
  {
    message('Proceeding with rank deficient model')
    singular <- TRUE
    J <- length(ids)
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
  return(list(mod = interactModels, singular = singular))
}
