#' ---
#' title: "Monte Carlo Simulation Code"
#' author: "Kristin Porter, Deni Chen and Zarni Htet"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: html_notebook
#' ---
#' 

# Loading the libraries from CRAN
library(RcppEigen) # rcpp for speed issues
library(snow) # for parallel coding
library(lme4) # for modeling
library(PowerUpR) # for checking with another power estimation function
library(here) # for relative file paths
library(tictoc) # for timing


# Installing and Loading Libraries from Bioconductor package
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

# BiocManager::install("multtest")
library(multtest)

# Sourcing the custom functions to run the simulations
#source(here::here("Validation/Simulations", "gen_data_dist_2_levels.R"))
#source(here::here("Validation/Simulations", "power_mcs_2_levels"))
source(here::here("Validation/Simulations", "adjust.WY.R"))

#function to check time lapse

#' Estimating Power through simulations
#'
#' Loop through different simulations like in Table C.3 of the paper
#'
#' @param rho Spearman's correlation determines the strength and direction of the monotonic relationship between two variables rather than the                 
#' strength and direction of the linear relationship between your two variables
#' @param ncl number of clusters max can be set to 16 (max on RStudio Server)
#' @param procs a vector of strings for adjustment procedures
#' @param design RCT design (see list/naming convention)
#' @param M number of tests/domains/outcomes 
#' @param MDES minimum detectable effect size, vector length M
#' @param p.j.range vector of minimum and maximum probabilities of being assigned to treatment, across all sites 
#' @param p.j probability of being assigned to treatment when no blocking 
#' @param S number of samples to generate
#' @param B number of samples of Westfall-Young
#' @param J number of blocks
#' @param n.j number of observations per block
#' @param theta MxM matrix of correlations between residuals in Level 2 model outcomes under no treatment and Level 2
#' @param omega effect size variability, between 0 and 1, 0 if no variation in effects across blocks, vector length M
#' @param Gamma.00 grand mean outcome w/o treat, held 0, vector length M
#' @param sig.sq vector length M, held at 1 for now
#' @param alpha the significance level, 0.05 usually
#' @param ICC a number, intraclass correlation; 0 if fixed model
#' @param R2.2 R squared for mth level 1 outcome by mth level 1 covar
#' @param R2.1 R squared for mth level 2 outcome by mth level 1 covar
#' @param maxT ?
#' @param rho.0_lev1 MxM matrix of correlations for Level 1 residuals in models of outcomes under no treatment  
#' @param rho.0_lev2 MxM matrix of correlations for Level 2 residuals in models of outcomes under no treatment
#' @param rho.1_lev2 MxM matrix of correlations for Level 2 effects
#' @param check ?
#'
#' @return a whole series of power data files for validation
#' @export
#'
#' @examples
validate_power_blocked_i1_2c <- function(rho,ncl,procs,design,M,MDES,p.j.range,p.j,S,  
                                         B,J,n.j,theta,omega,Gamma.00,sig.sq,alpha,
                                         ICC,R2.2,R2.1,maxT,rho.0_lev1,rho.0_lev2,
                                         rho.1_lev2,check) {
  
  tic()
  #create loop ID for list of power estimates with different correlations
  loop_idx <- 1
  
  #assign list object to save datafiles into a list
  sim_power_storage <- list()
  
  for (rho in rho) {
    
    # simulate and run power calculations
    # replace with est_power_sim function here along with some changes in parameter.
    
    simpwr <- est_power_sim(procs = procs, S = S, ncl = ncl, B = B, maxT = FALSE, 
                              M = M, MDES = MDES, n.j = n.j, J = J, rho.0_lev1 = rho.0_lev1,
                              rho.0_lev2 = rho.0_lev2, rho.1_lev2 = rho.1_lev2, theta = theta,
                              ICC = ICC, alpha = alpha, Gamma.00 = Gamma.00, p.j.range = p.j.range, 
                              p.j = p.j, R2.1 = R2.1, R2.2 = R2.2, omega = omega, check = check,
                              design = design)
                            
    # check against PowerUp
    power.up <- power.bira2c1(es = MDES[1],alpha,two.tailed = TRUE,p = mean(p.j.range),g1 = 1,r21 = R2.1[1],n = n.j,J = J)
    # If TRUE, then raw individual power matches estimate from Power-Up.
    
    me <- 0.05
    power.up$power < (simpwr["rawp","D1indiv"] + me) & power.up$power > (simpwr["rawp","D1indiv"] - me)
    
    
    #add data files to list
    sim_power_storage[[loop_idx]] <- list()
    sim_power_storage[[loop_idx]][["simname"]] <- simname
    sim_power_storage[[loop_idx]][["obj"]] <- simpwr
    
    #export separate data files - for Domino to run and then pull down
    #  fdir <- "I:/Multiplicity/datafiles/"
    #  fname<- paste0(fdir,simname)
    #  save(simpwr, file=fname)
    save(simpwr, file=simname)
    loop_idx <- loop_idx + 1
    
  }
  
  toc(log = TRUE)
  
  #export the list of data files with different correlations for import into validation RMD
  #lpath <- paste0(fdir, lname)
  #save(sim_power_storage, file= lpath)
  save(sim_power_storage, file= lname)

}
