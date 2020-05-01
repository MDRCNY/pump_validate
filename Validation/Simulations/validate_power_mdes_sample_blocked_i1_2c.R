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
library(pum) # for checking with the new methods
library(dplyr) # for combing data frames
library(tibble) # a modern take on data frames


# Installing and Loading Libraries from Bioconductor package
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

# BiocManager::install("multtest")
library(multtest)
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
#' @param S number of samples to generate for Monte Carlo Simulations
#' @param B number of samples of Westfall-Young, this translates to snum in our new method(the number of samples for Westfall-Young. The default is set at 1,000.)
#' @param J number of blocks
#' @param n.j number of observations per block
#' @param theta MxM matrix of correlations between residuals in Level 2 model outcomes under no treatment and Level 2
#' @param omega effect size variability, between 0 and 1, 0 if no variation in effects across blocks, vector length M
#' @param Gamma.00 grand mean outcome w/o treat, held 0, vector length M. 
#' @param sig.sq vector length M, held at 1 for now
#' @param alpha the significance level, 0.05 usually
#' @param ICC a number, intraclass correlation; 0 if fixed model
#' @param R2.2 R squared for mth level 2 outcome by mth level 2 covar
#' @param R2.1 R squared for mth level 1 outcome by mth level 1 covar
#' @param maxT ?
#' @param rho.0_lev1 MxM matrix of correlations for Level 1 residuals in models of outcomes under no treatment  
#' @param rho.0_lev2 MxM matrix of correlations for Level 2 residuals in models of outcomes under no treatment
#' @param rho.1_lev2 MxM matrix of correlations for Level 2 effects
#' @param check checking to see if intermeidate outputs are as expected.
#' @param tnum the number of test statistics (samples) for all procedures other than 
#' Westfall-Young & number of permutations for WY. The default is set at 10,000
#' @param storage_path file path to store the validation results 
#' @param validation_name name of the validation file
#' @param runSim if TRUE, we will re-run the simluation. if FALSE, we will pull previous run result.
#' @param runPump if TRUE, we will run method from our package. if FALSE, we will pull previous run result.
#'
#' @return a whole series of power data files for validation
#' @export
#'
#' @examples
validate_power_blocked_i1_2cfr <- function(rho,ncl,procs,design,M,MDES,p.j.range,p.j,S,  
                                             B,tnum,J,n.j,theta,omega,Gamma.00,sig.sq,alpha,
                                             ICC,R2.2,R2.1,maxT,rho.0_lev1,rho.0_lev2,
                                             rho.1_lev2,check, storage_path, validation_name,
                                             runSim, runPump) {
  
  
  # To be used as a variable in file saving!
  R2_1 <- R2.1[1]
  
  ##################### 
  # Simulation Values #
  #####################
  
  tic("Simulation of Two Level Constant Effects")

  #margin of error being 0.05
  me <- 0.05
  
      rho.0_lev1 <- matrix(rho,M,M); diag(rho.0_lev1) <- 1 #done
      rho.0_lev2 <- matrix(rho,M,M); diag(rho.0_lev2) <- 1 #done
      rho.1_lev2 <- matrix(rho,M,M); diag(rho.1_lev2) <- 1 #done
    
    # simulate and run power calculations
    # replace with est_power_sim function here along with some changes in parameter.

  if(runSim == TRUE){
    
    simpwr <- est_power_sim(procs = procs, S = S, ncl = ncl, B = B, maxT = FALSE, 
                              M = M, MDES = MDES, n.j = n.j, J = J, rho.0_lev1 = rho.0_lev1,
                              rho.0_lev2 = rho.0_lev2, rho.1_lev2 = rho.1_lev2, theta = theta,
                              ICC = ICC, alpha = alpha, Gamma.00 = Gamma.00, p.j.range = p.j.range, 
                              p.j = p.j, R2.1 = R2.1, R2.2 = R2.2, omega = omega, check = check,
                              design = design)
    
    toc(log = TRUE)
    
    saveRDS(simpwr, file = here("Validation/data", paste0(validation_name,"_",S,
                                                          "_", "samples", "_", rho, "_rho_", 
                                                          R2_1,"_R_Squared_","simulaton_results",".RDS")))     

  } else {
    
    simulation_results <- paste0(paste0(validation_name,"_",S,
                                        "_", "samples", "_", rho, "_rho_", 
                                        R2_1,"_R_Squared_","simulaton_results",".RDS"))
    
    simpwr <- readRDS(file = here::here("Validation/data",simulation_results))
  }
  
  ###################
  # Power Up Values #
  ###################
  
  # Power_Up_Results calculation
  power_up_results <- power.bira2c1(es = MDES[1],alpha,two.tailed = TRUE,p = mean(p.j.range),g1 = 1,r21 = R2.1[1],n = n.j,J = J)
  
  # Power_Up_Standard_Error
  power_up_results$se <- power_up_results$parms$es/power_up_results$ncp
  power_up_results$lower_ci <- power_up_results$power - (1.96 * power_up_results$se)
  power_up_results$upper_ci <- power_up_results$power + (1.96 * power_up_results$se)
  
  ######################
  # PUMP methods value #
  ######################
  
  if(runPump == TRUE){
    
    iterator = 0
    
    for (MTP in procs){
      
      # adding iterators for appending data frames       
      
        pum_results <- pum::power_blocked_i1_2c(M = M, MTP = MTP, MDES = MDES, numFalse = M, J = J, n.j = n.j, 
                                 p = p.j.range[1], alpha = alpha, numCovar.1 = 0, numCovar.2 = 0,
                                 R2.1 = R2.1, R2.2 = R2.2, ICC = ICC, sigma = NULL,rho = rho, omega = NULL,
                                 tnum = tnum, snum = B, ncl = ncl)
        
        pum_results <- data.frame(pum_results)
        
      if (iterator == 0){
        
        pum_combined_results <- pum_results
        
      }else{
    
        pum_combined_results <- dplyr::bind_rows(pum_combined_results, pum_results[2,])
        
      }
        
        
      iterator = iterator + 1  
        
    }
      # adding rownames to the pum_combined_results table
      rownames(pum_combined_results) <- c("rawp", procs)
      
      saveRDS(pum_combined_results, file = here::here("Validation/data", paste0(validation_name,"_",S,
                                                                                "_", "samples", "_", rho, "_rho_",
                                                                                R2_1,"_R_Squared_",
                                                                                "pump_results",".RDS")))
  }else{
    
      pump_results <-paste0(validation_name,"_",S,
                            "_", "samples", "_", rho, "_rho_",
                            R2_1,"_R_Squared_",
                            "pump_results",".RDS")     
      pum_combined_results <- readRDS(file = here::here("Validation/data", pump_results))
  }
    
  ########################################
  # Compare Results Table                #
  ########################################
  compare_results <- data.frame("pump_indiv" = pum_combined_results[,"indiv"],
                                "sim_indiv" = simpwr$adjusted_power[,"D1indiv"],
                                "powerup_indiv" = power_up_results$power,
                                "pump_min1" = pum_combined_results[,"min1"],
                                "sim_min1" = simpwr$adjusted_power[,"1/3"],
                                "pump_min2" = pum_combined_results[,"min2"],
                                "sim_min2" = simpwr$adjusted_power[,"2/3"],
                                "pump_complete" = pum_combined_results[,"complete"],
                                "sim_complete" = simpwr$adjusted_power[,"full"]
                                )
  
  # Setting NAs for the power definitions that do not need adjustment
  compare_results$powerup_indiv[2:4] <- NA
  # compare_results$powerup_indiv_lower_ci[2:4] <- NA
  # compare_results$powerup_indiv_upper_ci[2:4] <- NA
  compare_results$pump_complete[2:4] <- NA
  compare_results$sim_complete[2:4] <- NA
  # compare_results$sim_complete_lower_ci[2:4] <- NA
  # compare_results$sim_complete_upper_ci[2:4] <- NA
  
  # Giving Rownames a column header
  compare_results <- compare_results %>% 
                        tibble::rownames_to_column(var = "MTP")
  
  saveRDS(compare_results, file = here::here("Validation/data", paste0(validation_name,"_",S,
                                                                      "_", "samples", "_", rho, "_rho_",
                                                                      R2_1,"_R_Squared_",
                                                                      "comparison_results",".RDS")))
  return(compare_results)
  
} # validate_power_blocked_i1_2cfr
