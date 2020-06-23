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
# if (!requireNamespace("BiocManager", quietly = TRUE)){
#   install.packages("BiocManager")
# }
# BiocManager::install("multtest")
library(multtest)

#' Estimating Power through simulations
#'
#' Loop through different simulations like in Table C.3 of the paper
#'
#' @param user.params.list list of user-inputted parameters that feed into the DGP
#' @param sim.params.list list of simulation parameters
#' @param design RCT design (see list/naming convention)
#'
#' @return a whole series of power data files for validation
#' @export
#'
#' @examples
validate_power_blocked_i1_2cfr <- function(user.params.list,
                                           sim.params.list,
                                           design) {

  ######################################################
  # Variable setting for saving files
  ######################################################

  #####################
  # Simulation Values #
  #####################

  J <- max(user.params.list[['S.j']])
  K <- max(user.params.list[['S.k']])
  N <- length(user.params.list[['S.j']])
  n.jk <- N/J

  params.file.base <- paste0(
    design, "_", sim.params.list[['S']],
    "_", "S", "_", model.params.list[['M']], "_", "M","_",
    convert.vec.to.filename(user.params.list[['MDES']]),"_", "MDES",
    "_", J, "_", "J", "_", n.jk, "_", "njk","_",
    convert.vec.to.filename(user.params.list[['rho']]), "_rho_",
    convert.vec.to.filename(user.params.list[['R2.1']]),"_R21_"
  )

  #margin of error being 0.05
  me <- 0.05

  # simulate and run power calculations
  sim.filename = paste0(params.file.base, "simulation_results.RDS")
  if(sim.params.list[['runSim']]){

    simpwr <- est_power_sim(user.params.list,
                            sim.params.list,
                            design = design)

    saveRDS(simpwr, file = here("Validation/data", sim.filename))

  } else {

    simpwr <- readRDS(file = here::here("Validation/data", sim.filename))
  }

  ###################
  # Power Up Values #
  ###################

  # Power_Up_Results calculation
  # power_up_results <- power.bira2c1(
  #   es = user.params.list[['MDES']][1], alpha = sim.params.list[['alpha']], two.tailed = TRUE,
  #   p = mean(p.j.range), g1 = 1, r21 = R2.1[1], n = n.j, J = J
  # )
  ## UPDATE
  power_up_results <- quiet(power.bira2c1(
    es = user.params.list[['MDES']][1], alpha = sim.params.list[['alpha']], two.tailed = TRUE,
    p = 0.5, g1 = 1, r21 = 0.4, n = 10, J = 10
  ))
  # Power_Up_Standard_Error
  power_up_results$se <- power_up_results$parms$es/power_up_results$ncp
  power_up_results$lower_ci <- power_up_results$power - (1.96 * power_up_results$se)
  power_up_results$upper_ci <- power_up_results$power + (1.96 * power_up_results$se)

  ######################
  # PUMP methods value #
  ######################
  pump.filename <- paste0(params.file.base, "pump_results.RDS")
  if(sim.params.list[['runPump']]){

    pum_combined_results <- NULL

    # UPDATE p and n.j
    for (MTP in sim.params.list[['procs']]){
      pum_results <- pum::power_blocked_i1_2c(
        M = M, MTP = MTP,
        MDES = user.params.list[['MDES']], numFalse = M,
        J = J, n.j = n.jk,
        p = 0.5, alpha = sim.params.list[['alpha']], numCovar.1 = 0, numCovar.2 = 0,
        R2.1 = user.params.list[['R2.1']], R2.2 = user.params.list[['R2.1']],
        ICC = user.params.list[['ICC']], sigma = NULL,
        rho = rho, omega = NULL,
        tnum = sim.params.list[['tnum']], snum = sim.params.list[['B']],
        ncl = sim.params.list[['ncl']]
      )
      pum_results <- data.frame(pum_results)
      pum_combined_results <- dplyr::bind_rows(pum_combined_results, pum_results[2,])
    }
    # adding rownames to the pum_combined_results table
    # rownames(pum_combined_results) <- c("rawp", sim.params.list[['procs']])
    saveRDS(pum_combined_results, file = here::here("Validation/data", pump.filename))
  }else {
    pum_combined_results <- readRDS(file = here::here("Validation/data", pump.filename))
  }

  ########################################
  # Compare Results Table                #
  ########################################
  compare_results <- data.frame("pum_indiv" = pum_combined_results[,"indiv"],
                                "sim_indiv" = simpwr$adjusted_power[,"D1indiv"],
                                "pup_indiv" = power_up_results$power,
                                "pum_min1"  = pum_combined_results[,"min1"],
                                "sim_min1"  = simpwr$adjusted_power[,"1/3"],
                                "pum_min2"  = pum_combined_results[,"min2"],
                                "sim_min2"  = simpwr$adjusted_power[,"2/3"],
                                "pum_comp"  = pum_combined_results[,"complete"],
                                "sim_comp"  = simpwr$adjusted_power[,"full"]
                                )

  # # Setting NAs for the power definitions that do not need adjustment
  # # compare_results$pup_indiv[2:4] <- NA
  # # # compare_results$powerup_indiv_lower_ci[2:4] <- NA
  # # # compare_results$powerup_indiv_upper_ci[2:4] <- NA
  # # compare_results$pup_comp[2:4] <- NA
  # # compare_results$sim_comp[2:4] <- NA
  # # # compare_results$sim_complete_lower_ci[2:4] <- NA
  # # # compare_results$sim_complete_upper_ci[2:4] <- NA
  # #
  # Giving Rownames a column header
  compare_results <- compare_results %>%
                     tibble::rownames_to_column(var = "MTP")
  compare_results <- round_df(compare_results,2) # Rounding the data frames
  saveRDS(compare_results, file = here::here("Validation/data", paste0(params.file.base,"comparison_results.RDS")))

  return(compare_results)

} # validate_power_blocked_i1_2cfr
