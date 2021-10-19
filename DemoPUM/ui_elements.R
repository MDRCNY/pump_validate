######################################################################################################################
# Objective : This document is to organize all the different UI elements for each of the design & different scenario #
######################################################################################################################

############################
# Number of Units per block
############################

nbarInput <- function(design, scenario){
  
  print(scenario)
  
  id <- paste0("nbar", "_", design, "_" , scenario)
  
  numericInput(id,
               "Number of units per block", 
               min = 2, 
               max = 100, 
               value = 20, 
               step = 1)
} # number of unit per block

############################
# Number of Blocks
############################

jInput <- function(design, scenario){
  
  id <- paste0("j", "_", design, "_" , scenario)
  
  numericInput(id,
               "Number of blocks", 
               min = 2, 
               max = 100, 
               value = 50, 
               step = 1)
} # number of unit per block

#########################
# Number of Outcomes (M)
#########################

mInput <- function(design, scenario){
  
  if(scenario == "single_scenario_tab"){
    id <- paste0("m", "_", design, "_" , scenario)
    numericInput(id, 
                 "Number of Outcomes", 
                 min = 1, 
                 max = 10, 
                 value = 5, 
                 step = 1)
  }
  
} # Number of Outcomes (M)

#########################
# MTP element
#########################

mtpInput <- function(design, scenario){
  
  id <- paste0("mtp", "_", design, "_" , scenario)
  
  selectInput(id, "Multiple testing procedure (MTP)", 
              choices = list("Bonferroni" = "Bonferroni", 
                              "Holm" = "Holm", 
                              "Benjamini-Hochberg" = "BH", 
                              "Westfall-Young-Single-Step" = "WY-SS", 
                              "Westfall-Young-Step-Down" = "WY-SD"),
              selected = "Bonferroni",
              multiple = TRUE) # select input buttons div
} # mtp input

mtpActionButton <- function(design, scenario){
  
  question <- paste0("question", "mtp", design, scenario, sep = "_")
  actionButton(question,
               label = "", 
               icon = icon("question"),
               style = "font-size: 10px; 
               margin-top: 28px;") #div for button ends
  
} # mtpAction Button

mtpPopOver <- function(design, scenario){
  
  question <- paste0("question", "mtp", design, scenario, sep = "_")
  bsPopover(id = question, 
            title = NULL,
            content = paste0("For more information on MTP, please click!"),
            placement = "right", 
            trigger = "hover", 
            options = list(container = "body")
  )
} #mtpPop over


#########################
# Minimum Detectable Effect Size
#########################

mdesInput <- function(design, scenario, numOutcome){

  # if initial values are not set yet, set it at 5.  
  if(length(numOutcome) == 0){
    
    numOutcome <- 5
    
  } # set default value to numOutcome
  
  # default mdes values whose count of number will change depending on number of outcomes
  defaultmdesvalues <- paste0(rep(0.125, times = numOutcome), collapse = ",")
  
  id <- paste0("mdes", "_", design, "_" , scenario)
  textInput(id, 
           "Minimum detectable effect size (MDES) (Vector of length M, comma delimited)", 
           value = defaultmdesvalues)
} # mdesInput

##################################################################################
# Correlation between test statistics (assumed to be the same between all pairs)
##################################################################################

rhoInput <- function(design, scenario){
  
  id <- paste0("rho", "_", design, "_" , scenario)
  numericInput(id, 
               "Correlation between test statistics (assumed to be the same between all pairs)", 
               min = 0, 
               max = 1, 
               value = 0.5, 
               step = 0.1 )
} # rhoInput

##################################################################################
# Number of level 1 covariates
##################################################################################

numCovar.1Input <- function(design, scenario){
  
  id <- paste0("numCovar.1", "_", design, "_", scenario)
  numericInput(id, 
               "Number of level 1 covariates", 
               min = 0, 
               max = 10, 
               value = 1, 
               step = 1 )
} # number of level 1 covariates

##################################################################################
# Proportion of treatment assignment
##################################################################################

tbarInput <- function(design, scenario){
  
  id <- paste0("tbar", "_", design, "_" , scenario)
  numericInput(id, 
               "Proportion of treatment assignment", 
               min = 0.001, 
               max = 1.0, 
               value = 0.5, 
               step = 0.001)
} # tbarInput

##################################################################################
# Significance level (alpha)
##################################################################################

alphaInput <- function(design, scenario) {

  id <- paste0("alpha", "_", design, "_", scenario)
  numericInput(id, 
               "Significance level (alpha)", 
                min = 0.001, 
                max = 0.9, 
                value = 0.05,                     
                step = 0.001)
} #alpha

#########################
# R2.1 element
#########################

r2.1Input <- function(design, scenario, numOutcome){
  
  # if initial values are not set yet, set it at 5.  
  if(length(numOutcome) == 0){
    
    numOutcome <- 5
    
  } # set default value to numOutcome
  
  # default r2.1 values whose count of number will change depending on number of outcomes
  defaultr2.1values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
  
  
  id <- paste0("r2.1", "_", design, "_" , scenario)

  textInput(id, 
            "Proportion of variance explained by level-1 covariates (R2.1)", 
            value = defaultr2.1values)
} # R2.1 Input

#########################
# R2.2 element
#########################

r2.2Input <- function(design, scenario, numOutcome){
  
  # if initial values are not set yet, set it at 5.  
  if(length(numOutcome) == 0){
    
    numOutcome <- 5
    
  } # set default value to numOutcome
  
  # default r2.2 values whose count of number will change depending on number of outcomes
  defaultr2.2values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
  
  
  id <- paste0("r2.2", "_", design, "_" , scenario)
  
  textInput(id, 
            "Proportion of variance explained by level-2 covariates (R2.2)", 
            value = defaultr2.2values)
} # R2.2 Input

#########################
# ICC.2 element
#########################

icc.2Input <- function(design, scenario, numOutcome){
  
  # if initial values are not set yet, set it at 5.  
  if(length(numOutcome) == 0){
    
    numOutcome <- 5
    
  } # set default value to numOutcome
  
  # default icc.2 values whose count of number will change depending on number of outcomes
  defaulticc.2values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
  
  id <- paste0("icc.2", "_", design, "_" , scenario)
  textInput(id, 
            "Intraclass correlation between level-2 covariates of each outcome (ICC.2)", 
            value = defaulticc.2values)
} #ICC.2 element

#########################
# omega.2 element
#########################

omega.2Input <- function(design, scenario, numOutcome){
  
  # if initial values are not set yet, set it at 5.  
  if(length(numOutcome) == 0){
    
    numOutcome <- 5
    
  } # set default value to numOutcome
  
  # default omega.2 values whose count of number will change depending on number of outcomes
  defaultomega.2values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
  
  id <- paste0("omega.2", "_", design, "_" , scenario)
  textInput(id, 
            "Ratio of level-2 group covariate effect size variability to random effects variability (omega.2)", 
            value = defaultomega.2values)
} # omega.2 element


#########################
# R2.3 element
#########################

r2.3Input <- function(design, scenario, numOutcome){
  
  # if initial values are not set yet, set it at 5.  
  if(length(numOutcome) == 0){
    
    numOutcome <- 5
    
  } # set default value to numOutcome
  
  # default r2.2 values whose count of number will change depending on number of outcomes
  defaultr2.3values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
  
  
  id <- paste0("r2.3", "_", design, "_" , scenario)
  
  textInput(id, 
            "Proportion of variance explained by level-3 covariates (R2.3)", 
            value = defaultr2.3values)
} # R2.3 Input

#########################
# ICC.3 element
#########################

icc.3Input <- function(design, scenario, numOutcome){
  
  # if initial values are not set yet, set it at 5.  
  if(length(numOutcome) == 0){
    
    numOutcome <- 5
    
  } # set default value to numOutcome
  
  # default icc.3 values whose count of number will change depending on number of outcomes
  defaulticc.3values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
  
  id <- paste0("icc.3", "_", design, "_" , scenario)
  textInput(id, 
            "Intraclass correlation between level-3 covariates of each outcome (ICC.3)", 
            value = defaulticc.3values)
} #ICC.3 element

#########################
# omega.3 element
#########################

omega.3Input <- function(design, scenario, numOutcome){
  
  # if initial values are not set yet, set it at 5.  
  if(length(numOutcome) == 0){
    
    numOutcome <- 5
    
  } # set default value to numOutcome
  
  # default icc.3 values whose count of number will change depending on number of outcomes
  defaultomega.3values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
  
  id <- paste0("omega.3", "_", design, "_" , scenario)
  textInput(id, 
            "Ratio of level-3 group covariate effect size variability to random effects variability (omega.3)", 
            value = defaultomega.3values)
} # omega.3 element

############################
# Number of 3-level Grouping
############################

kInput <- function(design, scenario){
  
  id <- paste0("k", "_", design, "_" , scenario)
  
  numericInput(id,
               "Number of level-3 groupings", 
               min = 2, 
               max = 100, 
               value = 50, 
               step = 1)
} # number of 3rd level grouping

