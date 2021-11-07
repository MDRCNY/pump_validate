######################################################################################################################
# Objective : This document is to organize all the different UI elements for each of the design & different scenario #
######################################################################################################################

############################
# Which variables you want to vary
############################

varVaryInputEx <- function(estimation, design, scenario){
  
  print(paste0("I am in variable Input bar", design))
  id <- paste0(estimation, "_", "varVary", "_", design, "_" , scenario)
  
   if(design == "d1.1_m2cc") {

     print(paste0("I am in this", design))
     selectInput(id, "Which variable would you like to vary?",
                 choices = list("Multiple testing procedure" = "mtp",
                                "Minimum detectable effect size" = "mdes",
                                "rho" = "rho",
                                "numCovar.1" = "numCovar.1",
                                "tbar" = "tbar",
                                "alpha" = "alpha",
                                "r2.1" = "r2.1"),
                 selected = "rho") # select input buttons div

   } else if(design == "d2.1_m2fc") {

     print(paste0("I am in this", design))
     selectInput(id, "Which variable would you like to vary?",
                 choices = list("Multiple testing procedure" = "mtp",
                                "Minimum detectable effect size" = "mdes",
                                "rho" = "rho",
                                "numCovar.1" = "numCovar.1",
                                "tbar" = "tbar",
                                "alpha" = "alpha",
                                "r2.1" = "r2.1",
                                "icc.2" = "icc.2"),
                 selected = "rho") # select input buttons div

   } else if(design == "d2.1_m2ff"){

    print(paste0("I am in this", design))
    selectInput(id, "Which variable would you like to vary?",
                choices = list("Multiple testing procedure" = "mtp",
                                "Minimum detectable effect size" = "mdes",
                                "rho" = "rho",
                                "numCovar.1" = "numCovar.1",
                                "tbar" = "tbar",
                                "alpha" = "alpha",
                                "r2.1" = "r2.1",
                                "icc.2" = "icc.2"),
                 selected = "rho") # select input buttons div
    
   } else if(design == "d2.1_m2fr"){
     
     print(paste0("I am in this", design))
     selectInput(id, "Which variable would you like to vary?",
                 choices = list("Multiple testing procedure" = "mtp",
                                "Minimum detectable effect size" = "mdes",
                                "rho" = "rho",
                                "numCovar.1" = "numCovar.1",
                                "tbar" = "tbar",
                                "alpha" = "alpha",
                                "r2.1" = "r2.1",
                                "icc.2" = "icc.2",
                                "omega.2" = "omega.2"),
                 selected = "rho") # select input buttons div
     
   } else if(design == "d3.1_m3rr2rr"){
     
     print(paste0("I am in this", design))
     selectInput(id, "Which variable would you like to vary?",
                 choices = list("Multiple testing procedure" = "mtp",
                                "Minimum detectable effect size" = "mdes",
                                "rho" = "rho",
                                "numCovar.1" = "numCovar.1",
                                "tbar" = "tbar",
                                "alpha" = "alpha",
                                "r2.1" = "r2.1",
                                "icc.2" = "icc.2",
                                "omega.2" = "omega.2",
                                "icc.3" = "icc.3",
                                "omega.3" = "omega.3"),
                 selected = "rho") # select input buttons div
     
   } else if(design == "d2.2_m2rc"){
     
     print(paste0("I am in this", design))
     selectInput(id, "Which variable would you like to vary?",
                 choices = list("Multiple testing procedure" = "mtp",
                                "Minimum detectable effect size" = "mdes",
                                "rho" = "rho",
                                "numCovar.1" = "numCovar.1",
                                "tbar" = "tbar",
                                "alpha" = "alpha",
                                "r2.1" = "r2.1",
                                "icc.2" = "icc.2",
                                "r2.2" = "r2.2"),
                 selected = "rho") # select input buttons div
     
   } else if(design == "d3.3_m3rc2rc"){
     
     print(paste0("I am in this", design))
     selectInput(id, "Which variable would you like to vary?",
                 choices = list("Multiple testing procedure" = "mtp",
                                "Minimum detectable effect size" = "mdes",
                                "rho" = "rho",
                                "numCovar.1" = "numCovar.1",
                                "tbar" = "tbar",
                                "alpha" = "alpha",
                                "r2.1" = "r2.1",
                                "icc.2" = "icc.2",
                                "r2.2" = "r2.2",
                                "r2.3" = "r2.3",
                                "icc.3" = "icc.3"),
                 selected = "rho") # select input buttons div
     
   } else if(design == "d3.2_m3ff2rc"){
     
     print(paste0("I am in this", design))
     selectInput(id, "Which variable would you like to vary?",
                 choices = list("Multiple testing procedure" = "mtp",
                                "Minimum detectable effect size" = "mdes",
                                "rho" = "rho",
                                "numCovar.1" = "numCovar.1",
                                "tbar" = "tbar",
                                "alpha" = "alpha",
                                "r2.1" = "r2.1",
                                "icc.2" = "icc.2",
                                "r2.2" = "r2.2",
                                "icc.3" = "icc.3"),
                 selected = "rho") # select input buttons div
     
   } else if(design == "d3.2_m3rr2rc"){
     
     print(paste0("I am in this", design))
     selectInput(id, "Which variable would you like to vary?",
                 choices = list("Multiple testing procedure" = "mtp",
                                "Minimum detectable effect size" = "mdes",
                                "rho" = "rho",
                                "numCovar.1" = "numCovar.1",
                                "tbar" = "tbar",
                                "alpha" = "alpha",
                                "r2.1" = "r2.1",
                                "icc.2" = "icc.2",
                                "r2.2" = "r2.2",
                                "icc.3" = "icc.3",
                                "omega.3" = "omega.3"),
                 selected = "rho") # select input buttons div
   }
  
} # number of unit per block

############################
# Number of Units per block
############################

nbarInputEx <- function(estimation, design, scenario){
  

  id <- paste0(estimation, "_", "nbar", "_", design, "_" , scenario)
  
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

jInputEx <- function(estimation, design, scenario){
  
  id <- paste0(estimation, "_", "j", "_", design, "_" , scenario)
  
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

mInputEx <- function(estimation, design, scenario){
  
    id <- paste0(estimation, "_" ,"m", "_", design, "_" , scenario)
    numericInput(id, 
                 "Number of Outcomes", 
                 min = 1, 
                 max = 10, 
                 value = 5, 
                 step = 1)
  
} # Number of Outcomes (M)

#########################
# MTP element
#########################

mtpInputEx <- function(estimation, design, scenario, varVary){
  
  id <- paste0(estimation, "_", "mtp", "_", design, "_" , scenario, "_", varVary)
  
  
  if(varVary == "mtp") {
    
    if(TRUE){
      text_val <- HTML(paste("Multiple testing procedure (MTP) ",
                             "<span style=\"color:red\"> (Input multiple values) </span>"))
    }else{
      text_val <- HTML(paste("Multiple testing procedure (MTP) Input multiple values"))
    }

 
    
    
    
    selectInput(id, text_val, 
                choices = list("Bonferroni" = "Bonferroni", 
                                "Holm" = "Holm", 
                                "Benjamini-Hochberg" = "BH", 
                                "Westfall-Young-Single-Step" = "WY-SS", 
                                "Westfall-Young-Step-Down" = "WY-SD"),
                selected = "Bonferroni",
                multiple = TRUE) # select input buttons div
} else {
    
  selectInput(id, "Multiple testing procedure (MTP) <Input only one value>", 
              choices = list("Bonferroni" = "Bonferroni", 
                             "Holm" = "Holm", 
                             "Benjamini-Hochberg" = "BH", 
                             "Westfall-Young-Single-Step" = "WY-SS", 
                             "Westfall-Young-Step-Down" = "WY-SD"),
              selected = "Bonferroni",
              multiple = TRUE) # select input buttons div
  
  } # end of else
}

mtpActionButtonEx <- function(estimation, design, scenario, varVary){
  
  question <- paste0(estimation, "question", "mtp", design, scenario, varVary, sep = "_")
  actionButton(question,
               label = "", 
               icon = icon("question"),
               style = "font-size: 10px; 
               margin-top: 28px;") #div for button ends
  
} # mtpAction Button

mtpPopOverEx <- function(estimation, design, scenario, varVary){
  
  question <- paste0(estimation, "question", "mtp", design, scenario, varVary, sep = "_")
  
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

mdesInputEx <- function(estimation, design, scenario, numOutcome, varVary){

  
  if(varVary == "mdes") {
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default mdes values whose count of number will change depending on number of outcomes
    defaultmdesvalues <- paste0(rep(0.125, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "mdes", "_", design, "_" , scenario, "_", varVary)
    
    textInput(id, 
             "Minimum detectable effect size (MDES) <Input multiple values>", 
             value = defaultmdesvalues)
  } else {
    
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default mdes values whose count of number will change depending on number of outcomes
    defaultmdesvalues <- paste0(rep(0.125, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "mdes", "_", design, "_" , scenario, "_", varVary)
    
    textInput(id, 
              "Minimum detectable effect size (MDES) <Input a single value>", 
              value = defaultmdesvalues)
  }
  
} # mdesInput

##################################################################################
# Correlation between test statistics (assumed to be the same between all pairs)
##################################################################################

rhoInputEx <- function(estimation, design, scenario, varVary){
  
  if(varVary == "rho"){
    
    id <- paste0(estimation, "_", "rho", "_", design, "_" , scenario, "_" , varVary)
    numericInput(id, 
                 "Correlation between test statistics <Input multiple values>", 
                 min = 0, 
                 max = 1, 
                 value = 0.5, 
                 step = 0.1 )
    
  } else {
    
    id <- paste0(estimation, "_", "rho", "_", design, "_" , scenario, "_" , varVary)
    numericInput(id, 
                 "Correlation between test statistics <Input a single value>", 
                 min = 0, 
                 max = 1, 
                 value = 0.5, 
                 step = 0.1 )
    
  }
    
} # rhoInput

##################################################################################
# Number of level 1 covariates
##################################################################################

numCovar.1InputEx <- function(estimation, design, scenario, varVary){
  
  if (varVary == "numCovar.1"){
    
    id <- paste0(estimation, "_", "numCovar.1", "_", design, "_", scenario, "_", varVary)
    numericInput(id, 
                 "Number of level 1 covariates <Input multiple values>", 
                 min = 0, 
                 max = 10, 
                 value = 1, 
                 step = 1 )
  } else {
    
    id <- paste0(estimation, "_", "numCovar.1", "_", design, "_", scenario, "_", varVary)
    numericInput(id, 
                 "Number of level 1 covariates <Input a single value>", 
                 min = 0, 
                 max = 10, 
                 value = 1, 
                 step = 1 )
  }
    
} # number of level 1 covariates

##################################################################################
# Proportion of treatment assignment
##################################################################################

tbarInputEx <- function(estimation, design, scenario, varVary){
  
  if (varVary == "tbar") {

    id <- paste0(estimation, "_", "tbar", "_", design, "_" , scenario, "_", varVary)
    numericInput(id, 
                 "Proportion of treatment assignment <Input multiple values>", 
                 min = 0.001, 
                 max = 1.0, 
                 value = 0.5, 
                 step = 0.001)
  } else {
    
    id <- paste0(estimation, "_", "tbar", "_", design, "_" , scenario, "_", varVary)
    numericInput(id, 
                 "Proportion of treatment assignment <Input a single value>",
                 max = 1.0, 
                 value = 0.5, 
                 step = 0.001)
  }
    
} # tbarInput

##################################################################################
# Significance level (alpha)
##################################################################################

alphaInputEx <- function(estimation, design, scenario, varVary) {

  if (varVary == "alpha"){
    
    id <- paste0(estimation, "_", "alpha", "_", design, "_", scenario, "_", varVary)
    numericInput(id, 
                 "Significance level (alpha) <Input multiple values>", 
                  min = 0.001, 
                  max = 0.9, 
                  value = 0.05,                     
                  step = 0.001)
  } else {
    
    id <- paste0(estimation, "_", "alpha", "_", design, "_", scenario, "_", varVary)
    numericInput(id, 
                 "Significance level (alpha) <Input a single value>", 
                 min = 0.001, 
                 max = 0.9, 
                 value = 0.05,                     
                 step = 0.001)
  }
    
} #alpha

#########################
# R2.1 element
#########################

r2.1InputEx <- function(estimation, design, scenario, numOutcome, varVary){
  
  if (varVary == "r2.1") {
    # if initial values are not set yet, set it at 5.  
    
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default r2.1 values whose count of number will change depending on number of outcomes
    defaultr2.1values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    
    id <- paste0(estimation, "_", "r2.1", "_", design, "_" , scenario, "_", varVary)
  
    textInput(id, 
              "Proportion of variance explained by level-1 covariates (R2.1) <Input multiple values>", 
              value = defaultr2.1values)
  } else {
    
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default r2.1 values whose count of number will change depending on number of outcomes
    defaultr2.1values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    
    id <- paste0(estimation, "_", "r2.1", "_", design, "_" , scenario, "_", varVary)
    
    textInput(id, 
              "Proportion of variance explained by level-1 covariates (R2.1) <Input a single value>", 
              value = defaultr2.1values)
  }
  
} # R2.1 Input

#########################
# R2.2 element
#########################

r2.2InputEx <- function(estimation, design, scenario, numOutcome, varVary){
  
  if(varVary == "r2.2"){
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default r2.2 values whose count of number will change depending on number of outcomes
    defaultr2.2values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    
    id <- paste0(estimation, "_", "r2.2", "_", design, "_" , scenario, "_", varVary)
    
    textInput(id, 
              "Proportion of variance explained by level-2 covariates (R2.2) <Input multiple values>", 
              value = defaultr2.2values)
  } else {
    
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default r2.2 values whose count of number will change depending on number of outcomes
    defaultr2.2values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    
    id <- paste0(estimation, "_", "r2.2", "_", design, "_" , scenario, "_", varVary)
    
    textInput(id, 
              "Proportion of variance explained by level-2 covariates (R2.2) <Input a single value>", 
              value = defaultr2.2values)
    
  }
    
} # R2.2 Input

#########################
# ICC.2 element
#########################

icc.2InputEx <- function(estimation, design, scenario, numOutcome, varVary){
  
  if (varVary == "icc.2") {
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default icc.2 values whose count of number will change depending on number of outcomes
    defaulticc.2values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "icc.2", "_", design, "_" , scenario, "_", varVary)
    textInput(id, 
              "Intraclass correlation between level-2 covariates of each outcome (ICC.2) <Input multiple values>", 
              value = defaulticc.2values)
  } else {
    
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default icc.2 values whose count of number will change depending on number of outcomes
    defaulticc.2values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "icc.2", "_", design, "_" , scenario, "_", varVary)
    textInput(id, 
              "Intraclass correlation between level-2 covariates of each outcome (ICC.2) <Input a single value>", 
              value = defaulticc.2values)
  }
  
} #ICC.2 element

#########################
# omega.2 element
#########################

omega.2InputEx <- function(estimation, design, scenario, numOutcome, varVary){
  
  if(varVary =="omega.2"){
    
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default omega.2 values whose count of number will change depending on number of outcomes
    defaultomega.2values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "omega.2", "_", design, "_" , scenario, "_", varVary)
    textInput(id, 
              "Ratio of level-2 group covariate effect size variability to random effects variability (omega.2) <Input multiple values>", 
              value = defaultomega.2values)
    
  } else {
    
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default omega.2 values whose count of number will change depending on number of outcomes
    defaultomega.2values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "omega.2", "_", design, "_" , scenario, "_", varVary)
    textInput(id, 
              "Ratio of level-2 group covariate effect size variability to random effects variability (omega.2) <Input a single value>", 
              value = defaultomega.2values)
  }
  
} # omega.2 element


#########################
# R2.3 element
#########################

r2.3InputEx <- function(estimation, design, scenario, numOutcome, varVary){
  
  if (varVary == "r2.3") {
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default r2.2 values whose count of number will change depending on number of outcomes
    defaultr2.3values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    
    id <- paste0(estimation, "_", "r2.3", "_", design, "_" , scenario, "_", varVary)
    
    textInput(id, 
              "Proportion of variance explained by level-3 covariates (R2.3) <Input multiple values>", 
              value = defaultr2.3values)
  } else {
    
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default r2.2 values whose count of number will change depending on number of outcomes
    defaultr2.3values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    
    id <- paste0(estimation, "_", "r2.3", "_", design, "_" , scenario, "_", varVary)
    
    textInput(id, 
              "Proportion of variance explained by level-3 covariates (R2.3) <Input a single value>", 
              value = defaultr2.3values)
  }
} # R2.3 Input

#########################
# ICC.3 element
#########################

icc.3InputEx <- function(estimation, design, scenario, numOutcome, varVary){
  
  if (varVary == "icc.3") {
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default icc.3 values whose count of number will change depending on number of outcomes
    defaulticc.3values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "icc.3", "_", design, "_" , scenario, "_", varVary)
    textInput(id, 
              "Intraclass correlation between level-3 covariates of each outcome (ICC.3) <Input multiple values>", 
              value = defaulticc.3values)
  } else {
    
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default icc.3 values whose count of number will change depending on number of outcomes
    defaulticc.3values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "icc.3", "_", design, "_" , scenario, "_", varVary)
    textInput(id, 
              "Intraclass correlation between level-3 covariates of each outcome (ICC.3) <Input a single value>", 
              value = defaulticc.3values)
  }
} #ICC.3 element

#########################
# omega.3 element
#########################

omega.3InputEx <- function(estimation, design, scenario, numOutcome, varVary){
  
  if(varVary == "omega.3") {
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default icc.3 values whose count of number will change depending on number of outcomes
    defaultomega.3values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "omega.3", "_", design, "_" , scenario, "_", varVary)
    textInput(id, 
              "Ratio of level-3 group covariate effect size variability to random effects variability (omega.3) <Input multiple values>", 
              value = defaultomega.3values)
  } else {
    
    # if initial values are not set yet, set it at 5.  
    if(length(numOutcome) == 0){
      
      numOutcome <- 5
      
    } # set default value to numOutcome
    
    # default icc.3 values whose count of number will change depending on number of outcomes
    defaultomega.3values <- paste0(rep(0.2, times = numOutcome), collapse = ",")
    
    id <- paste0(estimation, "_", "omega.3", "_", design, "_" , scenario, "_", varVary)
    textInput(id, 
              "Ratio of level-3 group covariate effect size variability to random effects variability (omega.3) <Input a single value>", 
              value = defaultomega.3values)

  }

} # omega.3 element

############################
# Number of 3-level Grouping
############################

kInputEx <- function(estimation, design, scenario, varVary){
  
  if(varVary == "k") {
    
    id <- paste0(estimation, "_", "k", "_", design, "_" , scenario, "_", varVary)
    
    numericInput(id,
                 "Number of level-3 groupings <Input multiple values>", 
                 min = 2, 
                 max = 100, 
                 value = 50, 
                 step = 1)
  } else {
    
    id <- paste0(estimation, "_", "k", "_", design, "_" , scenario, "_", varVary)
    
    numericInput(id,
                 "Number of level-3 groupings <Input a single value>", 
                 min = 2, 
                 max = 100, 
                 value = 50, 
                 step = 1)
  }
  
} # number of 3rd level grouping

