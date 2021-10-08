######################################################################################################################
# Objective : This document is to organize all the different UI elements for each of the design & different scenario #
######################################################################################################################

#########################
# MTP element
#########################

mtpInput <- function(design, scenario){
  
  id <- paste0("mtp", "_", design, "_" , scenario)
  
  selectInput(id, "Multiple Testing Procedure (MTP)", 
              choices = list("Bonferroni" = "Bonferroni", 
                              "Holm" = "Holm", 
                              "Benjamini-Hochberg" = "BH", 
                              "Westfall-Young-Single-Step" = "WY-SS", 
                              "Westfall-Young-Step-Down" = "WY-SD"),
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
# Number of Outcomes (M)
#########################

mInput <- function(design, scenario){
  
  id <- paste0("M", "_", design, "_" , scenario)
  numericInput(id, 
               "Number of Outcomes", 
               min = 1, 
               max = 10, 
               value = 5, 
               step = 1)
  
  
} # Number of Outcomes (M)

#########################
# Minimum Detectable Effect Size
#########################

mdesInput <- function(design, scenario){
  
  id <- paste0("mtp", "_", design, "_" , scenario)
  textInput(id, 
           "Vary MDES vector (comma delimited)", 
           value = "0.125,0.125,0.125, 0,0")
} # mdesInput

#########################
# R2.1 element
#########################

r2.1Input <- function(design, scenario){
  
  id <- paste0("mtp", "_", design, "_" , scenario)

  textInput(id, 
            "Proportion of variance explained by Level-1 covariates (R2.1)", 
            value = "0.2, 0.2, 0.2, 0.2, 0.2")
} # R2.1 Input






