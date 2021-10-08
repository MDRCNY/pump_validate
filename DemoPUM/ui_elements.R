######################################################################################################################
# Objective : This document is to organize all the different UI elements for each of the design & different scenario #
######################################################################################################################



#########################
# Number Of MTP
#########################

# mtpInput <- function(design, scenario){
# 
# id <- paste0("mtp", design, scenario, sep = "_")
# question <- paste0("question", "mtp", design, scenario, sep = "_")
#   
# fluidRow(
#   column(10,
#          div(style = "display: inline-block, vertical-align:top;", 
#              selectInput(id, "Which MTP do you plan to use?", 
#                          choices = list("Bonferroni" = "Bonferroni", 
#                                         "Holm" = "Holm", 
#                                         "Benjamini-Hochberg" = "BH", 
#                                         "Westfall-Young-Single-Step" = "WY-SS", 
#                                         "Westfall-Young-Step-Down" = "WY-SD"),
#                          multiple = TRUE)) # select input buttons div
#   ), # column for inputs
#   
#   column(2, 
#          div(style ="display: inline-block, 
#                                              vertical-align:top;",
#              actionButton(question,
#                           label = "", 
#                           icon = icon("question"),
#                           style = "font-size: 10px;
#                           argin-top: 28px;")) #div for button ends
#   ) # column for buttons
#   
# ) # fluid Row to contain the question mark issue 
# 
# bsPopover(id = question, 
#           title = NULL,
#           content = paste0("For more information on MTP, please click!"),
#           placement = "right", 
#           trigger = "hover", 
#           options = list(container = "body")) # the bsPopover for the more information section of the Shiny App
#   
# } # mtp input

mtpInput <- function(design, scenario){
  
  id <- paste0("mtp", design, scenario, sep = "_")
  
  selectInput(id, "Which MTP do you plan to use?", 
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










