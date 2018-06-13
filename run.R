###########################################################################################
# Run file
  # This file converts the R Markdown files that generate power calculations into R files
  # and runs these files generating and saving down results 
###########################################################################################

options(error=traceback)

#source("libraries.install.R")

#load libraries
library(yaml)
library(knitr)

#read in configs file
configs <- yaml.load_file("configs.yaml")

#set file directory
fdir <- configs$fdir

#run powerFunctions file
if (configs$run_powerFunctions == TRUE)
{

  # convert powerFunction.Rmd to R file
  #purl("powerFunctions.Rmd", output = "powerFunctions.R", documentation = 2)
  
  #source files
  source("powerFunctions.R")

}



