###########################################################################################
# Run file
  # This file converts the R Markdown files that generate power calculations into R files
  # and runs these files generating and saving down results 
###########################################################################################

options(error=traceback)

# load libraries
library(knitr)

# convert powerFunction.Rmd to R file
purl("powerFunctions.Rmd", output = "powerFunctions.R", documentation = 2)

#source files
source("powerFunctions.R")
