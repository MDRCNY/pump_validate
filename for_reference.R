

#convert RMD file to R file with documentation
library(knitr)
purl("powerFunctions.Rmd", output = "powerFunctions.R", documentation = 2)
purl("MonteCarloSimulation.Rmd", output = "MonteCarloSimulation.R", documentation = 2)


#convert R script to RMD file 
library(rmarkdown)
#include this code at end of R Script
rmarkdown::render('powerFunctions.R')

