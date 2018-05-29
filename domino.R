######################################################################################################
## Install Domino R package
 # Purpose: Export R programs to run on Domino 

## Instructions for using the Domino package can be found on the Domino web site
  # https://support.dominodatalab.com/hc/en-us/articles/204842925-Domino-R-Package

## Make sure the Domino CLI is installed before following these instructions
  # https://support.dominodatalab.com/hc/en-us/articles/204856475-Installing-the-Domino-Client-CLI-

######################################################################################################

## install and activate domino package
# install.packages("domino")
library(domino)

## Type in your user name
domino.login("yourUsername", host="https://app.dominodatalab.com")

## set your working directory
setwd("I:/Multiplicity/git/yourrepo/Power-Under-Multiplicity")

## run script, type in script name
domino.run("yourScript.R")

## download results
domino.download()
