######################################################################################################
## PURPOSE: Export R programs to run on Domino 
#  This is a template - fields need to be adjusted to run the code

## Before using this template make sure you have: 
#  Installed the Domino CLI
#  Integrated Domino with Github

######################################################################################################

## make sure your working directory is set to your github repo

## install and activate domino package
# install.packages("domino")
library(domino)

## Type in your user name
domino.login("yourUsername", host="https://app.dominodatalab.com")

## run script, type in script name
domino.run("yourScript.R")

## download results
domino.download()
