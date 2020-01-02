# This is a sample R model

# Load dependencies
library(jsonlite)

# Define a function to create an API
# To call model use: {"data": {"min": 1, "max": 100}}
# Learn more at http://support.dominodatalab.com/hc/en-us/articles/204173149
my_model <- function(min, max) {
  random_number <- runif(1, min, max)
  return(list(number=random_number))
}
