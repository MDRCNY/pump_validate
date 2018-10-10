# This is a sample R script

# Load dependencies
library(jsonlite)

# Plot a histogram of 100 random numbers
# In R code, Domino automatically saves your plots to svg files in your project's "results" folder
hist(rnorm(100))

# Generate and save some key statistics to dominostats.json
#   learn more at http://support.dominodatalab.com/hc/en-us/articles/204348169
r2 <- runif(1)
p <- runif(1)
diagnostics = list("R^2" = r2, "p-value" = p)
fileConn<-file("dominostats.json")
writeLines(toJSON(diagnostics), fileConn)
close(fileConn)
