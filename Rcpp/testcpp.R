library("Rcpp")
sourceCpp("Rcpp/hello.cpp") #hello source function
sourceCpp("Rcpp/singlestep.cpp")


# Testing the Hello World Function
hello()

# Testing the pack box function
pack_boxes(10, c(0, 0.7, 0.2, 0.1))

# Testing the Compare WestFall Young Single Step Helper Function

h0 <- Rcpp::NumericVector(5, 0.4, 0.5, 0.3, 0.2, 0.1)
h1 <- Rcpp::NumericVector(5, 0.7, 0.7, 0.7, 0.7, 0.7)

compRawtSs(h0,h1)
