library("Rcpp")
sourceCpp("Rcpp/hello.cpp") # hello source function
sourceCpp("Rcpp/singlestep.cpp") # westfallyoung single step source function


# Testing the Hello World Function
hello()

# Testing the pack box function
pack_boxes(10, c(0, 0.7, 0.2, 0.1))

# Testing the Compare WestFall Young Single Step Helper Function

h0 <- c(0.4, 0.5, 0.3, 0.2, 0.1)
h1 <- c(0.5, 0.6, 0.3, 0.25, 0.75)

test <- compRawtSs(h0,h1)
test.int <- as.integer(test)