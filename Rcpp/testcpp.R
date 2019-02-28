library("Rcpp")
sourceCpp("Rcpp/hello.cpp") # hello source function
sourceCpp("Rcpp/singlestep.cpp") # westfallyoung single step source function
source("blockrct2_power.R") # For counter-checking answers


# Testing the Hello World Function
hello()

# Testing the pack box function
pack_boxes(10, c(0, 0.7, 0.2, 0.1))

# Testing the Compare WestFall Young Single Step Helper Function

h0 <- c(0.4, 0.5, 0.3, 0.2, 0.1)
h1 <- c(0.5, 0.6, 0.3, 0.25, 0.75)

test <- compRawtSs(h0,h1)

# Test out how to use R apply function with a cpp function for Single Step

## first create a matrix of h0 and h1 values
h0.mat <- matrix(data = h0, nrow = 5, ncol = 5, byrow = TRUE)
h1.mat <- matrix(data = h1, nrow = 5, ncol = 5, byrow = TRUE)

## second using apply function with the Rcpp function & pure R

results.rcpp = apply(X = h0.mat, MARGIN = 1, FUN = compRawtSs, absZsH11samp = h1.mat[1,])
results.r = apply(X = h0.mat, MARGIN = 1, FUN = comp.rawt.SS, abs.Zs.H1.1samp = h1.mat[1,])

## third compare to see if the results are the same

if(sum(results.rcpp == results.r) == 25){print("Function working!")}

# Test out how to use R apply function with a cpp function for Step Down

## first create a matrix of h0 and h1 values
h0.mat <- matrix(data = h0, nrow = 5, ncol = 5, byrow = TRUE)
h1.mat <- matrix(data = h1, nrow = 5, ncol = 5, byrow = TRUE) 

## second using apply function with the Rcpp function & pure R

results.rcpp.sd = apply(X = h0.mat, MARGIN = 1, FUN = compRawtSd, absZsH11samp = h1.mat[1,])
results.r.sd = apply(X = h0.mat, MARGIN = 1, FUN = comp.rawt.SD, abs.Zs.H1.1samp = h1.mat[1,])

## third compare to see if the results are the same

if(sum(results.rcpp.sd == results.r.sd) == 25){print("Function working!")}


# Trying out the Vector Subsetting code from Kristin

## have an initial vector. A very pretty code. Ingenius.

tmp <- c(0.6, 0.5, 0.3, 0.4, 0.3)
tmp.res1 <- max(tmp[-(1:1)])
tmp.res1
tmp.res2 <- max(tmp[-(1:2)])
tmp.res2
tmp.res3 <- max(tmp[-(1:3)])
tmp.res3 
tmp.res4 <- max(tmp[-(1:4)])
tmp.res4

tmp.r <- list(tmp.res1, tmp.res2, tmp.res3, tmp.res4)


# Using a function from hello.cpp to test out similar C++ subsetting features
tmp <- c(0.6, 0.5, 0.3, 0.4, 0.3)
tmp.rcpp <- listsubset(tmp)









## Deni's code help

deni <- as.data.frame(matrix(data = h0, nrow = 5, ncol = 5, byrow = TRUE))
zarni <- as.data.frame(matrix(data = h1, nrow = 5, ncol = 5, byrow = TRUE))

rownames <- c("JB", "DT", "IT", "JW", "SS")
row.names(deni) <- rownames
row.names(zarni) <- rownames

rowtocolumn <- function(df){
  
  df$colname <- row.names(df)
  return (df$colname)
  
}

dlist <- list(deni,zarni)
tmp <- lapply(dlist, rowtocolumn)
done <- mapply(cbind, dlist, "mtp" = tmp, SIMPLIFY = F)








