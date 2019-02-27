#include <Rcpp.h> // This sentence enables you to use classes and functions defined by the Rcpp package.
using namespace Rcpp;

// The function below is a helper function for comparison of Single Step WestFall Young.

//  By specifying export, we stipulate that the function defined just below this sentence will be accessible from R.
// [[Rcpp::export]]
IntegerVector compRawtSs (NumericVector absZsH01row, NumericVector absZsH11samp) {
  
  // abs.Zs.H0.1row A vector of permutated test statistics values under H0
  // abs.Zs.H1.1samp One sample of raw statistics
  // oo Order matrix of test statistics in descending order (Only used in Step Down)

  // getting the number of M outcomes from 1 row of H0  
  int m = absZsH01row.size();
  // creating an empty logical vector of length M to save boolean values
  LogicalVector maxt(m);
  // Beginning of for loop
  for (int i = 0; i < m; i++){
    
    // Testing functions
    /*
    double tmp0 = max(absZsH01row);
    double tmp1 = absZsH11samp[i];
    
    Rprintf("This is the H0 value, %f \n", tmp0);
    Rprintf("This is the H1 value, %f \n", tmp1);
    */
    
    maxt[i] = max(absZsH01row) > absZsH11samp[i];
    // Rprintf("This is the boolean value, %u \n", maxt[i]);
  } // end of for loop
  
  // Convert the LogicalVector type variable to an IntegerVector type variable through soft-copy.
  IntegerVector maxtInt = as<IntegerVector>(maxt);
  
  return maxtInt;
}

// The function below is a helper function for comparison of StepDown WestFall Young.
//  By specifying export, we stipulate that the function defined just below this sentence will be accessible from R.
// [[Rcpp::export]]
IntegerVector compRawtSd (NumericVector absZsH01row, NumericVector absZsH11samp, IntegerVector oo){
  
  // abs.Zs.H0.1row A vector of permutated test statistics values under H0
  // abs.Zs.H1.1samp One sample of raw statistics
  // oo Order matrix of test statistics in descending order (Only used in Step Down)
  
  // getting the number of M outcomes from 1 row of H0  
  int m = absZsH01row.size();
  // creating an empty logical vector of length M to save boolean values
  LogicalVector maxt(m);
  // Beginning of for loop
  for (int i = 0; i < m; i++){
    
  //  saving the null test statistics
  NumericVector nullOo = absZsH01row[oo];
  
  // saving the raw test statistics under H1
  NumericVector rawtOo = absZsH11samp[oo];
  
  // saving the first boolean by comparing the max of null values with the first of raw test statistics
  maxt[0] = max(nullOo) > rawtOo[0];
  
  // Step-down comparison where the next max of null values is compared to the next raw test statistics
  // for (h in 2:M) {
  //  maxt[h] <- max(nullt.oo[-(1:(h-1))]) > rawt.oo[h]
  // } # end of for loop
  
  } // end of for loop
  
  
  
  
  
}



/*
#' Helper Functions for WestFallYoung Step down
#'
#' @param abs.Zs.H0.1row A vector of permutated test statistics values under H0
#' @param abs.Zs.H1.1samp One sample of raw statistics
#' @param oo Order matrix of test statistics in descending order
#' @return returns a vector of 1s and 0s with lengths of M outcomes
#' @export
#'
comp.rawt.SD <- function(abs.Zs.H0.1row, abs.Zs.H1.1samp, oo) {
  
# getting M number of outcomes from 1 row of statistics
  M <- length(abs.Zs.H0.1row)
# creating an empty vector of length M to save boolean values
    maxt <- rep(NA, M)
# saving the null test statistics
    nullt.oo <- abs.Zs.H0.1row[oo]
# saving the raw test statistics under H1
  rawt.oo <- abs.Zs.H1.1samp[oo]
# saving the first boolean by comparing the max of null values with the first of raw test statistics
  maxt[1] <- max(nullt.oo) > rawt.oo[1]
  
# Step-down comparison where the next max of null values is compared to the next raw test statistics
  for (h in 2:M) {
    maxt[h] <- max(nullt.oo[-(1:(h-1))]) > rawt.oo[h]
  } # end of for loop
    
    return(as.integer(maxt))
}
*/


