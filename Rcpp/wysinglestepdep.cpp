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

// NumericVector adjustAllsampsWyss (IntegerVector snum, NumericVector  )


/*
#' WestFallYoung Single Step Adjustment Function
#'
#' This adjustment function utilizes the comp.rawt.SS helper function to compare
#' each row of the matrix sample test statistics under
#' alternative hypothesis to all the rows in the matrix of the test statistics under the complete null (i.e think a distribution).
#' Furthermore, it carries out the comparison for all the samples of raw test statistics under the alternative.
#'
#' @param snum the number of samples for which test statistics under the alternative hypothesis
#' are compared with the distribution (matrix) of test statistics under the complete null (this distribution
#' is obtained through drawing test values under H0 with a default of 10,000)
#' @param abs.Zs.H0 a matrix of test statistics under the complete null
#' @param abs.Zs.H1 a matrix of raw test statistics under the alternative
#'
#' @return a matrix of adjusted test statistics values

adjust.allsamps.WYSS <- function(snum,abs.Zs.H0,abs.Zs.H1) {
  
# creating the matrix to store the adjusted test values with the number of samples &
# number of M outcomes
  adjp.WY<-matrix(NA,snum,ncol(abs.Zs.H0))
# looping through all the samples of raw test statistics under the alternative hypothesis
    doWY<-for (s in 1:snum) {
      
# using apply to compare the distribution of test statistics under H0 with 1 sample of the raw statistics under H1
      ind.B<-t(apply(abs.Zs.H0, 1, comp.rawt.SS, abs.Zs.H1.1samp=abs.Zs.H1[s,]))
# calculating the p-value for each sample
        adjp.WY[s,]<-colMeans(ind.B)
        
    }
    return(adjp.WY)
}
 
*/

