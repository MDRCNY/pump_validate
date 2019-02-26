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





