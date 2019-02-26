#include <Rcpp.h>
using namespace Rcpp;

// The function below is a helper function for comparison of Single Step WestFall Young.
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
  for (int i = 1; i < m; i++){
    
    int tmp0 = max(absZsH01row);
    int tmp1 = absZsH11samp[i];
    
    Rprintf("This is the H0 value, %u", tmp0);
    Rprintf("This is the H1 value, %u", tmp1);
    
    maxt[i] = max(absZsH01row) > absZsH11samp[i];
    // Rprintf("This is the boolean value, %u", maxt[i]);
  } // end of for loop
  
  IntegerVector maxtInt = as<IntegerVector>(maxt);
  
  return maxtInt;
}





