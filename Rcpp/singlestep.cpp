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

// The function below is a helper function to print out vectors in Rcpp/C++ for IntegerVectors

void rcpp_rprinti(IntegerVector v){
  // printing values of all the elements of Rcpp vector  
  for(int i = 0; i < v.length(); ++i){
    Rprintf("the value of v[%i] : %i \n", i, v[i]);
  }
}

// The function below is a helper function to print out vectors in Rcpp/C++ for IntegerVectors

void rcpp_rprintn(NumericVector v){
  // printing values of all the elements of Rcpp vector  
  for(int i = 0; i < v.length(); ++i){
    Rprintf("the value of v[%i] : %f \n", i, v[i]);
  }
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
  
  // Rprintf("This is the H0 value, %u \n", m);
  
  // Subtracting the index by 1 as C++ starts index at 0
  IntegerVector ooF = oo - 1;
  
  // creating an empty logical vector of length M to save boolean values
  LogicalVector maxt(m);
  // Beginning of for loop
  for (int i = 0; i < m; i++){
    
  // First Print Statement Inside the for loop
  // Rprintf("We are printing out the iteration as the first line of the for loop, %i",i);
  rcpp_rprinti(ooF);
  
  //  saving the null test statistics
  NumericVector nullOo = absZsH01row[ooF];
  
  // saving the raw test statistics under H1
  NumericVector rawtOo = absZsH11samp[ooF];

  rcpp_rprintn(rawtOo);
    
  // saving the first boolean by comparing the max of null values with the first of raw test statistics
  maxt[0] = max(nullOo) > rawtOo[0];
  
  // A secondary for loop implement the stepdown
    for (int j = 1; j < m; j ++){
      
      Rprintf("This is iteration at, %u \n", j);
      NumericVector tmp = nullOo[seq(j, m-1)];
      rcpp_rprintn(tmp);
      maxt[j] = max(nullOo[seq(j, m-1)]) > rawtOo[j];
      // Rprintf("This is the H0 value, %u \n", j);
    } // end of inner for loop
    
  } // end of outer for loop
  
  // Convert the LogicalVector type variable to an IntegerVector type variable through soft-copy.
  IntegerVector maxtInt = as<IntegerVector>(maxt);
  return maxtInt;

} // end of SD function




