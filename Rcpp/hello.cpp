#include <Rcpp.h>
using namespace Rcpp;
extern "C" double digamma(double);
//[[Rcpp::export]]
void hello()
{
  Rprintf("Hello, world!\n"); 
}

//[[Rcpp::export]]
List pack_boxes(int n,NumericVector p) {
  
  // n is the number of steps to simulate
  // p the probability of the item weights
  
  Function sample = Environment("package:base")["sample"];
  
  // Sample item weights
  int w = p.size();
  IntegerVector item = sample(w,n, true, p);
  
  // Initialize loop variables
  IntegerVector weight(n); // Vector to hold weight of boxes
  weight[0] = item[0];
  
  IntegerVector first(n);
  first[0] = 1; // Vector to hold the number of boxes we would have
  int n_boxes = 1;
  
  for (int i = 1; i < n; i++){
    int new_weight = weight[i - 1] + item[i]; // initial weight of the current box
    
    if (new_weight <= w) {
      // Continue with current box.
      weight[i] = new_weight;
    } else {
      // Start a new box
      weight[i] = item[i];
      // Count the number of boxes we have
      first[n_boxes++] = i + 1;
    }
  
  } // simulating weights of items and boxing things up // end of for loop
  
  return List::create(
    _["item"] = item, 
    _["weight"] = weight, 
    _["first"] = first[seq(0, n_boxes -1)]
  );

} // end of function
