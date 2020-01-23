#include <Rcpp.h>
using namespace Rcpp;
extern "C" double digamma(double);
//[[Rcpp::export]]
void hello()
{
  Rprintf("Hello, world!\n"); 
}

//[[Rcpp::export]]
List pack_boxes(int n, NumericVector p) {
  
  // n is the number of items to simulate
  // p the probability weight of each of the item draws
  
  Function sample = Environment("package:base")["sample"];
  
  // Sampling the item weights
  int w = p.size(); // Count of the sample
  IntegerVector item = sample(w,n, true, p); // sampling the item weights
  
  // Initialize loop variables
  IntegerVector weight(n); // Vector to hold weight of boxes
  weight[0] = item[0];
  
  IntegerVector first(n);
  first[0] = 1; // Vector to hold the number of boxes we would have
  int n_boxes = 1;
  
  for (int i = 0; i < n; i++){
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
    _["item_weight"] = item, 
    _["box_holding_weight"] = weight, 
    _["total_number_of_boxes"] = first[seq(0, n_boxes -1)]
  );

} // end of function


//[[Rcpp::export]]
List listsubset(NumericVector sample){
  
  int length = sample.size();
  NumericVector item1 = sample[seq(1,length -1)];
  double item1Max = max(item1);
  NumericVector item2 = sample[seq(2, length -1)];
  double item2Max = max(item2);
  NumericVector item3 = sample[seq(3, length -1)];
  double item3Max = max(item3);
  NumericVector item4 = sample[seq(4, length -1)];
  double item4Max = max(item4);
  
  return List::create(
    item1 = item1Max,
    item2 = item2Max,
    item3 = item3Max,
    item4 = item4Max
  );
} 
