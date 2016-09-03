#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix stateT(int origin, IntegerVector newstates, NumericVector cumuprobs, IntegerMatrix sMatrix)
{
  // newstates and probs must be of same length

  // identify the number of cases
  int ncases = sMatrix.nrow();

  // for loop for each new state starts here
  // remember that the index in C++ starts at 0
  for (int i=0; i<newstates.size(); i++){
    // generate random numbers for every case
    NumericVector rno = runif(ncases);

    //following codes could be used
    int tmp = test(i) < x;
    result(i) = tmp*2;

    //expend
    NumericVector cumuprobs =
      //multiplication of numeric(double scalar) and numericVector is possible
      //and so is integer(scalar) and IntegerVector

    // subset the sMatrix for origin and newstate(s)
    sMatrix(_,i) = //equation here
      sMatrix(_,origin) = //equation here

      // test which cases will advance to next state based on the cumulative probabilities (cumuprobs)

  }


  return sMatrix;
}



/*** R
#p <- syn_pop(c(2,3,1))
stateT(2, 3)
stateT(c(3,2), c(2,9))
*/
