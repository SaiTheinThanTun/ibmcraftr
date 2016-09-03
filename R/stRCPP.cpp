#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix stateT(int origin, IntegerVector newstates, NumericVector cumuprobs, IntegerMatrix sMatrix)
{
  // newstates and probs must be of same length

  // identify the number of cases
  int ncases = sMatrix.nrow();

  // generate random numbers for every case
  NumericVector rno = runif(ncases);


  // subset the sMatrix for origin and newstate(s)


  // test which cases will advance to next state based on the cumulative probabilities (cumuprobs)


  return sMatrix;
}



/*** R
#p <- syn_pop(c(2,3,1))
stateT(2, 3)
stateT(c(3,2), c(2,9))
*/
