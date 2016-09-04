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
    //initializing the indexed values
    int newstateScalar = newstates[i];
    double cum_probs = cumuprobs[i];

    // generate random numbers for every case
    NumericVector rno = runif(ncases);

    //  multiplication of numeric(double scalar) and numericVector is possible
    //  and so is integer(scalar) and IntegerVector
    //  Calculation for the formulas below:

    // subset the sMatrix for origin and newstate(s)
    sMatrix(_,newstateScalar) = //equation here: s.matrix[,i]+(s.matrix[,origin]*(rand<probs_for)*(rand>last_prob)
    sMatrix(_,origin) = //equation here: s.matrix[,origin]-(s.matrix[,origin]*(rand<probs_for)*(rand>last_prob))

    // test which cases will advance to next state based on the cumulative probabilities (cumuprobs)

  }


  return sMatrix;
}



/*** R
#p <- syn_pop(c(2,3,1))
stateT(2, 3)
stateT(c(3,2), c(2,9))
*/
