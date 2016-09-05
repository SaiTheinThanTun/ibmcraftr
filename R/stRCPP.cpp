#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix stateT(int origin, IntegerVector newstates, NumericVector cumuprobs, NumericMatrix sMatrix)
{
  // length of cumuprobs is +1 than newstates

  // identify the number of cases
  int ncases = sMatrix.nrow();
  double last_probs = cumuprobs[0];
  NumericMatrix sMatrixTMP = sMatrix;

  //  adjustment for the state index (R starts at 1 and C++ starts at 0)
  newstates = newstates-1;
  origin = origin-1;

  // for loop for each new state starts here
  // remember that the index in C++ starts at 0
  for (int i=0; i<newstates.size(); i++){
    //initializing the indexed values
    int newstateScalar = newstates[i];
    double probs_for = cumuprobs[i+1]; // state will advance if rand is between last_probs and probs_for

    // generate random numbers for every case
    NumericVector rno = runif(ncases);

    //  multiplication of numeric(double scalar) and numericVector is possible
    //  and so is integer(scalar) and IntegerVector
    //  Calculation for the formulas below:
    LogicalVector tmp = rno < probs_for;
    NumericVector test1 = as<NumericVector> (tmp);
    // test which cases will advance to next state based on the cumulative probabilities (cumuprobs)
    LogicalVector tmp2 = rno > last_probs;
    NumericVector test2 = as<NumericVector> (tmp2);

    NumericVector change = sMatrix(_,origin) * test1 * test2;

    // subset the sMatrix for origin and newstate(s)
    sMatrixTMP(_,newstateScalar) = sMatrix(_,newstateScalar) + change; //equation here: s.matrix[,i]+(s.matrix[,origin]*(rand<probs_for)*(rand>last_prob)
    sMatrixTMP(_,origin) = sMatrix(_,origin) - change; //equation here: s.matrix[,origin]-(s.matrix[,origin]*(rand<probs_for)*(rand>last_prob))

    // advance the last_probs
    last_probs = probs_for;
  }


  return sMatrixTMP;
}



/*** R
p <- syn_pop(c(2,3,1))
stateT(2, 3, c(.5,1), p)
#stateT(c(3,2), c(2,9))
*/
