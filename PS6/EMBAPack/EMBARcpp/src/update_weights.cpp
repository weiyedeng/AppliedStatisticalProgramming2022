#include <Rcpp.h> 
using namespace Rcpp;

//' Update Weights with a ztk Matrix
//'
//' Update weights with a ztk matrix by estimating the expected value of the weights across observed outcomes t.
//' 
//' @param ztk_mat A ztk matrix output from the function \code{est_ztk{}}, with the entry (t, k) representing the probability that observation t was best predicted by model k.
//' @return A vector that provides the new weights for each model k.
//' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
//' @note Refer to the equation in Step 3.
//' @examples
//' library(EMBARcpp)
//' y <- c(0.1,0.2,0.5)
//' ftk <- seq(0,1,0.1)
//' sd <- 1
//' weights <- rep(1/length(ftk), length(ftk)) ## Intially assign equal weight to each model
//' ## Make sure `est_ztk()` is available in the global envrionment; NOT RUN
//' ## update_weights(esk_ztk(y=y, ftk=ftk, weights=weights, sd=1))
//'
//' @seealso \code{get_num_den}, \code{est_ztk}, \code{est_finalweights}
//' @rdname update_weights
//'
//' @export
// [[Rcpp::export]]
NumericVector update_weights(NumericMatrix ztk_mat) {
  NumericVector new_weights = colMeans(ztk_mat);
  return new_weights;
}

