#include <Rcpp.h> 
using namespace Rcpp;

//' Estimate the Weights that Maximize the Probability of Observed Outcomes
//'
//' With initial parameters, this function iteratively calculates the weights that seek to maximize the probability that an outcome t is best predicted by a series of forecast models. The function stops at iteration j and return the final weights when the estimated probability at iteration j converges based on a pre-defined threshold of your choosing.
//' 
//' @param y A numeric vector that indicates observed election outcomes.
//' @param ftk A numeric vector that indicates the mean of the forecast models. The models are assumed to follow a normal distribution.
//' @param weights A numeric vector that indicates the initial weights assigned to the forecast models.
//' @param sd A value that indicates the standard deviation of the forecast models. In this function, it is assumed to be 1 and remains consistent across the models.
//' @param threshold A value that indicates the level of tolerance in the difference between the estimated probability at iteration j-1 the estimated probability at iteration j.
//' @return A list with three elements:
//'  \item{final_weights}{The final weights that maximize the probability that an outcome t is best predicted by a series of forcast models, given a pre-defined tolerance threshold.}
//'  \item{final_density}{The final maximized probability.}
//'  \item{iterations}{The number of iterations the function goes through to reach the maximized probability.}
//' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
//' @note The value in Equation (1) is what this function seeks to maximize.
//' @note The function uses the \code{est_ztk()} function (see Equation 2) and the \code{update_weights()} function (see Equation 3) to iteratively update the weights. 
//' @examples
//' y <- c(0.1,0.2,0.5)
//' ftk <- seq(0,1,0.1)
//' sd <- 1
//' weights <- rep(1/length(ftk), length(ftk)) ## Intially assign equal weight to each model
//' est_finalWeights(y=y, ftk=ftk, weights=weights, sd=1, threshold=0.0001)
//'
//' @seealso \code{get_num_den}, \code{update_weights}, \code{est_finalweights}
//'
//' @rdname est_finalWeights
//'
//' @export
// [[Rcpp::export]]
List est_finalWeights(NumericVector y, NumericVector ftk, double sd, NumericVector weights, double threshold) {
  Function get_num_den("get_num_den");
  Function est_ztk("est_ztk");
  Function update_weights("update_weights");
  
  List L = get_num_den(y, ftk, sd, weights);
  NumericVector den_vec = L[1];
  
  NumericVector new_weights = update_weights(est_ztk(y, ftk, sd, weights));
  List new_L = get_num_den(y, ftk, sd, new_weights);
  NumericVector new_den_vec = new_L[1];
  
  NumericVector diff_den_vec = abs(new_den_vec - den_vec);
  NumericVector threshold_vec = NumericVector(diff_den_vec.size(), threshold);
  double iter = 1;
  
  while(is_true(any(diff_den_vec >= threshold_vec))) { // if the diff is larger than the threshold
    den_vec = new_den_vec; // replace the den_vec from last iteration to the current iteration
    new_weights = update_weights(est_ztk(y, ftk, sd, new_weights)); // similarly, replace the new_weights
    new_L = get_num_den(y, ftk, sd, new_weights); // similarly, replace the new_L by inputting the new_weights
    new_den_vec = new_L[1]; // Extract the next iteration of den_vec
    diff_den_vec = abs(new_den_vec - den_vec); // Extract the new diff_den_vec
    iter++;
  }
  
  List out_L = List::create(Named("final_weights")=new_weights,
                            Named("final_density")=new_den_vec,
                            Named("iterations")=iter);
  return out_L;
}
