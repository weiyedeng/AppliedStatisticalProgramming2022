#include <Rcpp.h> 
using namespace Rcpp;

//' Calculate the Latent Quantity ztk
//'
//' Calculate the Latent Quantity ztk, which is the fraction between the weighted density for each observed outcome t and the sum of weighted density across k forecast models for each t.
//' 
//' @param y A numeric vector that indicates observed election outcomes.
//' @param ftk A numeric vector that indicates the mean of the forecast models. The models are assumed to follow a normal distribution.
//' @param weights A numeric vector that indicates the initial weights assigned to the forecast models.
//' @param sd A value that indicates the standard deviation of the forecast models. In this function, it is assumed to be 1 and remains consistent across the models.
//' @return A matrix, with the entry (t, k) representing the probability that observation t was best predicted by model k.
//' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
//' @note Refer to the equation in Step 2.
//' @examples
//' y <- c(0.1,0.2,0.5)
//' ftk <- seq(0,1,0.1)
//' sd <- 1
//' weights <- rep(1/length(ftk), length(ftk)) ## Intially assign equal weight to each model
//' est_ztk(y=y, ftk=ftk, weights=weights, sd=1)
//'
//' @seealso \code{get_num_den}, \code{update_weights}, \code{est_finalweights}
//'
//' @rdname est_ztk
//'
//' @export
// [[Rcpp::export]]
NumericMatrix est_ztk(NumericVector y, NumericVector ftk, double sd, NumericVector weights) {
  int n = y.size();
  int m = ftk.size();
  NumericVector v = NumericVector(n*m, NumericVector::get_na());
  NumericMatrix ztk_mat(n, m, v.begin());
  Function get_num_den("get_num_den");
  
  List L = get_num_den(y, ftk, sd, weights);
  NumericMatrix num_mat = L[0];
  NumericVector den_vec = L[1];
  
  for (int i=0; i<n; i++){
    ztk_mat(i,_) = num_mat(i,_) / den_vec[i];
  }
  return ztk_mat;
}
