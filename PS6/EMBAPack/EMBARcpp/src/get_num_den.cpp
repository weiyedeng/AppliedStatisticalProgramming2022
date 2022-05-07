#include <Rcpp.h> 
using namespace Rcpp;

//' Calculate the Weighted Density for Observed Outcomes and the Sum of Weighed Density for Each Model
//'
//' Calculate the weighted density for each observed outcome t and the sum of weighted density across k forecast models for each t.
//' 
//' @param y A numeric vector that indicates observed election outcomes.
//' @param ftk A numeric vector that indicates the mean of the forecast models. The models are assumed to follow a normal distribution.
//' @param weights A numeric vector that indicates the initial weights assigned to the forecast models.
//' @param sd A value that indicates the standard deviation of the forecast models. In this function, it is assumed to be 1 and remains consistent across the models.
//' @return A list with two elements:
//'  \item{numerator_matrix}{Each entry (t, k) documents the weighted probability of each election outcome t in each forcast model k.}
//'  \item{denominator_vector}{The sum of weighted density across k forecast models for each t.}
//' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
//' @note Refer to the equation in Equation (2). The numerator matrix refers to the values in the numerator and the denominator vectore refers to the values in the deminator.
//' @note This is an intermediate function, which is mainly used to calculate the ztk matrix in Equation (2). See how to calculate the ztk matrix by referring to the function \code{est_zsk()}.
//' @examples
//' y <- c(0.1,0.2,0.5)
//' ftk <- seq(0,1,0.1)
//' sd <- 1
//' weights <- rep(1/length(ftk), length(ftk)) ## Intially assign equal weight to each model
//' get_num_den(y=y, ftk=ftk, weights=weights, sd=1)
//'
//' @seealso \code{est_ztk}, \code{update_weights}, \code{est_finalweights}
//' @rdname get_num_den
//'
//' @export
// [[Rcpp::export]]
List get_num_den(NumericVector y, NumericVector ftk, double sd, NumericVector weights) {
  int n = y.size();
  int m = ftk.size();
  NumericVector v = NumericVector(n*m, NumericVector::get_na());
  NumericMatrix num_mat(n, m, v.begin());
  NumericVector den_vec(0);
  
  for (int i=0; i<n; i++){
    NumericVector num_vec(0);
  
    for (int j=0; j<m; j++) {
      double num = R::dnorm(y[i], ftk[j], sd, FALSE) * weights[j];
      num_vec.push_back(num);
    }
    num_mat(i,_) = num_vec;
    double den = sum(num_vec);
    den_vec.push_back(den);
  }
  
  List L=List::create(Named("numerator_matrix")=num_mat, Named("denominator_vector")=den_vec);
  return L;
}

