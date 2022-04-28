#' Weight estimation by EM algorithm for forecast models
#'
#' Apply EM algorithm to estimate weights for each forecast model by maximizing the probability of observing the election outcomes
#'
#' @param y A numeric vector that indicates observed election outcomes.
#' @param ftk A numeric vector that indicates the mean of the forecast models. The models are assumed to follow a normal distribution.
#' @param sd A value that indicates the standard deviation of the forecast models. In this function, it is assumed to be 1 and remains consistent across the models. 
#' @param weights A numeric vector that indicates the initial weights assigned to the forecast models. 
#' @param threshold A numeric value that indicates the 
#'
#' @return A list with the elements
#'  \item{numerator_matrix}{The weighted probability of each election outcome.}
#'  \item{denominator_vector}{The final probability of observing each election outcome.} 
#'  \item{iterations}{Number of iterations the EM algorithms go through to update the weights.}
#' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
#' @note Weight estimation by EM algorithm for forecast models
#' @examples
#' y <- c(0.1,0.2,0.5)
#' ftk <- seq(0,1,0.1)
#' sd <- 1
#' weights <- rep(1/length(ftk), length(ftk)) ## Intially assign equal weight to each model
#' est_finalWeights(y=y, ftk=ftk, weights=weights, sd=1, threshold=0.0001)
#' 
#' @rdname est_finalWeights
#' @export
setGeneric(name="get_num_den",
           def=function(y, ftk, sd, weights)
           {standardGeneric("get_num_den")}
)

#' @export
cppFunction('List get_num_den(NumericVector y, NumericVector ftk, double sd, NumericVector weights) {
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
}')



setMethod(f="get_num_den",
          definition=get_num_den)