#' Calculate the Log Likelihood of Poisson Distribution
#'
#' Calculate the log likelihood of Poisson distribution
#'
#' @param y The vector of observed data.
#' @param lambda The assumed value of \eqn{\lambda}.
#'
#' @return A numeric value indicating the log likelihood.
#' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
#' @seealso mle, standardError, estimatePois
#' @examples
#' set.seed(123)
#' y <- sample(x=1:10, size=20, replace=TRUE)
#' logLik(y,2)
#' @rdname logLik
#' @include logLik.R
#' @import methods

#' @export
setGeneric(name="logLik",
           def=function(y, lambda)
           {standardGeneric("logLik")}
)

setMethod(f="logLik",
          definition=function(y, lambda){
  
            ## Count data has to be >= 0; lambda has to be > 0
            if (any(y < 0)) {
              stop("Every value of y must be larger than or equal to 0.")
            }
            if (lambda < 0) {
              stop("lambda must be larger than 0.")
            }
            
            ## MAIN CODES
            n <- length(y)
            LL <- -n*lambda - sum(log(factorial(y))) + log(lambda)*sum(y)
            
            return(LL)
          }
)