#' Calculate the MLE Estimator of Poisson Distribution
#'
#' Calculate the MLE estimator of Poisson distribution based on observation data.
#'
#' @param y The vector of observed data.
#'
#' @return A numeric value indicating the MLE estimator.
#' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
#' @seealso \code{\link[PoisMLE]{logLik}}, \code{\link[PoisMLE]{standardError}}, \code{\link[PoisMLE]{estimatePois}}
#' @examples
#' set.seed(123)
#' y <- sample(x=1:10, size=20, replace=TRUE)
#' mle(y)
#' @rdname mle
#' @include mle.R
#' @import methods

#' @export
setGeneric(name="mle",
           def=function(y)
           {standardGeneric("mle")}
)

setMethod(f="mle",
          definition=function(y){
            ## mle equals to the mean of y
            ## Count data has to be >= 0
            if (any(y < 0)) {
              stop("Every value of y must be larger than or equal to 0.")
            }
            
            return(mean(y))
          }
)