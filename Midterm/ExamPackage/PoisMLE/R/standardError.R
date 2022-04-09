#' Calculate the Standard Error associated with the MLE Estimator (Poisson)
#'
#' Calculate the standard rrror associated with the MLE estimator (Poisson)
#'
#' @param y The vector of observed data.
#' @param SEtype The type of standard errors, which can be either \code{basic} or \code{bootstrap}.
#' @param B The number of bootstrapped resamplings, only useful when \code{SEtype = 'bootstrap'}. The default is 100.  
#'
#' @return A numeric value indicating the standard error.
#' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
#' @seealso logLik, mle, estimatePois
#' @examples
#' set.seed(123)
#' y <- sample(x=1:10, size=20, replace=TRUE)
#' standardError(y, SEtype = 'bootstrap')
#' @rdname standardError
#' @include standardError.R
#' @import methods
#' @importFrom stats sd


#' @export
setGeneric(name="standardError",
           def=function(y, SEtype = c("basic", "bootstrap"), B=100)
           {standardGeneric("standardError")}
)

setMethod(f="standardError",
          definition=function(y, SEtype = c("basic", "bootstrap"), B=100){

            ## Count data has to be >= 0; Limit SEtype to the two options
            if (any(y < 0)) {
              stop("Every value of y must be larger than or equal to 0.")
            }
            
            if (!(SEtype %in% c("basic", "bootstrap"))) {
              stop("SEtype must be either 'basic' or 'bootstrap'.")
            }
            
            ## MAIN CODES
            n <- length(y)
            if (SEtype == "basic") {
              se <- sqrt(mle(y) / n)
            }
            
            if (SEtype == "bootstrap") {
              ### resampling with replacement B times
              re_ys_mat <- replicate(B, {
                return(sample(y, n, replace = T))
              })
              
              ### Get the MLE for each resampled y (column)
              re_ys_mle <- apply(re_ys_mat, 2, mle)
              se <- sd(re_ys_mle)
            }  
            return(se)
          }
)