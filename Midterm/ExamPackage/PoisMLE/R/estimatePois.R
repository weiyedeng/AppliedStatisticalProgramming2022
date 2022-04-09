#' Estimate Statistics Related to Poisson Distribution
#'
#' Estimate statistics related to Poisson distribution, including the log likelihood, MLE estimate, and its associated standard error. 
#'
#' @param y The vector of observed data.
#' @param lambda The assumed value of \eqn{\lambda}.
#' @param SEtype The type of standard errors, which can be either \code{basic} or \code{bootstrap}.
#' @param B The number of bootstrapped resamplings, only useful when \code{SEtype = 'bootstrap'}. The default is 100. 
#'
#' @return An object of S4 class \code{PoisMLE} with following slots:
#' \item{y}{The original data.}
#' \item{MLE}{The maximum likelihood estimator for this dataset.}
#' \item{LL}{The log likelihood calculated from the observed data assuming the MLE is correct.}
#' \item{SE}{The standard error for the MLE.}
#' \item{SEtype}{The method used to calculate the standard error.}
#' 
#' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
#' @seealso \code{\link[PoisMLE]{logLik}}, \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standardError}}
#' @examples
#' set.seed(123)
#' y <- sample(x=1:10, size=20, replace=TRUE)
#' estimatePois(y, lambda = 2, SEtype = "bootstrap")
#' @rdname estimatePois
#' @include estimatePois.R
#' @import methods

#' @export
setGeneric(name="estimatePois",
           def=function(y, lambda, 
                        SEtype = c("basic", "bootstrap"), B=100)
           {standardGeneric("estimatePois")}
)

setMethod(f="estimatePois",
          definition=function(y, lambda, 
                              SEtype = c("basic", "bootstrap"), B=100){
            MLE <- mle(y)
            LL <- logLik(y, lambda)
            SE <- standardError(y, SEtype, B)
            PoisMLE_obj <- new("PoisMLE", y=y, LL=LL, MLE=MLE, SE=SE, SEtype=SEtype)
            return(PoisMLE_obj)
          }
)