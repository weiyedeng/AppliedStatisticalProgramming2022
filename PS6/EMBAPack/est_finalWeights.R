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
#'  \item{final_weights}{The estimated weights that maximize the probability of observing each election outcome.}
#'  \item{final_density}{The final probability of observing each election outcome.} 
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
setGeneric(name="est_finalWeights",
           def=function(y, ftk, sd, weights, threshold)
           {standardGeneric("est_finalWeights")}
)

#' @export
setMethod(f="est_finalWeights",
          definition=function(x, fun, ends, Rule=c("Trapezoid","Simpson")){
            y <- fun(x)
            if (Rule == "Trapezoid") {
              n <- length(x) - 1
              h <-(ends[2] - ends[1]) / n
              int_T <- h / 2 * (y[1] + sum(2*y[2:n]) + y[n+1])
              Integrated <- new("Trapezoid", x=x, fun=fun, ends=ends, Rule="Trapezoid", integrated_value=int_T)
              return(list(Integrated = Integrated, 
                          input = list(x=x,fun=fun), 
                          output = int_T))
            }
            
            if (Rule == "Simpson") {
              n <- length(x) - 1
              h <-(ends[2] - ends[1]) / n
              if (n > 2) {
                int_S <- h / 3 * (y[1] + sum(4*y[seq(2,n,2)]) + sum(2*y[seq(3,n,2)]) + y[n+1])
              } else { ## equivalent to when n = 2. Validation requires x has to be >= 3 (that is, n has to be >= 2).
                int_S <- h / 3 * (y[1] + 4*y[2] + y[3])
              }
              
              Integrated <- new("Simpson", x=x, fun=fun, ends=ends, Rule="Simpson", integrated_value=int_S)
              return(list(Integrated = Integrated, 
                          input = list(x=x,fun=fun), 
                          output = int_S))
            }
            
            
          }
)