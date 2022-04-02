#' An integral approximation object 
#'
#' Print out the integrated value
#'
#' @param object A Simpson object
#'
#' @return The integrated value
#'  \item{Integrated_value}{The integrated value}
#' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
#' @note Approximation of integration
#' @examples
#' fx <- function(x) {
#' return(x^3 + x^2 + 1)
#' }
#' x <- seq(0,3,0.1)
#' y <- fx(x)
#' ends <- c(0,3)
#' a <- integrateIt(x=x,y=y,ends=ends,Rule="Trapezoid")
#' integrate(fx,0,3) ## Compared with the result calculated by integrate()
#' print(a[[1]])
#' @seealso integrateIt
#' @include show_Simpson.R

#' @export
setMethod(f="show", signature=c(object="Simpson"),
          function(object){
            val <- object@integrated_value
            names(val) <- "integrated value"
            return(val)
          }
) 




