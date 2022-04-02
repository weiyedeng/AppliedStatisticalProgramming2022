#' An integral approximation object 
#'
#' Apply the trapezoidal or Simpson's rule to approximate integration
#'
#' @param x A numeric vector.
#' @param y A numeric vector of evaluated values (`y[i] = f(x[i])`), with the same length as \code{x}.
#' @param ends A numeric vector of length 2, indicating the starting and ending value for integration. 
#' @param Rule A character string. Either `Trapezoid` or `Simpson` is allowed.
#'
#' @return A list with the elements
#'  \item{Integrated}{An object of class `Trapezoid` or class `Simpson`}
#'  \item{input}{x and y} 
#'  \item{output}{The result of approximation}
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
#' @seealso print.Trapezoid, print.Simpson
#' @rdname integrateIt
#' @export
setGeneric(name="integrateIt",
           def=function(x, y, ends, Rule, ...)
           {standardGeneric("integrateIt")}
)

#' @export
setMethod(f="integrateIt",
          definition=function(x, y, ends, Rule, ...){
            if (Rule == "Trapezoid") {
              n <- length(x) - 1
              h <-(ends[2] - ends[1]) / n
              int_T <- h / 2 * (y[1] + sum(2*y[2:n]) + y[n+1])
              Integrated <- new("Trapezoid", x=x, y=y, ends=ends, Rule="Trapezoid", integrated_value=int_T)
              return(list(Integrated = Integrated, 
                          input = list(x=x,y=y), 
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
              
              Integrated <- new("Simpson", x=x, y=y, ends=ends, Rule="Simpson", integrated_value=int_S)
              return(list(Integrated = Integrated, 
                          input = list(x=x,y=y), 
                          output = int_S))
            }
            
            
          }
)