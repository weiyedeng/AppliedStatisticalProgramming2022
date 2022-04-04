#' Test the Preciseness Required for an Approximation Method
#'
#' tolTest takes in a function and increase the number of intervals n until the answer it provides
#' using the specified approximation (i.e., either Trapezoid or Simpson) is within tolerance of the correct answer.
#'
#' @param fun A function to be integrated.
#' @param ends A numeric vector of length 2, indicating the starting and ending value for integration. 
#' @param tolerance A positive number that indicates to what extent the difference between the correct answer and approximation is allowed. The default is 0.001.
#' @param Rule A character string. Either `Trapezoid` or `Simpson` is allowed.
#' @param start The number of intervals (with equal length) it should start with. The default is 2.
#' @param correct The correct answer for the integral. The default is calculated by applying `fun` and `ends` to `integrate()`.
#'
#' @return A list with the elements
#'  \item{input}{All the specified inputs.}
#'  \item{n}{The number of intervals required to reach the precision defined by `tolerance`.} 
#'  \item{abs_error}{The absolute error of the estimate.}
#' @author Rex W. Deng <\email{weiye.deng@@wustl.edu}>
#' @note Test the Preciseness Required for an Approximation Method
#' @examples
#' fx <- function(x) {
#' return(x^3 + x^2 + 1)
#' }
#' ends <- c(0,3)
#' test <- tolTest(fun=fx, ends=ends, Rule="Trapezoid")
#' test 
#' @seealso integrateIt
#' @rdname tolTest
#' @include tolTest.R
#' @export
setGeneric(name="tolTest",
           def=function(fun, ends, tolerance=0.001, Rule=c("Trapezoid","Simpson"), start=2, correct=integrate(fun, ends[1], ends[2]))
           {standardGeneric("tolTest")}
)

#' @export
setMethod(f="tolTest",
          definition=function(fun, ends, tolerance=0.001, 
                              Rule=c("Trapezoid","Simpson"), start=2, 
                              correct=integrate(fun, ends[1], ends[2])$value) {
            ## set original values
            n <- start
            x <- seq(ends[1], ends[2], length.out = n + 1) ### x is defined by the number of intervals with equal length
            approx_output <- integrateIt(x=x, fun=fun, ends=ends, Rule=Rule)$output ### use the integrateIt function to get approximation results
            abs_error <- abs(approx_output - correct) ### Calculate absolute errors
            
            ## increase the number of intervals and redo the appximation until abs_error < tolerance
            while (abs_error >= tolerance) {
              n = n + 1 
              x <- seq(ends[1], ends[2], length.out = n + 1)
              approx_output <- integrateIt(x=x, fun=fun, ends=ends, Rule=Rule)$output
              abs_error <- abs(approx_output - correct)
            }
            
            return(list(input = list(fun=fun, ends=ends, tolerance = tolerance, Rule=Rule, start=start, correct=correct), 
                        n = n, 
                        abs_error = abs_error))    
            }
)