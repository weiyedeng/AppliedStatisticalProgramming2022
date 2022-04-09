#' Define PoisMLE (S4) Class
#' 
#' An Object of class \code{PoisMLE} can be created by the \code{estimatePois()} function
#'
#' 
#' An object of the class `PoisMLE` has the following slots:
#' \itemize{
#' \item \code{y} The original data.
#' \item \code{MLE} The maximum likelihood estimator for this dataset.
#' \item \code{LL} The log likelihood calculated from the observed data assuming the MLE is correct.
#' \item \code{SE} The standard error for the MLE.
#' \item \code{SEtype} The method used to calculate the standard error.
#' }
#' 
#' The \code{\link[PoisMLE]{estimatePois}} function calculates several statistics 
#' related to Poisson distribution based on a vector of observed data, including the log likelihood, 
#' MLE estimate, and its associated standard error. The output is an Object of class \code{PoisMLE}.
#' One can use the \code{plot()} function to produce a simple visualization of the MLE estimate with 95\% confidence interval. 
#'
#' @author Rex W. Deng: \email{weiye.deng@@wustl.edu}
#' @seealso \code{\link[PoisMLE]{logLik}}, \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standardError}}, \code{\link[PoisMLE]{estimatePois}}
#' @rdname PoisMLE_Class
#' @include PoisMLE_Class.R
#' @import methods
#' @importFrom graphics arrows
#' @export
setClass(Class="PoisMLE",
         representation = representation(
           y = "numeric",
           MLE = "numeric",
           LL = "numeric",
           SE = "numeric",
           SEtype = "character"
         ),
         prototype = prototype(
           y = numeric(),
           MLE = numeric(),
           LL = numeric(),
           SE = numeric(),
           SEtype = character()
         )
)

### Validate whether the slots match the following requirements
setValidity("PoisMLE", function(object){
  ## test for x
  y_classtest <- is.numeric(object@y)
  if (!y_classtest) {stop("y must be numeric vector.")}
  y_zerotest <- any(object@y > 0)
  if (!y_zerotest) {stop("Every value of y must be larger than or equal to 0.")}
  
  ## MLE
  MLE_classtest <- is.numeric(object@MLE)
  if (!MLE_classtest) {stop("MLE must be numeric value.")}
  MLE_lentest <- length(object@MLE) >= 1
  if (!MLE_lentest) {stop("The length of MLE must be 1.")}
  
  
  ## LL
  LL_classtest <- is.numeric(object@LL)
  if (!LL_classtest) {stop("LL must be numeric value.")}
  LL_lentest <- length(object@LL) >= 1
  if (!LL_lentest) {stop("The length of LL must be 1.")}
  
  
  ## SE
  SE_classtest <- is.numeric(object@SE)
  if (!SE_classtest) {stop("SE must be numeric value.")}
  SE_lentest <- length(object@SE) >= 1
  if (!SE_lentest) {stop("The length of SE must be 1.")}
  
  ## SEtype
  SEtype_classtest <- is.character(object@SEtype)
  if (!SEtype_classtest) {stop("SEtype must be a character string.")}
}
)

setMethod("initialize", "PoisMLE", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 


setMethod("plot", signature=c(x="PoisMLE", y="missing"), 
          function(x, y, ...){
            MLE <- x@MLE
            UPB <- x@MLE + 1.96*x@SE
            LWB <- x@MLE - 1.96*x@SE
            plot(MLE, xlab = "", ylab = "MLE Estimate", main = "MLE estimate with 95% CI", cex=2, xaxt='n')
            arrows(x0=1, y0=LWB, x1=1, y1=UPB, code=3, angle=90, length=0.1, col="blue", lwd=2)
          }
) 


