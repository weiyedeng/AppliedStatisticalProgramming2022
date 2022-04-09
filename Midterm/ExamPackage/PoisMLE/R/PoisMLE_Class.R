#' Define PoisMLE (S4) Class
#' 
#' Object of class \code{PoisMLE} created by the \code{estimatePois} functions
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
#' @author Rex W. Deng: \email{weiye.deng@@wustl.edu}
#' @rdname PoisMLE_Class
#' @include PoisMLE_Class.R
#' @import methods
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

