setClass(Class="Trapezoid",
         representation = representation(
           x = "numeric",
           y = "numeric",
           ends = "numeric",
           Rule = "character",
           integrated_value = "numeric"
         ),
         prototype = prototype(
           x = numeric(),
           y = numeric(),
           ends = numeric(),
           Rule = character(),
           integrated_value = numeric()
         )
)

setClass(Class="Simpson",
         representation = representation(
           x = "numeric",
           y = "numeric",
           ends = "numeric",
           Rule = "character",
           integrated_value = "numeric"
         ),
         prototype = prototype(
           x = numeric(),
           y = numeric(),
           ends =numeric(),
           Rule = character(),
           integrated_value = numeric()
         )
)

setValidity("Trapezoid", function(object){
  ## test for x
  x_classtest <- is.numeric(object@x)
  if (!x_classtest) {stop("x must be numeric vector.")}
  x_lentest <- length(x) >= 3
  if (!x_classtest) {stop("The length of x cannot be smaller than 3.")}
  
  
  ## test for y
  y_classtest <- is.numeric(object@x)
  if (!y_classtest) {stop("y must be numeric vector.")}
  y_lentest <- length(y) >= 3
  if (!x_classtest) {stop("The length of y cannot be smaller than 3.")}
  
  
  ## test for x and y
  xy_eqlentest <- length(object@x) == length(object@y)
  if (!xy_eqlentest) {stop("The lengths of x and y must be equal.")}
  
  
  ## test for ends
  ends_classtest <- is.numeric(object@ends)
  if (!ends_classtest) {stop("ends must be numeric vector.")}
  ends_lentest <- length(object@ends) == 2
  if (!ends_lentest) {stop("ends have to only contain the starting value and ending value.")}
  ends_comtest <- object@ends[2] > object@ends[1]
  if (!ends_comtest) {stop("The ending value is not larger than the starting value.")}
  
  
  ## test for x and ends; force x_0 = 0 and x_n = b
  xends_comtest1 <- ends[1] == min(object@x)
  if (!xends_comtest1) {stop("The min value in 0 must equal to the starting value in @ends.")}
  xends_comtest2 <- ends[2] == max(object@x)
  if (!xends_comtest1) {stop("The max value in 0 must equal to the ending value in @ends.")}
  
  ## test for Rule
  Rule_test <- object@Rule %in% c("Trapezoid", "Simpson")
  if (!Rule_test) {stop("Invalid Rule. Either Trapezoid or Simpson is allowed.")}
}
)


setValidity("Simpson", function(object){
  ## test for x
  x_classtest <- is.numeric(object@x)
  if (!x_classtest) {stop("x must be numeric vector.")}
  x_lentest <- length(x) >= 3
  if (!x_classtest) {stop("The length of x cannot be smaller than 3.")}
  
  
  ## test for y
  y_classtest <- is.numeric(object@x)
  if (!y_classtest) {stop("y must be numeric vector.")}
  y_lentest <- length(y) >= 3
  if (!x_classtest) {stop("The length of y cannot be smaller than 3.")}
  
  
  ## test for x and y
  xy_eqlentest <- length(object@x) == length(object@y)
  if (!xy_eqlentest) {stop("The lengths of x and y must be equal.")}
  
  
  ## test for ends
  ends_classtest <- is.numeric(object@ends)
  if (!ends_classtest) {stop("ends must be numeric vector.")}
  ends_lentest <- length(object@ends) == 2
  if (!ends_lentest) {stop("ends have to only contain the starting value and ending value.")}
  ends_comtest <- object@ends[2] > object@ends[1]
  if (!ends_comtest) {stop("The ending value is not larger than the starting value.")}
  
  
  ## test for x and ends; force x_0 = 0 and x_n = b
  xends_comtest1 <- ends[1] == min(object@x)
  if (!xends_comtest1) {stop("The min value in x must equal to the starting value in @ends.")}
  xends_comtest2 <- ends[2] == max(object@x)
  if (!xends_comtest1) {stop("The max value in x must equal to the ending value in @ends.")}
  
  ## test for Rule
  Rule_test <- object@Rule %in% c("Trapezoid", "Simpson")
  if (!Rule_test) {stop("Invalid Rule. Either Trapezoid or Simpson is allowed.")}
}
)



setMethod("initialize", "Trapezoid", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 


setMethod("initialize", "Simpson", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

