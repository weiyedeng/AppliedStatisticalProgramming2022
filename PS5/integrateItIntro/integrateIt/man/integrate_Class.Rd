% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/integrate_Class.R
\docType{class}
\name{Trapezoid-class}
\alias{Trapezoid-class}
\alias{Trapezoid/Simpson-class}
\alias{initialize,}
\alias{Trapezoid/Simpson-method}
\alias{integrateIt/print}
\title{An integral approximation object}
\description{
Object of class \code{Trapezoid} or \code{Simpson} as created by the \code{integrateIt} functions
}
\details{
An object of the class `Trapezoid` or `Simpson` has the following slots:
\itemize{
\item \code{x} A vector of input values.
\item \code{fun} A function to be integrated.
\item \code{ends} A vector that contains the starting and ending values.
\item \code{Rule} Either "Trapezoid" or "Simpson".
\item \code{integrated_value} The output of the approximation method. 
}
}
\author{
Rex W. Deng: \email{weiye.deng@wustl.edu}
}
