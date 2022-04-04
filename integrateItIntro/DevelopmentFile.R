
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("/Users/rexdeng/Dropbox/mine/academics_career/WashU/Classes/202122Spring-Stat programming Jacob/PS/ASP2022_PS/integrateItIntro") #This will need to be changed to match your directory

## This is run once when the package structure is first created

## This can be run many times as the code is updates
current.code <- as.package("integrateIt")
check(current.code)
load_all(current.code)
document(current.code)


## Let's look at a function
?integrateIt
?tolTest



## Let's try it out
fx <- function(x) {
  return(x^3 + x^2 + 1)
}
x <- seq(0,3,0.1)
ends <- c(0,3)


Tr <- integrateIt(x=x,fun=fx,ends=ends,Rule="Trapezoid")
Si <- integrateIt(x=x,fun=fx,ends=ends,Rule="Simpson")
integrate(fx,0,3) ## Compared with the result calculated by integrate()

Tr
Si

show(Tr[[1]])
show(Si[[1]])

