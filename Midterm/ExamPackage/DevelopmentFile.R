
## Load libraries and set working directory
library(devtools)
library(roxygen2)

#Package directory
setwd("/Users/rexdeng/Dropbox/mine/academics_career/WashU/Classes/202122Spring-Stat programming Jacob/PS/ASP2022_PS/Midterm/ExamPackage") 

## This can be run many times as the code is updates
current.code <- as.package("PoisMLE")
check(current.code)
load_all(current.code)
document(current.code)

## Install package
# install(current.code) ### only the package installed will cross references in the documentation work


## Let's look at a function
?`PoisMLE-class`
?logLik
?mle
?standardError
?estimatePois
### The plot function is built up in the class file


## Let's try it out
set.seed(123)
y <- sample(x=1:10, size=20, replace=TRUE)
logLik(y,2)
mle(y)
standardError(y,"bootstrap", B=1000)
standardError(y,"basic", B=1000)
estimatePois_obj_boot <- estimatePois(y, 2, "bootstrap")
estimatePois_obj_basic <- estimatePois(y, 2, "basic")
plot(estimatePois_obj_boot)
plot(estimatePois_obj_basic)

## Not working
y2 <- c(-1,2,3,4,5)
logLik(y2,2)
logLik(y,-1)
mle(y2)
standardError(y,"whatever")
estimatePois(y2)

