
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


## Let's look at a function
?logLik
?mle
?standardError


## Let's try it out
set.seed(123)
y <- sample(x=1:10, size=20, replace=TRUE)
logLik(y,2)
mle(y)
standardError(y,"bootstrap", B=1000)
standardError(y,"basic", B=1000)

## Not working
y2 <- c(-1,2,3,4,5)
logLik(y2,2)
mle(y2)

