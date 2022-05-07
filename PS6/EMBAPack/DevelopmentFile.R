
## Load libraries and set working directory
library(devtools)
library(roxygen2)
library(usethis)
setwd("/Users/rexdeng/Dropbox/mine/academics_career/WashU/Classes/202122Spring-Stat programming Jacob/PS/ASP2022_PS/PS6/EMBAPack") #This will need to be changed to match your directory

## This is run once when the package structure is first created
# Rcpp.package.skeleton("EMBARcpp")



## This can be run many times as the code is updates
current.code <- as.package("EMBARcpp")
document(current.code)
check(current.code)
load_all(current.code)
test(current.code)



## Let's look at functions
?get_num_den
?est_ztk
?update_weights
?est_finalWeights

## Let's try it out
### Define parameters
y <- c(0.1,0.2,0.5)
ftk <- seq(0,1,0.1)
sd <- 1
weights <- rep(1/length(ftk), length(ftk)) ## Intially assign equal weight to each model
threshold <- 0.0001

get_num_den(y,ftk,sd,weights)[[1]]
est_ztk(y,ftk,sd,weights)
update_weights(est_ztk(y,ftk,sd,weights))
est_finalWeights(y,ftk,sd,weights,threshold)
