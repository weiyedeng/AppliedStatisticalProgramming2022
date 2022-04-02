setGeneric(name="integrateIt",
           def=function(x, y, ends, Rule, ...)
           {standardGeneric("integrateIt")}
)

setMethod(f="integrateIt",
          definition=function(x, y, ends, Rule, ...){
            if (Rule == "Trapezoid") {
              x <- sort(x)
              y <- sort(y)
              n <- length(x) - 1
              h <-(ends[2] - ends[1]) / n
              int_T <- h / 2 * (y[1] + sum(2*y[2:n]) + y[n+1])
              Integrated <- new("Trapezoid", x=x, y=y, ends=ends, Rule=Rule,integrated_value=int_T)
              return(list(Integrated = Integrated, 
                          input = c(x=x,y=y), 
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
              
              Integrated <- new("Trapezoid", x=x, y=y, ends=ends, Rule=Rule,integrated_value=int_S)
              return(list(Integrated = Integrated, 
                          input = list(x=x,y=y), 
                          output = int_S))
            }
            
            
          }
)
