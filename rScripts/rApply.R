#####################################################################
# @author: Amitsingh Pardeshi                                       #
# rapply simulation and comparison with the rapply lib function		#
# 																    #
#####################################################################

# import libraries
library(microbenchmark)
library(ggplot2)

# creating list containing three sublists
data1 <- list(10,20,30)
data2 <- list(100,200,300)
data3 <- list(1000,2000,3000)
data <- list(data1,data2,data3)

# custom function which simulates the rapply behaviour for doubling the each elements of the list and
# printing result vector
rApplyLoop <- function(x){
  result <- c()
  for(i in 1: length(x)){
    x1 <- x[[i]]
    for(j in 1:length(x1)){
      x2 <- x1[[j]]
      if(is.numeric(x2)){
        result <- c(result, x2*2)
      }
      
    }
  }
  print(result)
}
# rapply for doubling each elements of the input list recursively
rapply(data,function(x) x*2,class=c("numeric"))
# custome function to achieve the same
rApplyLoop(data)

#benchmark sum of two vectors using mapply and custom mApplySum functions
doubleResult <- microbenchmark(rapply = rapply(data,function(x) x*2,class=c("numeric")), customRapplyFun = rApplyLoop(data),times = 1000L)

##print(doubleResult)

# plot the results in the graph
autoplot(doubleResult)
