#####################################################################
# @author: Amitsingh Pardeshi                                       #
# vapply simulation and comparison with the vapply lib function		#
# 																    #
#####################################################################

# import libraries
library(microbenchmark)
library(ggplot2)

# creating list containing three sublists
data <- list(10,20,30)

vApplyLoop <- function(x){
  v <- double(0)
  for(i in 1: length(x)){
    v <- c(v, x[[i]]*2)
  }
  print(v)
}


# rapply for doubling each elements of the input list and expects double of length 1 as output
vapply(data,function(x) x*2,FUN.VALUE = double(1))

# custome function to achieve the same
vApplyLoop(data)

#benchmark sum of two vectors using mapply and custom mApplySum functions
vApplyResult <- microbenchmark(vapply=vapply(data,function(x) x*2,FUN.VALUE = double(1)),customVapplyFun = vApplyLoop(data),5)


print(vApplyResult)

# plot the results in the graph
autoplot(vApplyResult)
