#####################################################################
# @author: Amitsingh Pardeshi                                       #
# mApply simulation and comparison with the mapply lib function		#
# 																    #
#####################################################################

# import libraries
library(microbenchmark)
library(ggplot2)

#####################################################################
# Example 1															#
#####################################################################

#calculate sum using mapply
mapply(sum,1:2,1:4)

# cusotm functions takes two vectors and simulate the mapply for sum function
mApplySum <- function(x,y){
	
   #calculates the length of input vectors
   lengthX <- length(x)
   lengthY <- length(y)
   result <- c()
   if(lengthX > lengthY){
		# checks if the length of vectors are multiples of each other else throw error and stop the execution
		if(lengthX %% lengthY != 0){
			stop("In mApplySum(x,y) :  longer argument not a multiple of length of shorter")
		}
   } else if(lengthX < lengthY) {
		# checks if the length of vectors are multiples of each other else throw error and stop the execution
		if(lengthY %% lengthX != 0){
			stop("In mApplySum(x,y) :  longer argument not a multiple of length of shorter")
		}
   }
   
   if(lengthX > lengthY){
		for( i in 1: lengthX){
			result[i] <- if(i<= lengthY) sum(x[i],y[i]) else sum(x[i],y[i/lengthY])
		}
   } else if(lengthX < lengthY){
		for( i in 1: lengthY){
			result[i] <- if(i<= lengthX) sum(y[i],x[i]) else sum(y[i],x[i/lengthX])
		}
   } else {
		for( i in 1: lengthX){
			result[i] <- sum(x[i],y[i])
		}
   }
   print(result)
}

#benchmark sum of two vectors using mapply and custom mApplySum functions
sumResult <- microbenchmark(mapply= mapply(sum, 1:4, 1:4),CustommApplyFun =  mApplySum(1:4, 1:4), times = 1000L)

##print(sumResult)

# plot the results in the graph
autoplot(sumResult)


#####################################################################
# Example 2															#
#####################################################################

# custom loop function 
mApplyLoop <- function(x){
	list = list()
	for(i in 1 : x){
	lv <- rep(i,i)
	list[[i]] <- lv
	}
	print(list)
}
loopVar <- mApplyLoop(4))

mApplyVar <- mapply(rep ,1:4, 1:4)

# benchamrk both the functions 
tm <- microbenchmark(mapply(rep, 1:4, 1:4), mApplyLoop(4),5)

print(tm)

#plot the results of the benchmark
autoplot(tm)
