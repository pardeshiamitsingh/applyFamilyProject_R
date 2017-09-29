#####################################################################
# @author: Amitsingh Pardeshi                                       #
# tapply simulation and comparison with the tapply lib function		#
# 																    #
#####################################################################

# import libraries
library(microbenchmark)
library(ggplot2)

# creating dataframe of baseball teams
baseball.example <-
  data.frame(team = gl(5, 5,labels = paste("Team", LETTERS[1:5])),
             player = sample(letters, 25),
             batting.average = runif(25, .200, .400))

# custom function to simulate tapply
tApplyLoop <- function(x){
  if(!is.data.frame(x)){
    stop("x is expected to be datframe")
  }
  out <- split( x , f = baseball.example$team)
  for(i in 1:length(out)){
    out2 <- out[[i]]
    print(max(out2[[3]]))
  }
}


# tapply for finding max batting avg group by team
tapply(baseball.example$batting.average, baseball.example$team,max)

# custome function to achieve the same
tApplyLoop(baseball.example)

#benchmark sum of two vectors using mapply and custom mApplySum functions
tApplyResult <- microbenchmark(tapply=tapply(baseball.example$batting.average, baseball.example$team,max),customtapplyFun = tApplyLoop(baseball.example),times = 1000L)
                               
print(tApplyResult)
                               
# plot the results in the graph
autoplot(tApplyResult)