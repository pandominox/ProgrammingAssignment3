# TODO: Add comment
# 
# Author: Anna Matysik
###############################################################################

source("rankhospital.R")



rankall <- function(outcome, num = "best") {
	states <- data$getstates()
	str(states)
	ranking <- data.frame(hospitals=NULL, state=NULL, value=NULL)
	
	for(i in seq(states)) {	
		state <- states[i]
		ranking <- rbind(ranking, data.frame(hospital=rankhospital(state, outcome, num), state=state))		
	}	
	ranking
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
