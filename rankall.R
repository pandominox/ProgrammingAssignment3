# TODO: Add comment
# 
# Author: Anna Matysik
###############################################################################

rankall <- function(outcome, num = "best") {
	
	## Check that outcome is valid	
	outcome.column.no <- NULL
	if (outcome == "heart attack") {
		outcome.column.no <- 11
	} else if (outcome == "heart failure") {		
		outcome.column.no <- 17
	} else if (outcome == "pneumonia") {
		outcome.column.no <- 23
	} else {
		stop("invalid outcome")		
	}
	
	## Read outcome data
	outcome.care.measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
	outcome.care.measures[ ,outcome.column.no] <- as.numeric(outcome.care.measures[ ,outcome.column.no])
	outcome.care.measures <- outcome.care.measures[!is.na(outcome.care.measures[,outcome.column.no]), ]
	
	hospitals.by.state <- split(outcome.care.measures,outcome.care.measures$State,drop=TRUE)
	
	ranking <- data.frame(hospitals=NULL, state=NULL, value=NULL)
	
	for(i in seq(hospitals.by.state)) {	
		hospitals <- hospitals.by.state[[i]]
		state <- head(hospitals$State,1)
		hospitals <- hospitals[order(hospitals[, outcome.column.no], hospitals$Hospital.Name),]
		
		if (num=="best") {
			ranking <- rbind(ranking, data.frame(hospital=head(hospitals$Hospital.Name, 1), state=state))
		} else if (num=="worst") {
			ranking <- rbind(ranking, data.frame(hospital=tail(hospitals$Hospital.Name, 1), state=state))			
		} else {
			ranking <- rbind(ranking, data.frame(hospital=hospitals[num, "Hospital.Name"], state=state))
		}
	}
	
	ranking
}
