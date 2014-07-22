# TODO: Add comment
# 
# Author: Anna Matysik
###############################################################################

rankhospital <- function(state, outcome, num = "best") {
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
	
	## Check that state is valid	
	state <- toupper(state)
	if (state %in% outcome.care.measures[ ,"State"]) {
		
		## Select only hospitals from selected state
		hospitals <- outcome.care.measures[outcome.care.measures$State==state, ]
		
		## Sort by lowest outcome and hospital name
		hospitals <- hospitals[order(hospitals[, outcome.column.no], hospitals$Hospital.Name),]
		
		## Return hospital name in that state with lowest 30-day death rate
		
		if (num=="best") {
			head(hospitals$Hospital.Name, 1)
		} else if (num=="worst") {
			tail(hospitals$Hospital.Name, 1)			
		} else {
			hospitals[num, "Hospital.Name"]
		}
	} else {
		stop("invalid state")
	}
}

#rankhospital("TX", "heart failure", 4)
#rankhospital("MD", "heart attack", "worst")
#rankhospital("MN", "heart attack", 5000)
