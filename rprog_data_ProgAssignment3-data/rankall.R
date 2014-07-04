rankall <- function(outcome, num = "best"){

	# get data
	outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	# get the list of states
	states <- unique(outcomedata[,7])
	states <- states[order(states)]

	# tests if outcome is an acceptable choice
	if(tolower(outcome) == "heart attack"){
		i <- 11}
	else if(tolower(outcome) ==	"heart failure"){
		i <- 17}
	else if(tolower(outcome) == "pneumonia"){
		i <- 23}
	else{
		stop("invalid outcome")}

	# creates an empty list of names
	Names <- c()

	for(state in states){
		# filters the data for the state and not NA
		tmp <- subset(outcomedata, outcomedata[,7] == state & !is.na(suppressWarnings(as.numeric(outcomedata[,i]))))
	
		# transform best in 1 and worst in length(tmp)
		if(num == "best"){
			ntmp <- 1}
		else if (num == "worst"){
			ntmp <- nrow(tmp)}	
		else{
			ntmp <- num}
	
		if (ntmp > nrow(tmp)){
			Names <- c(Names, NA)}
		else{
			# compute the ordered list
			result <- tmp[order(suppressWarnings(as.numeric(tmp[,i])), tmp[,2]) , 2]
	
			# returns the result
			Names <- c(Names, result[ntmp])
		}
	}
	data.frame(hospital = Names, state = states)
}
