best <- function(state, outcome){

	# get data
	outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	# get list of states and test if state is in
	states <- unique(outcomedata[,7])
	if(!(toupper(state) %in% states))
		stop("invalid state")

	# tests if outcome is an acceptable choice
	if(tolower(outcome) == "heart attack"){
		i <- 11}
	else if(tolower(outcome) ==	"heart failure"){
		i <- 17}
	else if(tolower(outcome) == "pneumonia"){
		i <- 23}
	else{
		stop("invalid outcome")}

	# creates the subset for the state
	tmp <- subset(outcomedata, outcomedata[,7] == state)

	# creates a list of names
	names <- tmp[,2]

	# creates the data
	data <- suppressWarnings(as.numeric(tmp[,i]))

	# computes the min
	names[which.min(data)]
}
