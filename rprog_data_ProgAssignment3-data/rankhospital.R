rankhospital <- function(state, outcome, num = "best"){

	# get data
	outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	# get the list of states and test if state is in
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

	# filters the data for the state and not NA
	tmp <- subset(outcomedata, outcomedata[,7] == state & !is.na(suppressWarnings(as.numeric(outcomedata[,i]))))

	# transform best in 1 and worst in length(tmp)
	if(num == "best"){
		num <- 1}
	else if (num == "worst"){
		num <- nrow(tmp)}
	else if (num > nrow(tmp)){
		return(NA)}
	
	# compute the ordered list
	result <- tmp[order(suppressWarnings(as.numeric(tmp[,i])), tmp[,2]) , 2]
	
	# returns the result
	result[num]
}
