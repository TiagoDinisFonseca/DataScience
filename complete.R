complete <- function(directory, id = 1:332) {
	tmp <- rep(0, length(id))
	for(i in 1:length(id)){
 		name <- sprintf("%s/%03d.csv", directory, id[i])
		tmp.data <- read.csv(name)
		tmp.good <- !is.na(tmp.data[["nitrate"]]) & !is.na(tmp.data[["sulfate"]])
		tmp[i] <- sum(tmp.good)
	}
	data.frame(id = id, nobs = tmp)
}
