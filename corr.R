corr <- function(directory, threshold = 0){
	correlation <- NULL
	for(i in 1:332){
 		name <- sprintf("%s/%03d.csv", directory, i)
		tmp.data <- read.csv(name)
		tmp.good <- !is.na(tmp.data[["nitrate"]]) & !is.na(tmp.data[["sulfate"]])
		if(sum(tmp.good) > threshold){
			nitrate.good <- tmp.data[tmp.good,"nitrate"]
			sulfate.good <- tmp.data[tmp.good,"sulfate"]
			correlation <- c(correlation, cor(nitrate.good, sulfate.good))
		}
	}
 	correlation
}
