pollutantmean <- function(directory, pollutant, id = 1:332){
	sum <- 0
	count <- 0
	for(i in id){
 		name <- sprintf("%s/%03d.csv", directory, i)
		tmp.data <- read.csv(name)
		tmp.pollutant <- tmp.data[[pollutant]]
		tmp.good <- !is.na(tmp.pollutant)
		count <- count + sum(tmp.good)
		sum <- sum + sum(tmp.pollutant[tmp.good])
	}
	sum/count
}		

