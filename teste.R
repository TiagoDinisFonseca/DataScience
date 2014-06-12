add2 <- function(x, y){
	x + y
}

above10 <- function(x){
	tmp <- x > 10
	x[tmp]
}

above <- function(x, n=10){
	tmp <- x > n
	x[tmp]
}

columnmean <- function(y, remove.na = TRUE) {
	nc <- ncol(y)
	means <- numeric(nc)
	for(i in 1:nc){
		means[i] <- mean(y[,i], na.rm = remove.na)
	}
	means
}

make.power <- function(n) {
	pow <- function(x) {
		x^n
	}
}
