getDeerCarcassData <- function(){
data <- c(
  11,   8,   7,   4,   2,   3,   1,   2,   6,   2,   
  2,   5,   1,   2,   4,   0,   1,   2,   1,   0,   
  0,   0,   0,   0,   0,   0,   0,   1,   0,   1,   
  0,   0,   2,   1,   0,   0,   0,   0,   0,   0,   
  0,   1,   0,   0,   0,   0,   0,   0,   0,   1,   
  0,   0,   0,   0,   0,   0,   0,   0,   0,   1,   
  0,   0,   0,   0,   0,   0,   0,   1,   0,   0,   
  0,   0,   0,   0,   0,   1,   0,   0,   0,   0,   
  0,   0,   0,   0,   0,   0,   0,   1 )

z <- c()
for (j in 1:length(data)) {
  x <- data[j]
  if (x > 0) {
    z <- c(z, rep(j, x))
  }
}
counts <- z
return( list(data = data, counts = counts))
}