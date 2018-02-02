
# Challenge 311 Easy ------------------------------------------------------

jolly <- function(x) {
  x <- x[!is.na(x)]
  top <- sort(abs(diff(x)))
  if(all(top == 1:(length(x)-1)))
    return("JOLLY")
  return("NOT JOLLY")
}

challenge <- "4 1 4 2 3\n5 1 4 2 -1 6\n4 19 22 24 21\n4 19 22 24 25\n4 2 -1 0 2"

input <- read.table(textConnection(challenge), fill = T, row.names = NULL, header = F)

output <- as.matrix(cbind(input[,1], r = t(t(apply(input[,-1], 1, jolly)))))
output[is.na(output)] <- ""

apply(output, 1, cat, "\n")
