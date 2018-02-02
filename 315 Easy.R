
# Challenge #315 Easy -----------------------------------------------------

xor_prod <- function(x, y) {
  
  ybin <- as.integer(intToBits(y))
  yones <- which(ybin == 1)
  xbin <- as.integer(intToBits(x))
  xones <- which(xbin == 1)
  
  if(length(yones) == 0 | length(xones) == 0)
    return(0)
  
  ylast <- yones[length(yones)]
  xlast <- xones[length(xones)]
  
  add.matrix <- matrix(data = 0, nrow = ylast, ncol = xlast + ylast - 1)
  xbin <- xbin[1:xlast]
  
  for(i in yones) {
    add.matrix[i, i:(i+xlast-1)] <- xbin 
  }
  
  bin <- as.integer(!((colSums(add.matrix) + 1) %% 2))
  manca <- 32 - length(bin)
  
  return(packBits(as.raw(c(bin, rep(0, manca))), type = "integer"))
  
}

input <- "1 2\n9 0\n6 1\n3 3\n2 5\n7 9\n13 11\n5 17\n14 13\n19 1\n63 63"
data.input <- read.table(textConnection(input))

results <- function(x) {
  return(paste0(x[1], "@", x[2], "=", xor_prod(x[1], x[2])))
}

output <- apply(data.input, 1, results)
cat(output, sep = "\n")
