
# Challenge 313 Intermediate ----------------------------------------------

library(stringr)
library(RCurl)
library(readr)

img_mani <- function(img, op) {
  all <- str_split(img, "\n")[[1]]
  data <- as.numeric(all[c(-1)])
  data <- data[!is.na(data)]
  dim <- str_split(all[1], " ")[[1]]
  img <- matrix(data, nrow = as.numeric(dim[3]), ncol = as.numeric(dim[2]), byrow = T)
  op <- str_split(op, "")[[1]]
  h <- sum(op == "H"); v <- sum(op == "V")
  r <- sum(op == "R"); l <- sum(op == "L")
  ops <- vector()
  if(h%%2 == 1)
    ops <- c(ops, "H")
  if(v%%2 == 1)
    ops <- c(ops, "V")
  if(r != l) {
    if(r > l)
      ops <- c(ops, rep("R", (r-l)%%4))
    else
      ops <- c(ops, rep("L", (l-r)%%4))
  }
  for(i in seq_along(ops)) {
    if(any(ops[i] == c("R", "L")))
      dim[2:3] <- dim[3:2]
    rc <- ifelse(any(ops[i] == c("R", "V")), 2, 1)
    img <- apply(img, rc, rev)
    if(any(ops[i] == c("R", "H")))
      img <- t(img)
  }
  dim <- str_c(dim, collapse = " ")
  op <- str_c(op, collapse = "")
  write(c(dim, as.vector(t(img))), sprintf("earth-%s.pgm", op))
}

input <- getURL("https://raw.githubusercontent.com/cosmologicon/problems/master/pgm/earth.pgm")
