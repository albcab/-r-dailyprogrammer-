
# Challenge 317 Easy ------------------------------------------------------

library(stringr)

collatz <- function(x) {
  alph <- list(a = "bc", b = "a", c = "aaa")
  todos <- c()
  while(str_length(x) >= 2) {
    el <- which(str_sub(x, 1, 1) == names(alph))
    x <- str_c(str_sub(x, 3, -1), alph[[el]])
    todos <- c(todos, x)
  }
  return(cat(todos, sep = "\n"))
}

cyclic <- function(x) {
  alph <- list(a = "100", b = "010", c = "001")
  Q <- list("010001", "100", "100100100", "-", "-", "-")
  todos <- c()
  while(str_length(x) > 3) {
    need <- which(str_sub(x, 1, 3) == alph)
    for(i in 1:6) {
      x <- str_sub(x, 2, -1)
      if(i == need)
        x <- str_c(x, Q[[i]])
      todos <- c(todos, x)
    }
  }
  return(cat(todos, sep = "\n"))
}

collatz("aaa")
cyclic("100100100")
collatz("aaaaaaa")
