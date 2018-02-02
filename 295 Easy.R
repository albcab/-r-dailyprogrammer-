
# Challenge 295 Easy ------------------------------------------------------

library(stringr)

lbyl <- function(x) {
  x <- str_split(x, "\n")[[1]]
  stopifnot(str_length(x[1]) == str_length(x[2]))
  out <- c(x[1])
  last <- ""
  for(i in 1:str_length(x[1])) {
    this <- str_c(str_sub(x[2], 1, i), str_sub(x[1], i+1, -1))
    if(last != this)
      out <- c(out, this)
    last <- this
  }
  return(cat(out, sep = "\n"))
}

lbyl("floor\nbrake")
lbyl("wood\nbook")
lbyl("a fall to the floor\nbraking the door in")
