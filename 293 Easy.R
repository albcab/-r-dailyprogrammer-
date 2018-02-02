
# Challenge 293 Easy ------------------------------------------------------

bomb <- function(x) {
  x <- substr(strsplit(x, "\n")[[1]], 1, 1)
  for(i in seq_along(x[-length(x)])) {
    if(x[i] == "w" & x[i+1] %in% c("w", "b"))
      return("Boom")
    if(x[i] == "r" & x[i+1] != "g")
      return("Boom")
    if(x[i] == "b" & x[i+1] %in% c("w", "g", "o"))
      return("Boom")
    if(x[i] == "o" & !x[i+1] %in% c("r", "b"))
      return("Boom")
    if(x[i] == "g" & !x[i+1] %in% c("o", "w"))
      return("Boom")
    if(x[i] == "p" & x[i+1] %in% c("p", "g", "o", "w"))
      return("Boom")
  }
  return("Bomb defused")
}

input <- list(one = "white\nred\ngreen\nwhite",
              two = "white\norange\ngreen\nwhite")

for(i in seq_along(input))
  print(bomb(input[[i]]))
