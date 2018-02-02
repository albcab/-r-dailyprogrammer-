
# Challenge 314 Easy ------------------------------------------------------
## Bonus, no permutations

library(combinat)
library(purrr)

concat <- function(x) {
  perm <- permn(x)
  poss <- as.double(map_chr(perm, paste0, collapse = ""))
  return(c(min(poss), max(poss)))
}

input <- "79 82 34 83 69\n420 34 19 71 341\n17 32 91 7 46"

data.input <- read.table(textConnection(input))

output <- t(apply(data.input, 1, concat))
apply(output, 1, cat, fill = TRUE)
