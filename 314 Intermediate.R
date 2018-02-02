
# Challenge 314 Intermediate ----------------------------------------------

library(stringr)

lexi <- function(x) {
  opt <- vector(length = str_length(x))
  for(i in 1:str_length(x))
    opt[i] <- str_sub(x, i)
  sub <- order(opt)[1] - 1
  new <- str_c(opt[sub+1], str_sub(x, 1, sub), collapse = "")
  return(cat(sub, new, "\n"))
}

input <- "aabbccddbbaabb\nonion\nbbaaccaadd\nalfalfa\nweugweougewoiheew\npneumonoultramicroscopicsilicovolcanoconiosis"
input <- str_split(input, "\n")[[1]]
invisible(sapply(input, lexi))
