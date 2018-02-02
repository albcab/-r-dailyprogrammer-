
# Challenge 298 Easy ------------------------------------------------------

library(stringr)

toomp <- function(x) {
  x <- str_replace_all(x, "\\(\\)", "")
  if(str_length(x) == 0)
    return(NULL)
  last <- ""
  new <- str_c(str_extract_all(x, "[a-z]")[[1]], collapse = "")
  stopit <- str_length(new)
  repeat {
    full <- str_extract(x, "[\\(][a-z]+[\\)]")
    nopar <- str_extract(full, "[a-z]+")
    fullregex <- str_replace_all(full, c("\\(" = "\\\\(",
                                         "\\)" = "\\\\)"))
    x <- str_replace(x, fullregex, nopar)
    if(last != full) {
      new <- str_replace_all(new, str_sub(nopar, 1, 1), str_sub(full, 1, 2))
      new <- str_replace_all(new, str_sub(nopar, -1, -1), str_sub(full, -2, -1))
    }
    last <- full
    if(stopit == str_length(x))
      break
  }
  return(cat(new, "\n"))
}

toomp("((a((bc)(de)))f)")
toomp("(((zbcd)(((e)fg))))")
toomp("ab((c))")
toomp("()")
toomp("((fgh()()()))")
toomp("()(abc())")
