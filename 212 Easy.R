
# Challenge 212 Easy ------------------------------------------------------

library(stringr)

robber <- function(x, decode = F) {
  if(decode) {
    rob <- str_replace_all(x, ".o.", 
                           function(x) return(paste0(str_sub(x, 1, 1))))
    return(rob)
  }
  rob <- str_replace_all(x, "[^(aeiouy���AEIOUY���!?.,' )]", 
                         function(x) return(paste0(x, "o", tolower(x))))
  return(rob)
}

robber("Tre Kronor �r v�rldens b�sta ishockeylag.")
robber("V�r kung �r coolare �n er kung.")
