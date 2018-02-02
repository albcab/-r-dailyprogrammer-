
# Challenge 212 Easy ------------------------------------------------------

library(stringr)

robber <- function(x, decode = F) {
  if(decode) {
    rob <- str_replace_all(x, ".o.", 
                           function(x) return(paste0(str_sub(x, 1, 1))))
    return(rob)
  }
  rob <- str_replace_all(x, "[^(aeiouyåäöAEIOUYÅÄÖ!?.,' )]", 
                         function(x) return(paste0(x, "o", tolower(x))))
  return(rob)
}

robber("Tre Kronor är världens bästa ishockeylag.")
robber("Vår kung är coolare än er kung.")
