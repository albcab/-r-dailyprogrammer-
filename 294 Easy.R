
# Challenge 294 Easy ------------------------------------------------------

library(stringr)

scrabble <- function(tiles, word) {
  alltrues <- T
  for(i in 1:str_length(word)) {
    check <- str_sub(word, i, i)
    alltrues <- str_detect(tiles, check)
    if(!alltrues) {
      alltrues <- str_detect(tiles, "\\?")
      if(alltrues)
        check <- "\\?"
    }
    if(alltrues)
      tiles <- str_replace(tiles, check, "")
    else
      return(FALSE)
  }
  return(TRUE)
}

library(RCurl)
wordlist <- getURL("https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/dotnetperls-controls/enable1.txt")
wordlist <- str_split(wordlist, "\r\n")[[1]]

longest <- function(x) {
  len <- sort(str_length(wordlist), decreasing = T)
  wordlist <- wordlist[order(str_length(wordlist), decreasing = T)]
  wordlist <- wordlist[len <= str_length(x)]
  fal <- FALSE
  count <- 0
  while(!fal) {
    count <- count + 1
    fal <- scrabble(x, wordlist[count])
  }
  return(wordlist[count])
}
