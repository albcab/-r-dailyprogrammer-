
# Challenge 317 Intermediate ----------------------------------------------

library(stringr)
library(dplyr)
library(purrr)

count <- function(x) {
  little <- str_extract_all(x, "[A-Z][a-z]")[[1]]
  ones <- str_replace_all(x, "[A-Z][a-z]?[0-9]+", "")
  if(ones != "") {
    notones <- str_extract_all(x, "[A-Z][a-z]?[0-9]+")[[1]]
    for(i in seq_along(notones))
      x <- str_replace(x, notones[i], tolower(notones[i]))
    ones <- str_extract_all(ones, "[A-Z][a-z]?")[[1]]
    one <- map_chr(ones, function(x) return(str_c(x, "1")))
    for(i in seq_along(ones))
      x <- str_replace(x, ones[i], tolower(one[i]))
    x <- toupper(x)
    for(i in seq_along(little))
      x <- str_replace(x, toupper(little[i]), little[i])
  }
  repeat {
    paren <- str_extract(x, "[\\(][A-Za-z0-9]+[\\)][0-9]+")
    if(is.na(paren))
      break
    noparen <- str_replace_all(paren, "\\(|\\)", "")
    noparen <- str_sub(noparen, 1, -2)
    num <- as.numeric(str_extract_all(paren, "[0-9]+")[[1]])
    num2 <- num[-length(num)] * num[length(num)]
    num <- as.character(num); num2 <- as.character(num2)
    for(i in seq_along(num2))
      noparen <- str_replace(noparen, num[i], num2[i])
    paren <- str_replace_all(paren, c("\\(" = "\\\\(", "\\)" = "\\\\)"))
    x <- str_replace(x, paren, noparen)
  }
  x <- str_extract_all(x, "([A-Z][a-z]?)|([0-9]+)")[[1]]
  x <- data.frame(matrix(x, ncol = 2, byrow = T), stringsAsFactors = F)
  x[,2] <- as.numeric(x[,2])
  x %<>% group_by(X1) %>% summarise(sum(X2))
  return(invisible(apply(x, 1, cat, sep = c(":\t", ""), "\n")))
}

count("C6H12O6"); cat("\n")
count("CCl2F2"); cat("\n")
count("NaHCO3"); cat("\n")
count("C4H8(OH)2"); cat("\n")
count("PbCl(NH3)2(COOH)2"); cat("\n")
