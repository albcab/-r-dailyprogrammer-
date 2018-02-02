
# Challenge 296 Easy ------------------------------------------------------

library(stringr)
library(RCurl)

twelve <- function(gifts = NULL) {
  url <- "http://www.41051.com/xmaslyrics/twelvedays.html"
  x <- str_split(getURL(url), "[<>]")[[1]][215:439]
  x <- x[seq(1, length(x), 2)]
  x <- str_replace_all(x, "[^(a-zA-Z0-9 :'!?)]", "")
  gifts <- str_split(gifts, "\n")[[1]]
  if(length(gifts) == 12)
    x <- str_replace_all(x, c("Drummers Drumming" = gifts[12],
                              "Pipers Piping" = gifts[11],
                              "Lords a Leaping" = gifts[10],
                              "Partridge in a Pear Tree" = gifts[1],
                              "Turtle Doves" = gifts[2],
                              "French Hens" = gifts[3],
                              "Calling Birds" = gifts[4],
                              "Golden Rings" = gifts[5],
                              "Geese a Laying" = gifts[6],
                              "Swans a Swimming" = gifts[7],
                              "Maids a Milking" = gifts[8],
                              "Ladies Dancing" = gifts[9]))
  x <- str_replace_all(x, c("12" = "twelve",
                            "11" = "eleven",
                            "10" = "ten",
                            "1" = "a",
                            "2" = "two",
                            "3" = "three",
                            "4" = "four",
                            "5" = "five",
                            "6" = "six",
                            "7" = "seven",
                            "8" = "eight",
                            "9" = "nine"))
  cat(x, sep = "\n")
}

gifts <- "Partridge in a Fuck\nTurtle Fuck\nFrench Fuck\nCalling Fuck\nGolden Fuck\nGeese a Fucking\nSwans a Fucking\nMaids a Fucking\nLadies Fucking\nLords a Fucking\nPipers Fucking\nDrummers Fucking"

twelve(gifts)
