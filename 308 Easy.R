
# Challenge 308 Easy ------------------------------------------------------

letitburn <- function(x, input) {
  x <- strsplit(x, "\n")[[1]]
  x <- strsplit(x, "")
  floor <- matrix(unlist(x), nrow = length(x), ncol = length(x[[1]]), byrow = T)
  input <- read.table(textConnection(input))
  for(i in 1:nrow(input)) {
    s <- as.integer(input[i,]) + 1
    if(!all(rev(s) <= dim(floor)))
      next
    poss <- floor[s[2], s[1]]
    if(poss %in% c("F", "#", "|", "/", "=", "_"))
      next
    if(poss == "S")
      floor[s[2], s[1]] <- "F"
    else
      floor[s[2], s[1]] <- "S"
  }
  smoke <- which(floor == "S")
  for(i in 1:length(smoke)) {
    top <- floor[smoke[i] - 1]
    bot <- floor[smoke[i] + 1]
    lef <- floor[smoke[i] - nrow(floor)]
    rig <- floor[smoke[i] + nrow(floor)]
    all <- c(top, bot, lef, rig)
    if("F" %in% all){
      floor[smoke[i]] <- "F"
      next
    }
    pass <- which(all == "/" | all == "=" | all == "_")
    if(length(which) == 0) next
    for(j in seq_along(pass)) {
      check <- ifelse(pass[j] <= 2, 2, nrow(floor)*2)
      if(pass[j] %% 2 == 0)
        check <- floor[smoke[i] + check]
      else
        check <- floor[smoke[i] - check]
      if("F" == check) floor[smoke[i]] <- "F"
    }
  }
  return(floor)
}

shouse <- "#############/#\n#     |       #\n#     #       #\n#     #       #\n#######       #\n#     _       #\n###############"

shinput <- "1 1\n1 2\n1 3\n5 6\n4 2\n1 1\n1 2\n5 5\n5 5\n9 1\n5 7\n2 2"

output <- letitburn(shouse, shinput)

apply(output, 1, cat, "\n")
