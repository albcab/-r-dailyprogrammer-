
# Challenge 316 Intermediate ----------------------------------------------

sydney <- function(x) {
  x <- strsplit(x, " ")[[1]]
  OH <- sum(x == "OH")
  BC <- sum(x == "BC")
  SK <- sum(x == "SK")
  opera <- 300 * OH - OH%/%3 * 300
  bridge <- ifelse(BC > 4, BC*90, BC*110)
  sky <- ifelse(SK >= OH, (SK-OH)*30, 0)
  total <- sum(opera, bridge, sky)
  return(cat(x, "=", total, collapse = " ", "\n"))
}

input <- "OH OH OH BC\nOH SK\nBC BC BC BC BC OH\nOH OH OH BC SK\nOH BC BC SK SK\nBC BC BC BC BC BC OH OH\nSK SK BC"
input <- strsplit(input, "\n")[[1]]
invisible(sapply(input, sydney))
