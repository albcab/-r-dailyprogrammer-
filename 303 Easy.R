
# Challenge 303 Easy ------------------------------------------------------

ricochet <- function(x) {
  h <- x[1]; w <- x[2]; v <- x[3]
  count <- 0; rep <- 0
  dif <- h-w
  leng <- vector()
  while(dif) {
    if(dif < 0) {
      count <- count - 1
      rep <- rep + 1
      leng <- leng[-(count+1)]
      dif <- abs(dif)
      next
    }
    if(count%%2 == 0) {
      leng <- c(leng, h - dif)
      dif <- w - dif
    }
    else {
      leng <- c(leng, w - dif)
      dif <- h - dif
    }
    count <- count + 1
  }
  count <- count+rep
  leng <- c(leng, min(h, w))
  return(list(C, count, leng, sum(leng)/v))
}

# Soy un puto imbecil

gcd <- function(x,y) {
  r <- x%%y
  return(ifelse(r, gcd(y,r), y))
}

lcm <- function(x,y) return((x*y)/gcd(x,y))

ricochet <- function(h, w, v) {
  dist <- lcm(h,w)
  hh <- dist/h
  ww <- dist/w
  hhh <- ifelse(hh %% 2 == 0, "U", "L")
  www <- ifelse(ww %% 2 == 0, "L", "R")
  return(c(paste0(hhh, www), hh+ww-2, dist/v))
}
