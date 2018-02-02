
# Challenge 306 Easy ------------------------------------------------------

cat("Exactly once:")
for(i in c(400,600))
  for(j in c(40,60))
    for(k in c(4,6))
      cat(1000+i+j+k, " ")
cat("\n")
cat("At least once:")
for(i in c(400,600,700,800))
  for(j in c(40,60,70,80))
    for(k in c(4,6,7,8))
      cat(1000+i+j+k, " ")

invisible(apply(expand.grid(c(1), c(4,6), c(4,6), c(4,6)), 1, cat, sep = "", " "))
