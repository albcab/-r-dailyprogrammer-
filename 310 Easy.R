
# Challenge 310 Easy ------------------------------------------------------

lotto <- function(names, output) {
  lottos <- matrix(nrow = length(names), ncol = output + 1)
  lottos[,1] <- t(sample(names, length(names)))
  options <- t(apply(lottos, 1, function(x) sample(names[(x[1] != names)], output)))
  lottos[,-1] <- options#[times,]
  return(lottos)
}

challenge <- "Rebbeca Gann;Latosha Caraveo;Jim Bench;Carmelina Biles;Oda Wilhite;Arletha Eason ;Theresa Kaczorowski;Jane Cover;Melissa Wise;Jaime Plascencia;Sacha Pontes;Tarah Mccubbin;Pei Rall;Dixie Rosenblatt;Rosana Tavera;Ethyl Kingsley;Lesia Westray;Vina Goodpasture;Drema Radke;Grace Merritt;Lashay Mendenhall;Magali Samms;Tiffaney Thiry;Rikki Buckelew;Iris Tait;Janette Huskins;Donovan Tabor;Jeremy Montilla;Sena Sapien;Jennell Stiefel"
input <- read.table(textConnection(challenge), sep = ";", colClasses = "character")

output <- lotto(input, 15)
output <- cbind(output[,1], rep(">", nrow(output)), output[,-1])

apply(output, 1, cat, "\n")
