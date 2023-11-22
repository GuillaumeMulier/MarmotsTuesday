library(Rcpp)

sourceCpp("C:/Users/gmulier/Documents/Github/MarmotsTuesday/Repetition/DiabolicNumber.cpp")

NSimu <- 1e+7
Somme <- 666

cat("Début de ", NSimu, " simulations pour trouver la somme ", Somme, " :\n")

NbOccurences <- CountDeci(NSimu, Somme, 121221)

cat("Proportion trouvée : ", round(NbOccurences / NSimu * 100, 2), "%\n")
