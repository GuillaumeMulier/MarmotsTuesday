# ---------------------------------------------------------------------- #
# Script pour tester des distances entre mots pour fusionner des tables  #
# ---------------------------------------------------------------------- #

# Idea coming from the TidyX cast
# https://www.youtube.com/watch?v=j78_7tF7HSE

library(tidyverse)
library(fuzzyjoin)

# Table of the regions (departments actually)
Tableau1 <- tribble(
  ~ville, ~region, 
  "Arras", "Pas-de-Calais",
  "Bénifontaine", "Pas-de-Calais",
  "Carvin", "Pas-de-Calais",
  "Carnin", "Pas-de-Calais",
  "Lille", "Nord",
  "Cambrai", "Nord"
)

# Table with messy names to join with table 1
Tableau2 <- tribble(
  ~ville, ~annee, ~habitant,
  "Arras", 2000, 15000,
  "Arrass", 2010, 16000,
  "Benifontaine", 2000, 10000,
  "Bénifontaine", 2010, 11000,
  "Carvin", 2000, 12000,
  "Caervin", 2010, 14000,
  "Carnnn", 2000, 6000,
  "Cartin", 2010, 6500,
  "Lll", 2000, 75000,
  "Lilel", 2010, 80000,
  "Lille", 2020, 70000,
  "Cambray", 2000, 30000,
  "Cmabray", 2010, 32000,
  "Cmbreye", 2020, 31000
)

left_join(Tableau2, Tableau1, by = "ville")
# Not much retrieved

stringdist_join(Tableau2, Tableau1, by = "ville", mode = "left")
# With package fuzyjoin we retrieve almost all the table, except for 1 error (Carnin -> Carvin)

# Create a helper and source the functions to understand the distances
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/string_distances.R"))
CorrespNom <- function(Nom,
                       Dico,
                       Distance = "DamerauLevenshtein",
                       Seuil) {
  
  Resultat <- character(length(Nom))
  Dist <- rep(Inf, length(Nom))
  for (n in Dico) {
    Dists <- DistanceMots(Nom, n, Methode = Distance)
    Resultat[Dists <= Seuil & Dists < Dist] <- n
    Dist[Dists <= Seuil & Dists < Dist] <- Dists[Dists <= Seuil & Dists < Dist]
  }
  return(Resultat)
  
}

# Same output as fuzyjoin function huray!
Tableau2 %>% 
  mutate(ville_corrigee = CorrespNom(ville, unique(Tableau1$ville), 
                                     Distance = "DamerauLevenshtein", Seuil = 2)) %>% 
  left_join(Tableau1, by = c("ville_corrigee" = "ville"))
# Levenshtein distance doesn't take into account transpositions
Tableau2 %>% 
  mutate(ville_corrigee = CorrespNom(ville, unique(Tableau1$ville), 
                                     Distance = "Levenshtein", Seuil = 2)) %>% 
  left_join(Tableau1, by = c("ville_corrigee" = "ville"))
# Increasing threshold might reduce number of non matches, but may lead to higher number of mistakes
Tableau2 %>% 
  mutate(ville_corrigee = CorrespNom(ville, unique(Tableau1$ville), 
                                     Distance = "DamerauLevenshtein", Seuil = 4)) %>% 
  left_join(Tableau1, by = c("ville_corrigee" = "ville"))

# To further refine the joining, we can adjust the weights (for now it's 1 for all).
# For example, we can say that deletion or insertion worth more than transposition.
# Or account for closeness of letters on keyboard by having lower weights in keys are closer.

