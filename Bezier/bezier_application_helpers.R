# ---------------------------------------------- #
# Scripts de fonctions d'aide pour l'application #
# Créé le 01/10/2022, modifié le 01/10/2022      #
# ---------------------------------------------- #


# Initier le tableau des points
InitierTableau <- function() {
  TableauPoints <- data.frame(
    XX = numeric(0L),
    YY = numeric(0L)
  )
  return(TableauPoints)
}

# Ajouter des lignes au tableau
