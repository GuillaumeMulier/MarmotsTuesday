# ---------------------------------------------------------------- #
# Script de fonctions pour animer et plotter des courbes de Bézier #
# Créé le 19/09/2022, modifié le 19/09/2022                        #
# ---------------------------------------------------------------- #


'%nin%' <- function(x, y) match(x, y, nomatch = 0L) == 0L
Pkg <- c(
  "tidyverse", # Le meilleur package
  "glue", # Pour concaténer des messages
  "NatParksPalettes", # Palettes de couleurs de parcs au USA lol 
  "animation", # Créer un GIF avec une boucle de plots
  "progress", # Barre de progression
  "beepr", # Pour faire le son de micro-onde à la fin
  "rstudioapi" # Retrouver le chemin du script
)
NouveauxPkg <- Pkg[Pkg %nin% installed.packages()]
lapply(NouveauxPkg, install.packages)
lapply(Pkg, require, character.only = TRUE)
rm(Pkg, NouveauxPkg)

# Tentative de palette de couleurs lol
# Si tu personnalises ça, ça changera les couleurs des droites
PallettePerso <- NatParksPalettes$Arches[[1]][c(1, 8, 4, 5, 2, 7, 3, 6)]


# Fonction qui renvoie la valeur de l'interpolation de Bézier entre les points
# (ancres et points en plus inclus) pour la valeur de poids donnée
InterpBezier <- function(Ancres, Poids, Degre) {
  
  Interpolation <- choose(Degre, seq_len(Degre + 1) - 1) * (1 - Poids) ^ (rev(seq_len(Degre + 1) - 1)) * 
    Poids ^ (seq_len(Degre + 1) - 1) * Ancres
  Interpolation <- sum(Interpolation)
  
  return(Interpolation)
  
}


# Fonction pour séparer un vecteur en des vecteurs plus petits à des points d'intersections
DelimPts <- function(Vecteur, Longueur) {
  
  # On cherche le nombre de Bezier curves
  NbVec <- (length(Vecteur) - 1) / (Longueur - 1)
  Debuts <- seq(0, length(Vecteur) - 1, Longueur - 1) + 1
  Fins <- Debuts[-1]
  Debuts <- Debuts[-length(Debuts)]
  # Liste des points pour chaque courbe
  ListeVecteurs <- map2(Debuts, Fins, ~ Vecteur[.x:.y])
  
  return(ListeVecteurs)
  
}


# Fonction similaire pour avoir à l'interieur d'un groupe les vecteurs adjacents
AdjPts <- function(Vecteur, Longueur) {
  map(seq_len(length(Vecteur) - Longueur + 1), ~ Vecteur[.x:(.x + Longueur - 1)])
}


# Fonction qui crée le tableau pour les différents points de la courbe de Bézier,
# mais aussi des courbes de degré inférieur utilisées pour la construire
# Arguments :
## - Abscisses, Ordonnees : les vecteurs de coordonnée des points pour tracer les courbes
## - Degre : le degré max de polynôme de Bézier
## - NbInterpolation : le nombre d'étapes entre 2 points
ComputeBezier <- function(Abscisses,
                          Ordonnees,
                          Degre = 2L,
                          NbInterpolation = 50L) {
  
  # Checks des arguments
  stopifnot(length(Abscisses) == length(Ordonnees), is.numeric(Abscisses), is.numeric(Ordonnees))
  if (!is.numeric(Degre) || Degre %% 1 != 0 || Degre < 1)
    stop("\"Degre\" représente le degré des polynômes de Bézier et doit être un entier entre 1 et le nombre de points - 1.
         La marmotte va devoir changer ça, mais souvent on ne va pas plus loin que 3 ou 4.", call. = FALSE)
  if (!is.numeric(NbInterpolation) || NbInterpolation %% 1 != 0 || NbInterpolation < 1)
    stop("\"NbInterpolation\" représente le nombre de points pour approximer une droite.
         La marmotte essaye de ne pas faire une courbe de Bézier ? Il faut mettre un entier plus grand que 1.", call. = FALSE)
  if (length(Abscisses) %% Degre != 1)
    stop(glue("Désolé ma marmotte si c'est un peu compliqué, il faut qu'il y ait un nombre de points proportionnel au degré et un point en plus.
              Par exemple, tu as choisi une courbe de degré {Degre} donc tu peux prendre {paste(1 + (1:3) * Degre, collapse = ' ou ')} points."),
         call. = FALSE)
  
  # Convertir Abscisses et Ordonnees en des sets de points utilisés pour la courbe de Bézier principale
  AbscissesInteret <- DelimPts(Abscisses, Degre + 1)
  OrdonneesInteret <- DelimPts(Ordonnees, Degre + 1)
  
  # Définir une palette de couleurs
  PaletteBezier <- c(PallettePerso[seq_len(Degre - 1)], "#000000")
  
  # On va faire le calcul pour chaque point de début d'interpolation et pour chaque degré de polynome de Bézier
  TabBezier <-  map_dfr(seq_along(AbscissesInteret), # On va d'abord boucler sur les différentes courbes de Bézier qu'on souhaite
                         function(I) { # On va séparer les calculs pour les différents degrés possible (<= Degre)
                           Absc <- AbscissesInteret[[I]]
                           Ordo <- OrdonneesInteret[[I]]
                           map_dfr(seq_len(Degre),
                                   function(degre) {
                                     # on va reboucler sur les différentes sous-parties, c'est dégueu je sais xD
                                     XX <- AdjPts(Absc, degre + 1)
                                     YY <- AdjPts(Ordo, degre + 1)
                                     map_dfr(seq_along(XX),
                                             function(index) {
                                               data.frame(.poids = seq(0, 1, length.out = NbInterpolation)) |> 
                                                 rowwise() |> 
                                                 mutate(bezier_abscisse = InterpBezier(XX[[index]], .poids, degre),
                                                        bezier_ordonnee = InterpBezier(YY[[index]], .poids, degre),
                                                        courbe = glue("Deg{degre}-C{index}")) |> 
                                                 ungroup() |> 
                                                 mutate(etape = seq_len(NbInterpolation) + NbInterpolation * (I - 1),
                                                        deg = degre,
                                                        ind = index + length(XX) * (I - 1), 
                                                        couleur_anim = PaletteBezier[degre])
                                             })
                                   })
                         })
  attr(TabBezier, "steps") <- NbInterpolation * length(AbscissesInteret)
  attr(TabBezier, "pointsX") <- Abscisses
  attr(TabBezier, "pointsY") <- Ordonnees
  attr(TabBezier, "DegreMax") <- Degre
  attr(TabBezier, "Couleurs") <- PaletteBezier
  class(TabBezier) <- c(class(TabBezier), "Bezier")
  
  return(TabBezier)
  
}   


# Fonction qui va créer l'animation des courbes de Bézier qui bougent
# Arguments :
## - TableauBezier : le tableau issu de la fonction ComputeBezier qui regroupe les infos des points
## - Duree : la durée souhaitée de l'animation en secondes (parfois saveGIF est un peu capricieux, 
## mais en général mettre un dizaine de seconde avec 100 images ça passe)
## - CouleurBase : la couleur du fond de l'animation
## - FichierGIF : nom du fichier ou chemin pour sauvegarder le GIF
AnimerBezier <- function(TableauBezier,
                         Duree = 5,
                         CouleurBase = PallettePerso[5],
                         FichierGIF = "courbe_bezier") {
  
  if (!inherits(TableauBezier, "Bezier"))
    stop("Il faut donner un tableau créé avec la fonction ComputeBezier. Je vois que la marmotte cherche encore à filouter :)", call. = FALSE)
  
  # Retrouver les points entre lesquels on fait les courbes
  TabPts <- data.frame(X = attr(TableauBezier, "pointsX"),
                       Y = attr(TableauBezier, "pointsY"))
  
  # Donner le chemin du fichier de sortie
  if (!str_detect(FichierGIF, "/|\\.gif")) {
    Chemin <- glue("{dirname(rstudioapi::getSourceEditorContext()$path)}/{FichierGIF}.gif")
  } else {
    Chemin <- FichierGIF
  }
  
  # Configurer la barre de progression
  pb <- progress_bar$new(format = " [:bar] :percent (img :current / :total) ",
                         total = attr(TableauBezier, "steps"), clear = FALSE, width = getOption("width") - 30,
                         incomplete = " ", current = "|")
  
  # Trouver un pas de temps acceptable
  PasTemps <- round(Duree / attr(TableauBezier, "steps"), 4)
  
  # On trace le graphique de base pour ne pas avoir à la faire pour l'animation
  Graphique <- ggplot(data = NULL) +
    geom_path(data = TabPts, mapping = aes(X, Y), color = CouleurBase, size = .8) + 
    geom_point(data = TabPts, mapping = aes(X, Y), color = CouleurBase, size = 2) +
    coord_fixed(ratio = 1, ylim = c(min(TableauBezier$bezier_ordonnee), max(TableauBezier$bezier_ordonnee)),
                xlim = c(min(TableauBezier$bezier_abscisse), max(TableauBezier$bezier_abscisse))) +
    theme_void() +
    theme(plot.background = element_rect(fill = PallettePerso[4], color = PallettePerso[4])) +
    scale_color_identity()
  
  # Sauvegarder l'animation
  saveGIF(
    expr = {
      for (i in seq_len(attr(TableauBezier, "steps"))) {
        GraphIter <- Graphique +
          map(seq_len(attr(TableauBezier, "DegreMax")),
              function(degre) {
                if (degre == max(attr(TableauBezier, "DegreMax"))) {
                  list(
                    geom_path(data = TableauBezier[TableauBezier$etape <= i & TableauBezier$deg == degre, ], 
                              mapping = aes(bezier_abscisse, bezier_ordonnee, color = couleur_anim), size = 1.3),
                    annotate("point", size = 2.5, color = attr(TableauBezier, "Couleurs")[degre], shape = 21,
                             x = TableauBezier[TableauBezier$etape == i & TableauBezier$deg == degre, ][["bezier_abscisse"]],
                             y = TableauBezier[TableauBezier$etape == i & TableauBezier$deg == degre, ][["bezier_ordonnee"]])
                  )
                } else {
                  list(
                    annotate("point", size = 2, color = attr(TableauBezier, "Couleurs")[degre],
                             x = TableauBezier[TableauBezier$etape == i & TableauBezier$deg == degre, ][["bezier_abscisse"]],
                             y = TableauBezier[TableauBezier$etape == i & TableauBezier$deg == degre, ][["bezier_ordonnee"]]),
                    pmap(list(x = TableauBezier[TableauBezier$etape == i & TableauBezier$deg == degre, ][["bezier_abscisse"]] %>% '['(-length(.)),
                              xend = TableauBezier[TableauBezier$etape == i & TableauBezier$deg == degre, ][["bezier_abscisse"]] %>% '['(-1),
                              y = TableauBezier[TableauBezier$etape == i & TableauBezier$deg == degre, ][["bezier_ordonnee"]] %>% '['(-length(.)),
                              yend = TableauBezier[TableauBezier$etape == i & TableauBezier$deg == degre, ][["bezier_ordonnee"]] %>% '['(-1)),
                         function(x, xend, y, yend) {
                           annotate("segment", size = .8, x = x, xend = xend, y = y, yend = yend, 
                                    size = 1, color = attr(TableauBezier, "Couleurs")[degre])
                         })
                  )
                }
              })
        pb$tick()
        plot(GraphIter)
      }
    },
    movie.name = Chemin,
    interval = PasTemps,
    ani.width = 600,
    ani.height = 600
  )
  
}

