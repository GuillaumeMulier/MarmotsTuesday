# ---------------------------------------------------------------- #
# Script pour faire le jeu du chaos pour le triangle de Sierpinski #
# ---------------------------------------------------------------- #

# Packages ----

library(tidyverse)
library(animation)
library(progress)

Couleurs <- c("#fff421", "#177fff", "#e330ff")
Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

MilieuPoints <- function(Point1, Point2) c((Point1[1] + Point2[1]) / 2, (Point1[2] + Point2[2]) / 2)


# Tableau de données ----

# On fait un triangle. J'ai pris un triangle équilatéral, mais tu peux prendre 3 points quelconques.
XTriangle <- 3 * cos(seq(pi / 2, 5 * pi / 2, length.out = 4)[-4])
YTriangle <- 3 * sin(seq(pi / 2, 5 * pi / 2, length.out = 4)[-4])

# Le point initial qu'on choisit au hasard (c(x, y))
Point <- c(0, 0)

# Faire la règle : choisir un des sommets du triangle au hasard, et prendre le milieu entre le point et le sommet choisi
set.seed(121221)
NPoints <- 19999
Probabilites <- runif(NPoints, 0, 3)
DonneesPoints <- data.frame(
  iterations = seq_len(NPoints + 1) - 1,
  abscisses = rep(NA_real_, NPoints + 1),
  ordonnees = rep(NA_real_, NPoints + 1),
  couleur = rep("#FFFFFF", NPoints + 1)
)
DonneesPoints[1, 2:3] <- Point
for (i in seq_len(NPoints)) {
  Sommet <- sum(0:2 < Probabilites[i])
  DonneesPoints[i + 1, 2:3] <- MilieuPoints(c(XTriangle[Sommet], YTriangle[Sommet]), Point) 
  DonneesPoints$couleur[i + 1] <- Couleurs[Sommet]
  Point <- DonneesPoints[i + 1, 2:3]
}

# La figure ----

(PlotSierpinski <- ggplot(DonneesPoints, aes(abscisses, ordonnees, color = couleur)) +
   geom_point(alpha = .3, size = .25) +
   scale_color_identity() +
   theme_void() +
   theme(plot.background = element_rect(fill = "#070738")) +
   coord_fixed(ratio = 1))
ggsave(PlotSierpinski, filename = paste0(Chemin, "/triangle_points.png"),
       device = "png", width = 1200, height = 1200, units = "px")


# L'animation ----

# Nombre de frames
NFrames <- (NPoints + 1) / 100
pb <- progress_bar$new(total = NFrames,
                       format = " {:bar} :percent (:current/:total)") # Barre de progression
saveGIF(
  expr = {
    for (i in seq_len(NFrames)) {
      PS <- ggplot(DonneesPoints[seq(1, i * 100), ], aes(abscisses, ordonnees, color = couleur)) +
        geom_point(alpha = .3, size = .25) +
        scale_color_identity() +
        theme_void() +
        theme(plot.background = element_rect(fill = "#070738")) +
        coord_fixed(ratio = 1, xlim = c(-2.8, 2.8), ylim = c(-1.6, 3.1))
      pb$tick()
      print(PS)
    }
  },
  movie.name = paste0(Chemin, "/triangleS.gif"),
  interval = .1, 
  ani.width = 1200,
  ani.height = 1200
)

