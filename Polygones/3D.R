library(tidyverse)
Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

# Tableaux de données des faces
Donnees <- list(
  avant = tibble(x = c(1, 1, 1, 1, 1), y = c(1, 1, 2, 2, 1), z = c(1, 2, 2, 1, 1), couleur = rep("#de2517", 5)),
  arriere = tibble(x = c(2, 2, 2, 2, 2), y = c(1, 1, 2, 2, 1), z = c(1, 2, 2, 1, 1), couleur = rep("#cede17", 5)),
  droite = tibble(x = c(1, 1, 2, 2, 1), y = c(2, 2, 2, 2, 2), z = c(1, 2, 2, 1, 1), couleur = rep("#24de17", 5)),
  gauche = tibble(x = c(1, 1, 2, 2, 1), y = c(1, 1, 1, 1, 1), z = c(1, 2, 2, 1, 1), couleur = rep("#337e27", 5)),
  haut = tibble(x = c(1, 1, 2, 2, 1), y = c(1, 2, 2, 1, 1), z = c(2, 2, 2, 2, 2), couleur = rep("#1dcfc9", 5)),
  bas = tibble(x = c(1, 1, 2, 2, 1), y = c(1, 2, 2, 1, 1), z = c(1, 1, 1, 1, 1), couleur = rep("#a712a7", 5))
)

# Fonction pour projeter les coordonnées en 2D
Projection <- function(Matrice, PosCam = c(-1, -1, -1), OrientationCam = c(0, 0, 0), SurfaceProj = c(1, 1, 1), Couleur) {
  RotMat <- matrix(c(cos(OrientationCam[3]) * cos(OrientationCam[2]), cos(OrientationCam[2]) * sin(OrientationCam[3]), -sin(OrientationCam[2]),
           sin(OrientationCam[1]) * sin(OrientationCam[2]) * cos(OrientationCam[3]) - cos(OrientationCam[1]) * sin(OrientationCam[3]), sin(OrientationCam[1]) * sin(OrientationCam[2]) * sin(OrientationCam[3]) + cos(OrientationCam[1]) * cos(OrientationCam[3]), sin(OrientationCam[1]) * cos(OrientationCam[2]),
           cos(OrientationCam[1]) * sin(OrientationCam[2]) * cos(OrientationCam[3]) + sin(OrientationCam[1]) * sin(OrientationCam[3]), cos(OrientationCam[1]) * sin(OrientationCam[2]) * sin(OrientationCam[3]) - sin(OrientationCam[1]) * cos(OrientationCam[3]), cos(OrientationCam[1]) * cos(OrientationCam[2])),
         ncol = 3, nrow = 3)
  VecD <- RotMat %*% (Matrice - PosCam)
  VecF <- matrix(c(1, 0, 0,
                   0, 1, 0,
                   SurfaceProj[1] / SurfaceProj[3], SurfaceProj[2] / SurfaceProj[3], 1 / SurfaceProj[3]),
                 ncol = 3, nrow = 3) %*% VecD
  return(data.frame(x = VecF[1, ] / VecF[3, ], y = VecF[2, ] / VecF[3, ], couleur = Couleur))
}

# Retrouver les coordonnées en 2D
ListeFaces <- lapply(Donnees, \(tab) Projection(Matrice = t(as.matrix(tab[, 1:3])), PosCam = c(-5, -5, -5), Couleur = tab$couleur, SurfaceProj = c(.1, .1, .1), OrientationCam = c(.1, .5, 2)))

# Plotter la projection
Graphe <- ggplot() +
  map(ListeFaces, \(.Data) geom_polygon(data = .Data, aes(x, y, fill = couleur), color = "black", alpha = .5)) +
  coord_fixed(ratio = 1) +
  scale_fill_identity() +
  theme_void()
ggsave(Graphe, filename = paste0(Chemin, "/essai_cube.png"), device = "png", height = 6, width = 6)

