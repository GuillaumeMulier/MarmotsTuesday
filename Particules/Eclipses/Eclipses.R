# ----------------------------- #
# Essai pour faire des éclipses #
# Auteur : G. Mulier            # 
# Créé le 06/07/2024            #
# Modifié le 06/07/2024         #
# ----------------------------- #

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

# Packages ----

library(tidyverse)

# Fonctions ----

## Create a circle
CreerCercle <- function(NPts, Rayon) {
  tibble(angles = seq(0, 2 * pi, length.out = NPts),
         x = Rayon * cos(angles),
         y = Rayon * sin(angles))
}

## Add random noise to create the light
AddLight <- function(NPts, Rayon, PtsDensite, Dispersion) {
  Base <- CreerCercle(NPts, Rayon)
  DemiAngle <- mean((Base$angles[-1] - Base$angles[-NPts]) / 2)
  map_dfr(seq_len(NPts - 1), \(index) {
    Distances <- abs(rnorm(PtsDensite, 0, Dispersion))
    Offset <- runif(PtsDensite, -DemiAngle, DemiAngle)
    tibble(angles = Base$angles[index] + Offset,
           x = (Distances + Rayon) * cos(angles),
           y = (Distances + Rayon) * sin(angles))
  })
}


# Tests d'éclipses ----

Donnees <- CreerCercle(100, 5)
Lumiere <- AddLight(30, 4.8, 1e+5, .5)

Graphe <- ggplot() +
  geom_point(data = Lumiere, aes(x, y), shape = 20, size = .05, alpha = .2, color = rgb(252, 171, 162, maxColorValue = 255)) +
  geom_polygon(data = Donnees, aes(x, y), color = "black", fill = rgb(19, 53, 25, maxColorValue = 255)) +
  coord_equal() +
  theme_void() +
  theme(panel.background = element_rect(fill = rgb(15, 2, 110, maxColorValue = 255)))
ggsave(Graphe, filename = paste0(Chemin, "/eclipse1.png"), device = "png", height = 14, width = 14, dpi = 300)

Graphe <- ggplot() +
  geom_density_2d_filled(data = Lumiere, aes(x, y)) +
  geom_polygon(data = Donnees, aes(x, y), color = "black", fill = rgb(19, 53, 25, maxColorValue = 255)) +
  coord_equal() +
  theme_void() +
  theme(panel.background = element_rect(fill = rgb(15, 2, 110, maxColorValue = 255)))
ggsave(Graphe, filename = paste0(Chemin, "/eclipse2.png"), device = "png", height = 14, width = 14, dpi = 300)

plot(Donnees$x, Donnees$y, xlim = c(-6, 10))
Ex <- tibble(angles = Base$angles[index] + Offset,
             x = (Distances + Rayon) * cos(angles),
             y = (Distances + Rayon) * sin(angles))
points(Ex$x, Ex$y, pch = ".", cex = .5)
