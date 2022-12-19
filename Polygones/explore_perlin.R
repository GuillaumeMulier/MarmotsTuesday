library(tidyverse)
library(ambient)
library(NatParksPalettes)

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

GenererCercle <- function(Rayon, NPoints, Couleur) {
  Angles <- seq(0, 2 * pi, length.out = NPoints + 1)
  data.frame(angle = Angles,
             xx = Rayon * cos(Angles),
             yy = Rayon * sin(Angles),
             couleur = Couleur)
}

set.seed(121221)
Couleurs <- natparks.pals("Arches", 200)
Rayons <- sort(abs(runif(200, -20, 100)))

Tableau <- map2_dfr(Couleurs, Rayons, 
    function(Couleurs, Rayons) {
      GenererCercle(Rayons, 500, Couleurs)
    },
    .id = "cercle") %>% 
  mutate(bruit = gen_perlin(xx, yy, frequency = .35),
         distance = sqrt(xx ** 2 + yy ** 2), 
         xx = xx + bruit * distance ** (1 / 3) * cos(angle),
         yy = yy + bruit * distance ** (1 / 3) * sin(angle))

Graphique1 <- ggplot(Tableau, aes(xx, yy, group = cercle, color = couleur)) +
  geom_path(show.legend = FALSE, size = .5) +
  coord_equal(ratio = 1) +
  scale_color_identity() +
  theme_void() +
  theme(panel.background = element_rect(color = "#020A3D", fill = "#020A3D"))
ggsave(Graphique1, filename = paste0(Chemin, "/cercle_perlin.png"),
       device = "png", height = 8, width = 8)
ggsave(Graphique1 + coord_polar(), filename = paste0(Chemin, "/donut_perlin.png"),
       device = "png", height = 8, width = 8)
