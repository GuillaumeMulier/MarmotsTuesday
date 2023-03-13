library(tidyverse)
library(ambient)
library(NatParksPalettes)
library(scales)

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

GenererCercle <- function(Rayon, NPoints, Couleur) {
  Angles <- seq(0, 2 * pi, length.out = NPoints + 1)
  data.frame(angle = Angles,
             xx = Rayon * cos(Angles),
             yy = Rayon * sin(Angles),
             couleur = Couleur)
}

# Les couleurs utilisées
Couleurs <- natparks.pals("Arches", 2, type = "continuous")

# Faire 3 cercles
Cercles <- map(c(3, 6, 10), GenererCercle, NPoints = 500, Couleur = "#555555")

# Ajouter du bruit de Perlin sur les cercles avec différentes
set.seed(121221)
BruitPerlin <- map2(c(2, 4, 8), c(.01, .1, .02), ~ noise_perlin(dim = c(1000, 1000), frequency = .y, octaves = .x))
Cercles <- pmap(list(Cercles = Cercles, Bruit = BruitPerlin, Amplification = c(1, 2, 1)),
     \(Cercles, Bruit, Amplification) {
       Cercles$rayon <- sqrt(Cercles$xx ** 2 + Cercles$yy ** 2)
       Rayon <- Cercles$rayon[1]
       Cercles$matX <- 1 + floor((Cercles$xx + Cercles$rayon) * 999 / 2 / Cercles$rayon)
       Cercles$matY <- 1 + floor((Cercles$yy + Cercles$rayon) * 999 / 2 / Cercles$rayon)
       Cercles$bruit <- Bruit[as.matrix(Cercles[, c("matX", "matY")])]
       Cercles$rayon <- Cercles$rayon + Cercles$bruit
       Cercles$abscisse <- seq(0, 2 * pi, length.out = nrow(Cercles))
       return(Cercles)
     })
Cercles <- map_dfr(1:3, ~ Cercles[[.x]] %>% mutate(num_cercle = .x))

# Datamanagement pour avoir les couleurs dans le tableau de données
Cercles <- Cercles %>% 
  select(num_cercle, abscisse, rayon) %>% 
  group_by(num_cercle) %>% 
  mutate(id = row_number()) %>% 
  pivot_wider(names_from = "num_cercle", values_from = "rayon", names_glue = "rayon_{num_cercle}") %>% 
  mutate(hauteur_1 = rayon_1,
         hauteur_2 = rayon_2 - rayon_1,
         hauteur_3 = rayon_3 - rayon_2,
         init_1 = 0,
         init_2 = rayon_1,
         init_3 = rayon_2) %>% 
  pivot_longer(cols = -c(1, 2), names_pattern = "^(.*)_(\\d)$", names_to = c("var", "cercle")) %>% 
  pivot_wider(names_from = "var") %>% 
  group_by(cercle) %>% 
  arrange(cercle, id) %>% 
  mutate(abscisse_max = lead(abscisse)) %>% 
  ungroup() %>% 
  mutate(interp = map2(cercle, hauteur, \(c, h) {
    data.frame(yy = seq(0, h, length.out = 10)) %>% 
      mutate(couleur = colorRampPalette(colors = if (c == 2) rev(Couleurs) else Couleurs, interpolate = "spline")(nrow(.)))
  })) %>% 
  unnest(interp) %>% 
  mutate(yy = yy + init) %>% 
  group_by(cercle, id) %>% 
  mutate(yy_max = lead(yy)) %>% 
  ungroup() %>% 
  filter(!is.na(abscisse_max), !is.na(yy_max))

Image <- ggplot(Cercles, aes(xmin = abscisse, xmax = abscisse_max, ymin = yy, ymax = yy_max, fill = couleur)) +
  geom_rect() +
  theme_void() +
  coord_polar() +
  scale_fill_identity()
ggsave(Image, device = "png", height = 8, width = 8,
       filename = paste0(Chemin, "/perlin_circles2.png"))
