# ---------------------------------------------- #
# Script pour 2 échelles de couleurs différentes #
# ---------------------------------------------- #

library(ggnewscale)
library(tidyverse)
library(MASS)

Donnees1 <- data.frame(xx = runif(5000, -1, 0),
                       yy = runif(5000, 0, 10)) %>% 
  mutate(groupe = factor(yy > 3, levels = c(F, T), labels = c("y<=3", "y>3")))
Donnees2 <- mvrnorm(20000, mu = c(5, 6), Sigma = matrix(c(4, 1, 8, 3), nrow = 2)) %>% 
  as.data.frame() %>% 
  rename(xx = V1, yy = V2) %>% 
  mutate(distance = sqrt((xx - 5) ^ 2 + (yy - 6) ^ 2))

Graphique <- ggplot() +
  geom_point(data = Donnees1, mapping = aes(xx, yy, color = groupe)) +
  scale_color_manual(name = "Groupe", values = c("red", "green")) +
  new_scale_color() +
  geom_point(data = Donnees2, mapping = aes(xx, yy, color = distance)) +
  scale_color_gradient2(name = "Distance", high = "blue", low = "white", mid = "yellow", midpoint = .35) +
  theme_void()
ggsave(Graphique, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/double_couleurs.png"),
       device = "png", height = 6, width = 6, bg = "white")
