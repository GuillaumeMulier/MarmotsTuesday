library(imager)
library(ambient)
library(tidyverse)
library(patchwork)

UpdateGrille <- function(Grille, Iteration, Generator = gen_perlin, TimeStep = .05, ...) {
  FlowField <- curl_noise(
    generator = Generator,
    x = Grille$x,
    y = Grille$y,
    ...
  )
  Grille <- Grille %>% 
    mutate(
      x = x + FlowField$x * TimeStep,
      y = y + FlowField$y * TimeStep,
      tps = tps + 1
    )
  return(Grille)
}

Dimensions <- c(268, 268)
NPts <- 150

set.seed(121221)
Grille <- tibble(
  id = 1:NPts,
  x = runif(NPts, 0, 1),
  y = runif(NPts, 0, 1),
  tps = 0
)
Grille <- accumulate(1:1000, UpdateGrille, .init = Grille, Generator = gen_simplex, TimeStep = .005, octaves = 6, frequency = 1.5) %>% 
  bind_rows()
Graphique <- ggplot(Grille, aes(x, y, group = id)) +
    theme_void() +
    geom_path(alpha = .25, color = "#0B0B3B") +
    coord_equal()

Marmotte <- load.image("C:/Users/DRY12/Documents/GitHub/MarmotsTuesday/Fractals/Perlin/dessin_marmotte.png")
Grille2 <- Marmotte %>%
  # resize(size_x = 100, size_y = 100) %>% 
  as.data.frame() %>% 
  group_by(x, y) %>% 
  summarise(value = mean(value), .groups = "drop") %>% 
  filter(value < 1) %>% 
  slice_sample(n = 500) %>% 
  select(x, y) %>% 
  mutate(tps = 0, id = row_number(),
         x = normalize(x), y = normalize(y))
Grille2 <- accumulate(1:200, UpdateGrille, .init = Grille2, Generator = gen_simplex, TimeStep = .005, octaves = 4, frequency = 1.2, lacunarity = 1.5) %>% 
  bind_rows()
Graphique2 <- ggplot(Grille2, aes(x, y, group = id)) +
    theme_void() +
    geom_path(alpha = .25, color = "white", show.legend = FALSE, size = .25) +
    coord_equal() +
    theme(panel.background = element_rect(fill = "#0B0B3B", color = "#0B0B3B"))

ggsave(Graphique | Graphique2, filename = "C:/Users/DRY12/Documents/GitHub/MarmotsTuesday/Fractals/Perlin/essai_FF.png", device = "png", height = 10, width = 10)
