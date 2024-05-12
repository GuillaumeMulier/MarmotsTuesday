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
NPts <- 1000

set.seed(121221)
Grille <- tibble(
  id = 1:NPts,
  x = runif(NPts, 0, 20),
  y = runif(NPts, 0, 20),
  tps = 0
)
Grille <- accumulate(1:100, UpdateGrille, .init = Grille, Generator = gen_perlin, TimeStep = 5, frequency = .1) %>% 
  bind_rows()
Graphique <- ggplot(Grille, aes(x, y, group = id)) +
    theme_void() +
    geom_path(alpha = .25, color = "#0B0B3B") +
    coord_equal(xlim = c(0, 20), ylim = c(0, 20))
ggsave(Graphique, filename = "C:/Users/DRY12/Downloads/test2.png", device = "png", height = 10, width = 10, bg = "white")

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





smol_grid <- long_grid(x = seq(1, 20, .01), y = seq(1, 20, .01))
ggplot(smol_grid) +
  geom_point(aes(x, y), colour = "black") + 
  theme_void() + 
  coord_equal()
smol_simplex <- smol_grid |>
  mutate(z1 = gen_perlin(x, y, seed = 121221, frequency = .2),
         z2 = (gen_value(x, y, seed = 121221, frequency = .85)),
         z = z1 + sign(z1) * z2) 

smol_simplex |>
  ggplot(aes(x, y, size = z)) +
  geom_point(colour = "black", show.legend = FALSE) + 
  theme_void() + 
  coord_equal()
ggsave(smol_simplex |>
  ggplot(aes(x, y, z = z)) +
  geom_contour_filled(color = '#555555', show.legend = FALSE, bins = 15) + 
  theme_void() + 
    discrete_scale("fill", "myscale", colorRampPalette(c("#3346be", "#999999", "#aa0055"))) +
  coord_equal(), filename = "C:/Users/DRY12/Downloads/test3.png", device = "png", height = 10, width = 10)
smol_simplex <- smol_grid |>
  mutate(z1 = gen_perlin(x, y, seed = 121221, frequency = 1.5),
         z2 = (gen_value(x, y, seed = 121221, frequency = 1.5)),
         z = z1 + sign(z1) * z2) 

smol_simplex |>
  ggplot(aes(x, y, size = z)) +
  geom_point(colour = "black", show.legend = FALSE) + 
  theme_void() + 
  coord_equal()
ggsave(smol_simplex |>
  ggplot(aes(x, y, z = z)) +
  geom_contour_filled(color = '#cccccc', show.legend = FALSE, bins = 25, size = .1) + 
  theme_void() + 
    discrete_scale("fill", "myscale", colorRampPalette(c("#3346be", "#999999", "#aa0055"))) +
  coord_equal(), filename = "C:/Users/DRY12/Downloads/test4.png", device = "png", height = 10, width = 10)
eps <- .01
smol_curl <- smol_grid |> mutate(
  x_add = gen_perlin(x + eps, y, seed = 1, frequency = .1),
  x_sub = gen_perlin(x - eps, y, seed = 1, frequency = .1),
  y_add = gen_perlin(x, y + eps, seed = 1, frequency = .1),
  y_sub = gen_perlin(x, y - eps, seed = 1, frequency = .1),
  x_slope = 5 * (x_add - x_sub) / (2 * eps), 
  y_slope = 5 * (y_add - y_sub) / (2 * eps),
  x_curl = -y_slope, 
  y_curl = x_slope
)
ggplot(smol_curl) + 
  geom_segment(
    mapping = aes(
      x = x, 
      y = y, 
      xend = x + x_slope * 2, 
      yend = y + y_slope * 2
    ), 
    colour = "black", 
    arrow = arrow(length = unit(0.1, "cm"))
  ) + 
  theme_void() + 
  coord_equal()
