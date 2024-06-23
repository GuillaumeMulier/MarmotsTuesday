library(imager)
library(ambient)
library(tidyverse)
library(patchwork)

NPts <- 500
Resolution <- .05

VisualiserFField <- function(.Data) {
  ggplot(.Data, aes(x, y)) +
         geom_tile(aes(fill = bruit)) +
         theme_void() +
         coord_equal()
}

GenererFField <- function(.Data, Bruit = gen_perlin, Graine, ...) {
  .Data$bruit <- fracture(noise = Bruit, seed = Graine, 
                          x = .Data$x, y = .Data$y, ...)
  return(.Data)
}

UpdateFField <- function(.Data, Iteration, Bruit = gen_perlin, Resolution = .05, 
                         XLims = c(0, 1), YLims = c(0, 1), Graine, ...) {
  if (Iteration != 1) {
    .Data$x <- .Data$xend
    .Data$y <- .Data$yend
    OutBounds <- .Data$x > XLims[2] | .Data$x < XLims[1] | .Data$y > YLims[2] | .Data$y < YLims[1]
    .Data$x[OutBounds] <- runif(sum(OutBounds), XLims[1], XLims[2])
    .Data$y[OutBounds] <- runif(sum(OutBounds), YLims[1], YLims[2])
    .Data$id[OutBounds] <- seq(max(.Data$id) + 1, max(.Data$id) + sum(OutBounds))
  }
  .Data$bruit <- fracture(noise = Bruit, seed = Graine, 
                          x = .Data$x, y = .Data$y, ...)
  .Data$bruit <- (.Data$bruit - min(.Data$bruit)) / (max(.Data$bruit) - min(.Data$bruit)) * 2 * pi
  .Data$xend <- .Data$x + Resolution * cos(.Data$bruit)
  .Data$yend <- .Data$y + Resolution * sin(.Data$bruit)
  return(.Data)
}

# Exploration du flow field pour trouver des paramètres qui sont pas mal
XLim <- c(0, 10)
YLim <- c(0, 10)
NOctaves <- 6
GrilleTest <- expand.grid(x = seq(XLim[1], XLim[2], length.out = 250), y = seq(YLim[1], YLim[2], length.out = 250))
GrilleTest <- GenererFField(GrilleTest, gen_perlin, Graine = 121221, octaves = NOctaves, fractal = fbm, 
                         frequency = .75 * 1.5 ** seq(1, NOctaves), gain = ~ . / 2)
VisualiserFField(GrilleTest)

# Générer le flowfield avec les paramètres trouvés
set.seed(121221)
Grille <- data.frame(id = seq_len(NPts), x = runif(NPts, XLim[1], XLim[2]), y = runif(NPts, YLim[1], YLim[2]))



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





smol_grid <- long_grid(x = seq(1, 20, .1), y = seq(1, 20, .1))
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

smol_curl <- smol_simplex |> mutate(
  z = (z - min(z)) / (max(z) - min(z)) * 2 * pi,
  x_end = x + .1 * cos(z),
  y_end = y + .1 * sin(z)
)

ggplot(smol_curl) + 
  geom_segment(
    mapping = aes(
      x = x, 
      y = y, 
      xend = x_end,
      yend = y_end
    ), 
    colour = "black", 
    arrow = arrow(length = unit(0.1, "cm"))
  ) + 
  theme_void() + 
  coord_equal()




Grille <- long_grid(x = runif(5e+2, 0, 1), y = runif(5e+2, 0, 1))
Grille <- long_grid(x = seq(0, 1, .001), y = seq(0, 1, .001))
Grille <- Grille %>%
  mutate(bruit = gen_perlin(x, y, seed = 121221, frequency = 3)) 
Grille$bruit <- (Grille$bruit - min(Grille$bruit)) / (max(Grille$bruit) - min(Grille$bruit)) * 2 * pi
Grille$x_end <- Grille$x + .05 * cos(Grille$bruit)
Grille$y_end <- Grille$y + .05 * sin(Grille$bruit)
TestGrille <- accumulate(1:50, \(donnees, index) {
  print(index)
  donnees$x <- donnees$x_end
  donnees$y <- donnees$y_end
  donnees$bruit <- gen_perlin(donnees$x, donnees$y, seed = 121221, frequency = 3)
  donnees$bruit <- (donnees$bruit - min(donnees$bruit)) / (max(donnees$bruit) - min(donnees$bruit)) * 2 * pi
  donnees$x_end <- donnees$x + .0005 * cos(donnees$bruit)
  donnees$y_end <- donnees$y + .005 * sin(donnees$bruit)
  return(donnees)
}, .init = Grille)
TestGrille <- do.call("rbind", TestGrille)

ggsave(ggplot(TestGrille, aes(x, y)) + 
  geom_segment(aes(xend = x_end, yend = y_end), alpha = .001) +
  theme_void() +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)),
  filename = "GitHub/MarmotsTuesday/Fractals/Perlin/essai1.png",
  device = "png", height = 20, width = 20, bg = "white")
