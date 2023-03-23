library(tidyverse)
library(ambient)

MatPerlin <- expand.grid(xx = seq(-.5, 1.5, .001), yy = seq(-.5, 1.5, .01))
set.seed(121221)
MatPerlin$bruit <- gen_perlin(MatPerlin$xx, MatPerlin$yy, frequency = 15)
(GraphePerlin <- MatPerlin %>% 
  mutate(ordonnee = yy + bruit / 15 * cos(yy * 2 * pi)) %>% 
  ggplot(aes(x = xx, y = ordonnee, group = yy, )) +
  geom_line(color = "white") +
  theme_void() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme(plot.background = element_rect(fill = "#000012", color = "white", size = 2),
        plot.margin = unit(c(1, 1, 1, 1), "mm")))
ggsave(GraphePerlin, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/CoverPerlin1.png"),
       device = "png", height = 7, width = 7)


MatPerlin <- expand.grid(xx = -5 + seq(-.5, 1.5, .001), yy = -5 + seq(-.5, 1.5, .015))
set.seed(121221)
MatPerlin$bruit <- gen_perlin(MatPerlin$xx, MatPerlin$yy, frequency = 20)
(GraphePerlin <- MatPerlin %>% 
    mutate(ordonnee = yy + bruit / 15) %>% 
    ggplot(aes(x = xx, y = ordonnee, group = yy, )) +
    geom_line(color = "white") +
    theme_void() +
    coord_cartesian(xlim = c(-5, -4), ylim = c(-5, -4)) +
    theme(plot.background = element_rect(fill = "#000012", color = "white", size = 2),
          plot.margin = unit(c(1, 1, 1, 1), "mm")))
ggsave(GraphePerlin, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/CoverPerlin2.png"),
       device = "png", height = 7, width = 7)






MatPerlin <- expand.grid(xx = 15 + seq(-.5, 1.5, .001), yy = 50 + seq(-.5, 1.5, .01))
set.seed(121221)
MatPerlin$bruit <- gen_perlin(MatPerlin$xx, MatPerlin$yy, frequency = 40)
(GraphePerlin <- MatPerlin %>% 
    mutate(ordonnee = yy + bruit / 20 * cos(pi / 2 + xx * pi)) %>% 
    ggplot(aes(x = xx, y = ordonnee, group = yy, )) +
    geom_line(color = "white") +
    theme_void() +
    coord_cartesian(xlim = c(15, 16), ylim = c(50, 51)) +
    theme(plot.background = element_rect(fill = "#000012", color = "white", size = 2),
          plot.margin = unit(c(1, 1, 1, 1), "mm")))
ggsave(GraphePerlin, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/CoverPerlin3.png"),
       device = "png", height = 7, width = 7)


MatPerlin <- expand.grid(xx = 15 + seq(-.5, 1.5, .001), yy = 50 + seq(0, 1.1, .025))
set.seed(121221)
MatPerlin$bruit <- gen_perlin(MatPerlin$xx, MatPerlin$yy, frequency = 10)
Palette <- colorRampPalette(c("#11FD02", "#B31F31"), interpolate = "linear")
Couleurs <- Palette(45)
Couleurs <- sample(Couleurs)
MatPerlin$couleur <- Couleurs[match(MatPerlin$yy, 50 + seq(0, 1.1, .025))]
MatPerlin$ordonnee <- MatPerlin$yy + (MatPerlin$bruit) ** 3 * cos(pi / 2 + MatPerlin$xx * 2 * pi)
(GraphePerlin <- ggplot(mapping = aes(x = xx, y = ordonnee, group = yy, fill = couleur)) +
    map(rev(50 + seq(0, 1.5, .025)), ~ geom_area(data = MatPerlin %>% filter(yy == .x), color = "black")) +
    scale_fill_identity() +
    scale_color_identity() +
    theme_void() +
    coord_cartesian(xlim = c(15, 16), ylim = c(50, 51)) +
    theme(plot.background = element_rect(fill = "#030E59", color = "white", size = 2),
          plot.margin = unit(c(1, 1, 1, 1), "mm")))
ggsave(GraphePerlin, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/CoverPerlin4.png"),
       device = "png", height = 7, width = 7)


