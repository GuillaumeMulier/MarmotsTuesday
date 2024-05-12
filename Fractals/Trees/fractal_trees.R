library(Rcpp)
library(tidyverse)
library(ambient)
library(ggfx)

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

# Partie corail ----

sourceCpp("~/GitHub/MarmotsTuesday/Fractals/Trees/fractal_trees.cpp")

set.seed(121221)
MatExemple <- IterateTree(13, 0, 0, 0, 1, Angles = c(-pi / 6, 0, pi / 7.5), 
                          Shrinkage = c(.75, .8, .75), 
                          PerteEpaisseur = c(.7, .65, .7), 
                          ProbaRester = c(1, .25, 1), PuissRester = .25, 
                          ProbaAlea = c(0, .06, 0), CoefAlea = 1, PuissAlea = .95)
# MatExemple <- IterateTree(14, 0, 0, 0, 1, Angles = c(-pi / 6, pi / 8), 
#                           Shrinkage = c(.75, .7), 
#                           PerteEpaisseur = c(.9, .9), 
#                           ProbaRester = c(1, 1), PuissRester = 1, 
#                           ProbaAlea = c(0, 0), CoefAlea = 1, PuissAlea = .95)
MatExemple <- as.data.frame(MatExemple)
names(MatExemple) <- c("iteration", "x0", "y0", "angle", "x1", "y1", "epaisseur")
MatExemple$epaisseur <- MatExemple$epaisseur * 4
MatExemple$alpha <- 1
MatExemple$alpha[MatExemple$iteration > 11] <- .05
MatExemple$alpha[MatExemple$iteration > 5 & MatExemple$iteration <= 11] <- seq(1, .3, length.out = 7)[MatExemple$iteration[MatExemple$iteration > 5 & MatExemple$iteration <= 11] - 5]

Graphique <- ggplot() +
  with_blur(geom_segment(data = MatExemple, 
               aes(x = x0, y = y0, xend = x1, yend = y1, size = epaisseur, alpha = alpha), 
               color = "#dd090c", lineend = "round"), sigma = 5) +
  theme_void() +
  scale_size_identity() +
  scale_alpha_identity() +
  coord_equal() +
  theme(plot.background = element_rect(fill = "#030a4c"))
ggsave(Graphique, filename = paste0(Chemin, "/corail.png"), height = 10, width = 10)


plot(c(MatExemple$x0, MatExemple$x1), c(MatExemple$y0, MatExemple$y1), type = "n")
segments(MatExemple$x0, MatExemple$y0, MatExemple$x1, MatExemple$y1, col = MatExemple$iteration + 1)
