library(tidyverse)
library(ggforce)

NbSegments <- 5
Largeur <- 6
Hauteur <- 2
PropInv <- .3
PropInvY <- .9

set.seed(121221)
xx <- sort(runif(NbSegments * 3 + 1, 0, Largeur))
IndXx <- sample(seq_along(xx), size = floor((NbSegments * 3 + 1) * PropInv), replace = FALSE)
xx[sort(IndXx)] <- xx[IndXx]
yy <- sort(rnorm(NbSegments * 3 + 1, 0, Hauteur / 2))
IndYy <- sample(seq_along(yy), size = floor((NbSegments * 3 + 1) * PropInvY), replace = FALSE)
yy[sort(IndYy)] <- yy[IndYy]
Repet <- c(1, rep(c(1, 1, 2), NbSegments - 1), 1, 1, 1)
Donnees <- data.frame(
  x = rep(xx, Repet),
  y = rep(yy, Repet),
  groupe = rep(seq_len(NbSegments), each = 4)
) %>% mutate(taille = ((1 + (groupe > 2) & (groupe < 4)) / 3) ** .25)
(Graphe <- ggplot(Donnees, aes(x, y, group = groupe, size = taille)) + 
  # geom_bezier(show.legend = FALSE, color = "white") + 
  geom_bspline(show.legend = FALSE, color = "white") +
  theme_void() +
  coord_fixed() +
  scale_size_identity() +
  theme(panel.background = element_rect(fill = "#040131")))
ggsave(Graphe, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/signature_marmotte.png"), device = "png", height = 4, width = 6)

set.seed(121221)
xx <- mtcars$mpg[-1]
xx <- sort(2.5 * (xx - min(xx)) / (max(xx) - min(xx)))
IndXx <- sample(seq_along(xx), size = floor(31 * PropInv), replace = FALSE)
xx[sort(IndXx)] <- xx[IndXx]
yy <- mtcars$disp[-1]
yy <- (yy - min(yy)) / (max(yy) - min(yy))
Repet <- c(1, rep(c(1, 1, 2), 9), 1, 1, 1)
Donnees <- data.frame(
  x = rep(xx, Repet),
  y = rep(yy, Repet),
  groupe = rep(seq_len(NbSegments), each = 4)
) %>% mutate(taille = (((groupe > 3) + (groupe < 7)) / 3) ** .25)
ggplot(Donnees, aes(x, y, group = groupe, size = taille)) + 
  # geom_bezier(show.legend = FALSE, color = "white") + 
  geom_bspline(show.legend = FALSE, color = "white") +
  theme_void() +
  coord_fixed() +
  scale_size_identity() +
  theme(panel.background = element_rect(fill = "#040131"))

