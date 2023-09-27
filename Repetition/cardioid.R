library(data.table)
library(ggplot2)
library(ggforce)
library(ambient)
library(patchwork)

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

CoordPoints <- function(Rayon, Angle) {
  data.frame("x" = Rayon * cos(Angle), "y" = Rayon * sin(Angle))
}

SplitLines <- function(x0, y0, x1, y1, NSeg) {
  Xs <- seq(x0, x1, length.out = NSeg + 1)
  Ys <- seq(y0, y1, length.out = NSeg + 1)
  return(data.frame(x = Xs[-(NSeg + 1)], y = Ys[-(NSeg + 1)], xend = Xs[-1], yend = Ys[-1]))
}

GenererPins <- function(Rayon, NPins) {
  Tableau <- data.table(num = seq_len(NPins), rayon = Rayon, angle = seq(0, 2 * pi, length.out = NPins + 1)[-(NPins + 1)])
  Tableau[, coord := lapply(num, \(x) CoordPoints(rayon[[x]], angle[[x]]))]
  Tableau <- Tableau[, rbindlist(coord), .(num, angle)]
  return(Tableau)
}

FinSegment <- function(Tab, Decallage) {
  NPins <- nrow(Tab)
  VecFins <- (2 * seq_len(NPins)) %% NPins
  VecFins[VecFins == 0] <- NPins
  Tab[, xend := unlist(lapply(VecFins, \(index) x[[index]]))]
  Tab[, yend := unlist(lapply(VecFins, \(index) y[[index]]))]
  Tab[, coord := lapply(num, \(index) SplitLines(x[[index]], y[[index]], xend[[index]], yend[[index]], 50))]
  Tab <- Tab[, rbindlist(coord), .(num, angle)]
  return(Tab)
}

Tab <- GenererPins(10, 400)
Tab <- FinSegment(Tab, 2)
Tab[, dist_centre := ((x + xend) / 2) ** 2 + ((y + yend) / 2) ** 2]
Tab[, xnorm := normalise(x)]
Tab[, ynorm := normalise(y)]
Tab[, xendnorm := normalise(xend)]
Tab[, yendnorm := normalise(yend)]

Cercle <- data.table(x = 10 * cos(seq(0, 2 * pi, length.out = 401)), y = 10 * sin(seq(0, 2 * pi, length.out = 401)))
Cercle[, xnorm := normalise(x)]
Cercle[, ynorm := normalise(y)]
Graphique1 <- ggplot(Tab) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = dist_centre), show.legend = FALSE) +
  geom_path(data = Cercle, aes(x, y), inherit.aes = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#010d31")) +
  coord_fixed(ratio = 1) +
  scale_color_gradient(low = "#ff6c00", high = "#39057c")

set.seed(121221)
DimPerlin <- 201
MatPerlin <- noise_perlin(c(DimPerlin, DimPerlin), octave = 8, lacunarity = 8, frequency = .05)

Cercle[, num := seq_len(.N)]
Cercle[, x2 := unlist(lapply(num, \(index) {
  x[index] + MatPerlin[1 + floor(xnorm[index] * (DimPerlin - 1)), 1 + floor(ynorm[index] * (DimPerlin - 1))]
}))]
Cercle[, y2 := unlist(lapply(num, \(index) {
  y[index] + MatPerlin[1 + floor(xnorm[index] * (DimPerlin - 1)), 1 + floor(ynorm[index] * (DimPerlin - 1))]
}))]
NumLignes <- seq_len(nrow(Tab))
Tab[, x2 := unlist(lapply(NumLignes, \(index) {
  x[index] + MatPerlin[1 + floor(xnorm[index] * (DimPerlin - 1)), 1 + floor(ynorm[index] * (DimPerlin - 1))]
}))]
Tab[, y2 := unlist(lapply(NumLignes, \(index) {
  y[index] + MatPerlin[1 + floor(xnorm[index] * (DimPerlin - 1)), 1 + floor(ynorm[index] * (DimPerlin - 1))]
}))]
Tab[, xend2 := unlist(lapply(NumLignes, \(index) {
  xend[index] + MatPerlin[1 + floor(xendnorm[index] * (DimPerlin - 1)), 1 + floor(yendnorm[index] * (DimPerlin - 1))]
}))]
Tab[, yend2 := unlist(lapply(NumLignes, \(index) {
  yend[index] + MatPerlin[1 + floor(xendnorm[index] * (DimPerlin - 1)), 1 + floor(yendnorm[index] * (DimPerlin - 1))]
}))]
Tab[, dist_centre2 := ((x2 + xend2) / 2) ** 2 + ((y2 + yend2) / 2) ** 2]

Graphique2 <- ggplot(Tab) +
  geom_segment(aes(x = x2, y = y2, xend = xend2, yend = yend2, color = dist_centre2), show.legend = FALSE) +
  geom_path(data = Cercle, aes(x2, y2), inherit.aes = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#010d31")) +
  coord_fixed(ratio = 1) +
  scale_color_gradient(low = "#ff6c00", high = "#39057c")
Graphique <- Graphique1 | Graphique2
ggsave(Graphique, filename = paste0(Chemin, "/perlin_cardioid.png"), device = "png", height = 10, width = 14)
