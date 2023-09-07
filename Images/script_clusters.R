library(imager)
library(data.table)
library(ggplot2)
library(patchwork)

Marmottes <- load.image("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/img_marmottes.jpg")
Marmottes <- resize(Marmottes, 750, 500)

set.seed(121221)

MarmottesDf <- as.data.frame(Marmottes)
setDT(MarmottesDf)
MarmottesDf[, cc := factor(cc, levels = 1:3, labels = c("red", "green", "blue"))]
MarmottesDf <- dcast(MarmottesDf, x + y ~ cc, value.var = "value")
MarmottesDf[, ":="(x2 = x / 750 / 4, y2 = y / 500 / 4)]
MarmottesDf[, couleur := rgb(red, green, blue)]
KK <- kmeans(MarmottesDf[, 3:5], centers = 4, iter.max = 20)
MarmottesDf[, cluster1 := KK$cluster]
MarmottesDf[, couleur1 := c("#000000", "#9c099c", "#ffffff", "#505ed3")[cluster1]]
KK2 <- kmeans(MarmottesDf[, 3:7], centers = 15, iter.max = 20)
Centres <- as.data.frame(KK2$centers)
setDT(Centres)
Centres[, couleur := rgb(red, green, blue)]
Centres[, cluster2 := (1:.N)]
MarmottesDf[, cluster2 := KK2$cluster]
MarmottesDf[Centres, on = "cluster2", couleur2 := i.couleur]
KK3 <- kmeans(MarmottesDf[, 3:7], centers = 10, iter.max = 20)
Centres <- as.data.frame(KK3$centers)
setDT(Centres)
Centres[, ':='(red = red ** 2.2, green = green ** 2.2, blue = blue ** 2.2)]
Centres[, ':='(red = pmin(.65, red / max(red)), green = pmin(.65, green / max(green)), blue = pmin(.65, blue / max(blue)))]
Centres[, couleur := rgb(.5 * red, green, 1.2 * blue)]
Centres[, couleur2 := rgb(green, blue, pmin(1, red * 1.6))]
Centres[, cluster3 := (1:.N)]
MarmottesDf[, cluster3 := KK3$cluster]
MarmottesDf[Centres, on = "cluster3", ':='(couleur3 = i.couleur, couleur4 = i.couleur2)]
KK4 <- kmeans(MarmottesDf[, 3:5], centers = 3, iter.max = 20)
Centres <- as.data.frame(KK4$centers)
setDT(Centres)
Centres[, couleur := rgb(red, green, blue)]
Centres[, cluster4 := (1:.N)]
MarmottesDf[, cluster4 := KK4$cluster]
MarmottesDf[Centres, on = "cluster4", couleur5 := i.couleur]

(Graphe1 <- ggplot(MarmottesDf, aes(x, y)) +
    geom_raster(aes(fill = couleur)) +
    scale_fill_identity() +
    scale_y_reverse() +
    theme_void() +
    coord_fixed(ratio = 1) +
    theme(plot.margin = unit(c(-1, -1, -1, -1), "cm")))
(Graphe2 <- ggplot(MarmottesDf, aes(x, y)) +
    geom_raster(aes(fill = couleur1)) +
    scale_fill_identity() +
    scale_y_reverse() +
    theme_void() +
    coord_fixed(ratio = 1) +
    theme(plot.margin = unit(c(-1, -1, -1, -1), "cm")))
(Graphe3 <- ggplot(MarmottesDf, aes(x, y)) +
    geom_raster(aes(fill = couleur2)) +
    scale_fill_identity() +
    scale_y_reverse() +
    theme_void() +
    coord_fixed(ratio = 1) +
    theme(plot.margin = unit(c(-1, -1, -1, -1), "cm")))
(Graphe4 <- ggplot(MarmottesDf, aes(x, y)) +
    geom_raster(aes(fill = couleur3)) +
    scale_fill_identity() +
    scale_y_reverse() +
    theme_void() +
    coord_fixed(ratio = 1) +
    theme(plot.margin = unit(c(-1, -1, -1, -1), "cm")))
(Graphe5 <- ggplot(MarmottesDf, aes(x, y)) +
    geom_raster(aes(fill = couleur4)) +
    scale_fill_identity() +
    scale_y_reverse() +
    theme_void() +
    coord_fixed(ratio = 1) +
    theme(plot.margin = unit(c(-1, -1, -1, -1), "cm")))
(Graphe6 <- ggplot(MarmottesDf, aes(x, y)) +
    geom_raster(aes(fill = couleur5)) +
    scale_fill_identity() +
    scale_y_reverse() +
    theme_void() +
    coord_fixed(ratio = 1) +
    theme(plot.margin = unit(c(-1, -1, -1, -1), "cm")))

PlotTot <- (Graphe2 + Graphe3 + Graphe6 + Graphe5) +
  plot_layout(ncol = 2) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "lightgrey")))
ggsave(PlotTot, filename = "C:/Users/gmulier/Documents/Github/MarmotsTuesday/Images/MarmottesCluster.png",
       device = "png", height = 8, width = 12)
