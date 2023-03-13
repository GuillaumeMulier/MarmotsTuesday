library(tidyverse)
library(progressr)

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

GenererPolyReg <- function(NPoints = 3, Rayon = 10) {
  Angles <- seq(0, 2 * pi, length.out = NPoints + 1)[-1]
  DonneesVertices <- matrix(c(Rayon * cos(Angles), Rayon * sin(Angles)), ncol = 2)
  if (NPoints %% 2 == 0) {
    DonneesVertices <- DonneesVertices %*% matrix(c(cos(pi / NPoints), sin(pi / NPoints), -sin(pi / NPoints), cos(pi / NPoints)), ncol = 2)
  } else if (NPoints == 3) {
    DonneesVertices <- DonneesVertices %*% matrix(c(cos(pi / (2 * NPoints)), sin(pi / (2 * NPoints)), -sin(pi / (2 * NPoints)), cos(pi / (2 * NPoints))), ncol = 2)
  } else {
    DonneesVertices <- DonneesVertices %*% matrix(c(cos(- pi / (2 * NPoints)), sin(- pi / (2 * NPoints)), -sin(- pi / (2 * NPoints)), cos(- pi / (2 * NPoints))), ncol = 2)
  }
  DonneesVertices <- mutate(as.data.frame(DonneesVertices), n_sommet = row_number(), .before = 1)
  names(DonneesVertices) <- c("n_sommet", "xx", "yy")
  return(DonneesVertices)
}

InterpLin <- function(Coord1, Coord2, Poids) Poids * Coord1 + (1 - Poids) * Coord2 

GetCoord <- function(.Data, Sommet) .Data %>% filter(n_sommet == Sommet) %>% select(xx, yy) %>% as.numeric()

InterColors <- function(Couleurs, Coefs) {
  DonneesCouleurs <- rowSums(as.data.frame(col2rgb(Couleurs) * Coefs)) / sum(Coefs)
  return(rgb(DonneesCouleurs[1], DonneesCouleurs[2], DonneesCouleurs[3], maxColorValue = 255))
}

ResumerPixels <- function(PointsADessiner, Rayon, NbPix, NCotes, Couleurs) {
  Pixels <- seq(-Rayon, Rayon, length.out = NbPix + 1)
  TableauTemp <- expand.grid(
    tempx = Pixels,
    tempy = Pixels
  )
  Tableau <- data.frame(
    xxmax = TableauTemp$tempx[TableauTemp$tempx != Pixels[1]],
    yymax = TableauTemp$tempy[TableauTemp$tempy != Pixels[1]],
    xxmin = TableauTemp$tempx[TableauTemp$tempx != Pixels[NbPix + 1]],
    yymin = TableauTemp$tempy[TableauTemp$tempy != Pixels[NbPix + 1]]
  ) %>%
    mutate(xx = (xxmin + xxmax) / 2, yy = (yymin + yymax) / 2) %>%
    mutate(alpha = NA_real_, color = NA_character_)
  handlers(handler_progress(format = "(:bar) :current/:total :percent"))
  with_progress({
    PBarre <- progressor(steps = nrow(Tableau))
    for (i in seq_len(nrow(Tableau))) {
      PBarre()
      Vec <- Tableau$xxmin[i] <= PointsADessiner$xx & Tableau$xxmax[i] >= PointsADessiner$xx &
                     Tableau$yymin[i] <= PointsADessiner$yy & Tableau$yymax[i] >= PointsADessiner$yy
      if (sum(Vec) == 0) next
      Tableau$alpha[i] <- sum(Vec)
      Effectifs <- factor(PointsADessiner[Vec, "coord"], levels = seq_len(NCotes)) %>% table()
      Tableau$color[i] <- InterColors(Couleurs, as.numeric(Effectifs))
    }
  })
  return(Tableau)
}


Rayon = 10

# Le pentagone non symétrique
PtDepart <- c(0, 0)
Poids <- .5
N <- 5
Gen <- 500000
Points <- GenererPolyReg(N)
PtPrec <- 0
PointsADessiner <- data.frame(xx = numeric(Gen), yy = numeric(Gen), coord = numeric(Gen))
handlers(handler_progress(format = "(:bar) :current/:total :percent"))
set.seed(121221)
with_progress({
  PBarre <- progressor(steps = Gen)
  for (i in seq_len(Gen)) {
    NumSommet <- sum(runif(1) <= seq_len(N) / N)
    while (NumSommet == (PtPrec %% N + 1)) NumSommet <- sum(runif(1) <= seq_len(N) / N)
    PtPrec <- NumSommet
    PointCible <- GetCoord(Points, NumSommet)
    Coordonnees <- InterpLin(PtDepart, PointCible, Poids)
    PointsADessiner[i, ] <- c(Coordonnees, NumSommet)
    PtDepart <- Coordonnees
    PBarre()
  }
})
save(PointsADessiner, file = paste0(Chemin, "/donnees_penta1bis.RData"))
load(paste0(Chemin, "/donnees_penta1.RData"))

PointsADessiner <- ResumerPixels(PointsADessiner, Rayon, 1000, N, c("#e51515", "#0fb914", "#1650dc", "#b7ba11", "#d71c44"))

Graphique <- ggplot(PointsADessiner, aes(xx, yy, alpha = alpha, fill = color)) +
  geom_raster(show.legend = FALSE) +
  theme_void() +
  coord_fixed(ratio = 1) +
  scale_fill_identity() +
  scale_alpha_continuous(range = c(.5, 1)) +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/pentagone1.png"), device = png, height = 25, width = 25, units = "cm")

PointsADessiner$r <- sqrt(PointsADessiner$xx ** 2 + PointsADessiner$yy ** 2)
PointsADessiner$theta <- atan2(PointsADessiner$yy, PointsADessiner$xx) + pi
Couleurs <- c("#7522D7", "#A40000", "#7522D7")
Couleurs <- map(Couleurs, ~ as.numeric(col2rgb(.x)))
PointsADessiner$poids <- PointsADessiner$theta / (2 * pi)
PointsADessiner$index <- as.numeric(PointsADessiner$poids < .5) + 1
PointsADessiner$poids <- ifelse(PointsADessiner$poids < .5, PointsADessiner$poids * 2, (PointsADessiner$poids - .5) * 2)
PointsADessiner$couleur <- map2_chr(PointsADessiner$index, PointsADessiner$poids, 
                                    function(x, y) {
                                      Coul <- InterpLin(Couleurs[[x]], Couleurs[[x + 1]], y)
                                      return(rgb(Coul[1], Coul[2], Coul[3], maxColorValue = 255))
                                    })

Graphique <- ggplot(PointsADessiner, aes(xx, yy, color = couleur)) +
  geom_point(alpha = .1, size = .2) +
  theme_void() +
  coord_fixed(ratio = 1) +
  scale_color_identity() +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/pentagone2.png"), device = png, height = 25, width = 25, units = "cm")


PtDepart <- c(0, 0)
Poids <- .5
N <- 6
Gen <- 750000
Points <- GenererPolyReg(N)
PtPrec <- 0
PointsADessiner <- data.frame(xx = numeric(Gen), yy = numeric(Gen), coord = numeric(Gen))
handlers(handler_progress(format = "(:bar) :current/:total :percent"))
set.seed(121221)
with_progress({
  PBarre <- progressor(steps = Gen)
  for (i in seq_len(Gen)) {
    NumSommet <- sum(runif(1) <= seq_len(N) / N)
    if (NumSommet == PtPrec) NumSommet <- NumSommet %% N + 1
    PtPrec <- NumSommet
    PointCible <- GetCoord(Points, NumSommet)
    Coordonnees <- InterpLin(PtDepart, PointCible, Poids)
    PointsADessiner[i, ] <- c(Coordonnees, NumSommet)
    PtDepart <- Coordonnees
    PBarre()
  }
})
save(PointsADessiner, file = paste0(Chemin, "/donnees_hexa1.RData"))
load(paste0(Chemin, "/donnees_hexa1.RData"))
Couleurs <- c("#0095ED", "#2EAC12", "#BFA22E", "#0095ED", "#2EAC12", "#BFA22E")
PointsADessiner$couleur <- Couleurs[PointsADessiner$coord]
Graphique <- ggplot(PointsADessiner[, ], aes(xx, yy)) +
  geom_point(alpha = .01, size = .15, color = "white") +
  theme_void() +
  coord_fixed(ratio = 1) +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/hexagone1.png"), device = png, height = 20, width = 20, units = "cm")
Graphique <- ggplot(PointsADessiner, aes(xx, yy, color = couleur)) +
  geom_point(alpha = .1, size = .15) +
  theme_void() +
  scale_color_identity() +
  coord_fixed(ratio = 1) +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/hexagone2.png"), device = png, height = 25, width = 25, units = "cm")


PtDepart <- c(0, 0)
Poids <- .5
N <- 5
Gen <- 1000000
Points <- GenererPolyReg(N)
PtPrec <- 0
PointsADessiner <- data.frame(xx = numeric(Gen), yy = numeric(Gen), coord = numeric(Gen))
handlers(handler_progress(format = "(:bar) :current/:total :percent"))
set.seed(121221)
with_progress({
  PBarre <- progressor(steps = Gen)
  for (i in seq_len(Gen)) {
    NumSommet <- sum(runif(1) <= seq_len(N) / N)
    if (NumSommet == PtPrec) NumSommet <- sum(runif(1) <= seq_len(N) / N)
    PtPrec <- NumSommet
    PointCible <- GetCoord(Points, NumSommet)
    Coordonnees <- InterpLin(PtDepart, PointCible, Poids)
    PointsADessiner[i, ] <- c(Coordonnees, NumSommet)
    PtDepart <- Coordonnees
    PBarre()
  }
})
save(PointsADessiner, file = paste0(Chemin, "/donnees_penta2.RData"))
load(paste0(Chemin, "/donnees_penta2.RData"))
Graphique <- ggplot(PointsADessiner, aes(xx, yy)) +
  geom_point(alpha = .2, size = .15, color = "white") +
  theme_void() +
  coord_fixed(ratio = 1) +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/pentagone3.png"), device = png, height = 40, width = 40, units = "cm")

Couleurs <- c("#2D61D2", "#B41D13", "#8410A5", "#225A18", "#0AB289")
PointsADessiner$couleur <- Couleurs[PointsADessiner$coord]
Graphique <- ggplot(PointsADessiner, aes(xx, yy, color = couleur)) +
  geom_point(alpha = .2, size = .15) +
  theme_void() +
  scale_color_identity() +
  coord_fixed(ratio = 1) +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/pentagone4.png"), device = png, height = 40, width = 40, units = "cm")



PtDepart <- c(0, 0)
Poids <- .5
N <- 10
Gen <- 1000000
Points <- GenererPolyReg(N)
PtPrec <- 0
PointsADessiner <- data.frame(xx = numeric(Gen), yy = numeric(Gen), coord = numeric(Gen))
handlers(handler_progress(format = "(:bar) :current/:total :percent"))
set.seed(121221)
with_progress({
  PBarre <- progressor(steps = Gen)
  for (i in seq_len(Gen)) {
    NumSommet <- sum(runif(1) <= seq_len(N) / N)
    if (NumSommet == PtPrec & NumSommet %% 2 == 0) NumSommet <- NumSommet %% N + 1
    PtPrec <- NumSommet
    PointCible <- GetCoord(Points, NumSommet)
    Coordonnees <- InterpLin(PtDepart, PointCible, Poids)
    PointsADessiner[i, ] <- c(Coordonnees, NumSommet)
    PtDepart <- Coordonnees
    PBarre()
  }
})
save(PointsADessiner, file = paste0(Chemin, "/donnees_deca1.RData"))
load(paste0(Chemin, "/donnees_deca1.RData"))
Couleurs <- c("#0095ED", "#2EAC12", "#BFA22E", "#0095ED", "#2EAC12", "#BFA22E")
PointsADessiner$couleur <- Couleurs[PointsADessiner$coord]
Graphique <- ggplot(PointsADessiner, aes(xx, yy)) +
  geom_point(alpha = .2, size = .15, show.legend = FALSE, color = "white") +
  theme_void() +
  # scale_color_identity() +
  coord_fixed(ratio = 1) +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/decagone3.png"), device = png, height = 40, width = 40, units = "cm")





Rayon <- 10
PtDepart <- c(0, 0)
Poids <- 2 / 3
N <- 3
Gen <- 1000000
Points <- GenererPolyReg(N)
PtPrec <- 0
PointsADessiner <- data.frame(xx = numeric(Gen), yy = numeric(Gen), coord = numeric(Gen))
handlers(handler_progress(format = "(:bar) :current/:total :percent"))
set.seed(121221)
with_progress({
  PBarre <- progressor(steps = Gen)
  for (i in seq_len(Gen)) {
    NumSommet <- sum(runif(1) <= seq_len(N) / N)
    PtPrec <- NumSommet
    PointCible <- GetCoord(Points, NumSommet)
    Coordonnees <- InterpLin(PtDepart, PointCible, Poids)
    PointsADessiner[i, ] <- c(Coordonnees, NumSommet)
    PtDepart <- Coordonnees
    PBarre()
  }
})
save(PointsADessiner, file = paste0(Chemin, "/donnees_tria1.RData"))
load(paste0(Chemin, "/donnees_tria1.RData"))
Couleurs <- c("#0095ED", "#2EAC12", "#BFA22E", "#0095ED", "#2EAC12", "#BFA22E")
PointsADessiner$couleur <- Couleurs[PointsADessiner$coord]
Graphique <- ggplot(PointsADessiner, aes(xx, yy)) +
  geom_point(alpha = .1, size = .1, color = "white") +
  theme_void() +
  coord_fixed(ratio = 1) +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/triangle1.png"), device = png, height = 35, width = 35, units = "cm")

PointsADessiner$r <- sqrt(PointsADessiner$xx ** 2 + PointsADessiner$yy ** 2)
PointsADessiner$theta <- atan2(PointsADessiner$yy, PointsADessiner$xx) + pi
Couleurs <- c("#79349B", "#F06637", "#1D2181", "#79349B")
Couleurs <- map(Couleurs, ~ as.numeric(col2rgb(.x)))
PointsADessiner$index <- as.numeric(PointsADessiner$r > (Rayon / 3)) + 1
PointsADessiner$poids <- ifelse(PointsADessiner$r <= (Rayon / 3), 
                                PointsADessiner$r / (Rayon / 3),
                                sqrt(((PointsADessiner$r - (Rayon / 3)) / (2 * Rayon / 3))))
PointsADessiner$couleur <- map2_chr(PointsADessiner$index, PointsADessiner$poids, 
                                    function(x, y) {
                                      Coul <- InterpLin(Couleurs[[2 * x - 1]], Couleurs[[2 * x]], y)
                                      return(rgb(Coul[1], Coul[2], Coul[3], maxColorValue = 255))
                                    })

Graphique <- ggplot(PointsADessiner, aes(xx, yy, color = couleur)) +
  geom_point(alpha = .1, size = .2) +
  theme_void() +
  coord_fixed(ratio = 1) +
  scale_color_identity() +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/triangle2.png"), device = png, height = 25, width = 25, units = "cm")





Rayon <- 10
PtDepart <- c(0, 0)
Poids <- .5
N <- 4
Gen <- 1000000
Points <- GenererPolyReg(N)
PtPrec <- 0
PointsADessiner <- data.frame(xx = numeric(Gen), yy = numeric(Gen), coord = numeric(Gen))
handlers(handler_progress(format = "(:bar) :current/:total :percent"))
set.seed(121221)
with_progress({
  PBarre <- progressor(steps = Gen)
  for (i in seq_len(Gen)) {
    NumSommet <- sum(runif(1) <= seq_len(N) / N)
    if (NumSommet == PtPrec & NumSommet %% 2 == 0) NumSommet <- NumSommet %% N + 1
    PtPrec <- NumSommet
    PointCible <- GetCoord(Points, NumSommet)
    Coordonnees <- InterpLin(PtDepart, PointCible, Poids)
    PointsADessiner[i, ] <- c(Coordonnees, NumSommet)
    PtDepart <- Coordonnees
    PBarre()
  }
})
save(PointsADessiner, file = paste0(Chemin, "/donnees_carre1.RData"))
load(paste0(Chemin, "/donnees_deca1.RData"))
Couleurs <- c("#0095ED", "#2EAC12", "#BFA22E", "#0095ED", "#2EAC12", "#BFA22E")
PointsADessiner$couleur <- Couleurs[PointsADessiner$coord]
Graphique <- ggplot(PointsADessiner, aes(xx, yy)) +
  geom_point(alpha = .2, size = .15, show.legend = FALSE, color = "white") +
  theme_void() +
  # scale_color_identity() +
  coord_fixed(ratio = 1) +
  theme(plot.background = element_rect(fill = "#051c41", color = "#051c41"))
ggsave(Graphique, filename = paste0(Chemin, "/decagone3.png"), device = png, height = 40, width = 40, units = "cm")
