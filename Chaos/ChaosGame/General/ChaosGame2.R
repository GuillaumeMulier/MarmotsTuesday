library(tidyverse)
library(Rcpp)

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

sourceCpp(paste0(Chemin, "/chaos_functions.cpp"))

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
  DonneesVertices <- mutate(as.data.frame(DonneesVertices), n_sommet = row_number(), across(c(V1, V2), ~ round(.x, 2)), .before = 1)
  names(DonneesVertices) <- c("n_sommet", "xx", "yy")
  return(DonneesVertices)
}


CarreReg <- GenererPolyReg(NPoints = 4)
Donnees <- CreerPoints(1000000,
                       0, 0, CarreReg[["xx"]], CarreReg[["yy"]],
                       c(1, 1.8, 1.8, 1), 4, 3)
MatDonnees <- Rasterize(Donnees, 500, 500, c(-7.5, 7.5), c(-7.5, 7.5))

png(filename = paste0(Chemin, "/maisons.png"), width = 500, height = 500, units = "px")
par(bg = "#0c0e3b", mai = c(0, 0, 0, 0))
image((MatDonnees + .75) ** .25, col = gray.colors(100), axes = FALSE)
dev.off()



library(grid)

CarreReg <- GenererPolyReg(NPoints = 4)
Donnees <- CreerPoints(1000000,
                       0, 0, CarreReg[["xx"]], CarreReg[["yy"]],
                       c(1, .5, .3, 1), 4, 2)
MatDonnees <- Rasterize(Donnees, 500, 500, c(-7.5, 7.5), c(-7.5, 7.5), c(255, 255, 66, 231), c(43, 149, 28, 255), c(241, 0, 255, 10))
MatCouleurs <- lapply(2:4, \(i) {
  Res <- MatDonnees[[i]] / MatDonnees[[1]]
  Res[MatDonnees[[1]] == 0] <- 0
  return(Res / 255)
}) %>% do.call(what = "rgb", arg = .)
dim(MatCouleurs) <- c(500, 500)

grid.newpage()
grid.raster(MatCouleurs)


png(filename = paste0(Chemin, "/maisons.png"), width = 500, height = 500, units = "px")
par(bg = "#0c0e3b", mai = c(0, 0, 0, 0))
image((MatDonnees + .75) ** .25, col = gray.colors(100), axes = FALSE)
dev.off()


# plot(Donnees$X, Donnees$Y, xlim = c(-7.5, 7.5), ylim = c(-7.5, 7.5),
   # col = "white", pch = 16, cex = .05, bty = "n", axes = FALSE)

# Expérimentations encore pas au point

Donnees <- CreerPoints(500000,
                       0, 0, CarreReg[["xx"]], CarreReg[["yy"]],
                       c(1, 1.8, 1.2, 1), 4, 2)
plot(Donnees$X, Donnees$Y, xlim = c(-7.5, 7.5), ylim = c(-7.5, 7.5),
     col = Donnees$P + 1, pch = 16, cex = .05, bty = "n", axes = FALSE)

Donnees <- CreerPoints(500000,
                       0, 0, CarreReg[["xx"]], CarreReg[["yy"]],
                       c(1, 1), 3, 1)
plot(Donnees$X, Donnees$Y, xlim = c(-7.5, 7.5), ylim = c(-7.5, 7.5),
     col = Donnees$P + 1, pch = 16, cex = .05, bty = "n", axes = FALSE)

Donnees <- CreerPoints(500000,
                       0, 0, CarreReg[["xx"]], CarreReg[["yy"]],
                       c(1.1, 1), 3, 1)
plot(Donnees$X, Donnees$Y, xlim = c(-7.5, 7.5), ylim = c(-7.5, 7.5),
     col = Donnees$P + 1, pch = 16, cex = .05, bty = "n", axes = FALSE)

HeptaReg <- GenererPolyReg(NPoints = 7)
Donnees <- CreerPoints(500000,
                       0, 0, HeptaReg[["xx"]], HeptaReg[["yy"]],
                       c(1, 1.25), 4, 5)
plot(Donnees$X, Donnees$Y, xlim = c(-10, 10), ylim = c(-10, 10),
     col = Donnees$P + 1, pch = 16, cex = .05, bty = "n", axes = FALSE)

Donnees <- CreerPoints(500000,
                       0, 0, c(HeptaReg[["xx"]], 0), c(HeptaReg[["yy"]], 0),
                       c(1, 2), 2, 2)
plot(Donnees$X, Donnees$Y, xlim = c(-10, 10), ylim = c(-10, 10),
     col = Donnees$P + 1, pch = 16, cex = .05, bty = "n", axes = FALSE)

Donnees <- CreerPoints(500000,
                       0, 0, c(HeptaReg[["xx"]], 0), c(HeptaReg[["yy"]], 0),
                       c(1, 2), 4, 8)
plot(Donnees$X, Donnees$Y, xlim = c(-10, 10), ylim = c(-10, 10),
     col = Donnees$P + 1, pch = 16, cex = .05, bty = "n", axes = FALSE)

