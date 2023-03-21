library(tidyverse)

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

InitiateGrid <- function(LimX = c(-2, .7),
                         LimY = c(-1.5, 1.5),
                         Pas = .01) {
  
  # Checks arguments
  if (length(LimX) != 2 || !is.numeric(LimX) || LimX[1] > LimX[2]) 
    stop("LimX should be a vector of numerics of length 2 for the bounds (min x; max x).", call. = FALSE)
  if (length(LimY) != 2 || !is.numeric(LimY) || LimY[1] > LimY[2]) 
    stop("LimY should be a vector of numerics of length 2 for the bounds (min y; max y).", call. = FALSE)
  
  # Table of coordinates for grid
  Coordonnees <- expand.grid(x2 = seq(LimX[1], LimX[2], Pas)[-1],
                             y2 = seq(LimY[1], LimY[2], Pas)[-1]) %>% 
    mutate(x1 = x2 - Pas,
           y1 = y2 - Pas,
           xc = (x1 + x2) / 2,
           yc = (y1 + y2) / 2,
           C = complex(length.out = length(.), real = xc, imaginary = yc),
           Zed = complex(real = 0, imaginary = 0),
           iter_seuil = NA_integer_)
  
  return(Coordonnees)
  
}


DrawMandelbrot <- function(Coordonnees,
                           Color = list(
                             Continu = FALSE,
                             Seuil = 2,
                             Colors = c("#052284", "#d79933")
                           )) {
  
  if (Color$Continu) {
    Graphique <- ggplot(Coordonnees %>% 
                          mutate(couleur = ifelse(is.na(iter_seuil), Mod(Zed), iter_seuil)), aes(xc, yc)) +
      geom_raster(aes(fill = couleur), show.legend = FALSE) +
      coord_equal(ratio = 1) +
      theme_void()
    plot(Graphique)
  } else {
    Coordonnees$couleur <- ifelse(Mod(Coordonnees$Zed) >= Color$Seuil, Color$Colors[1], Color$Colors[2])
    Graphique <- ggplot(Coordonnees, aes(xc, yc)) +
      geom_raster(aes(fill = couleur), show.legend = FALSE) +
      coord_equal(ratio = 1) +
      theme_void() +
      scale_fill_identity()
    plot(Graphique)
  }
  
  return(Graphique)
  
}

IterateMandel <- function(Coordonnees,
                          Iter,
                          Fonction = \(x) x ** 2,
                          Seuil = 2) {
  
  Coordonnees$Zed <- Fonction(Coordonnees$Zed) + Coordonnees$C
  Coordonnees$iter_seuil[is.na(Coordonnees$iter_seuil) & Mod(Coordonnees$Zed) >= Seuil] <- Iter
  return(Coordonnees)
  
}

DataMandel <- reduce(seq_len(50),
       IterateMandel, 
       Fonction = \(x) sin(x) ** 2, 
       .init = InitiateGrid(LimX = c(-1.5, 0.6), LimY = c(-1.35, 1.35), Pas = .0005))

Graphe1 <- DrawMandelbrot(DataMandel)
ggsave(Graphe1, filename = paste0(Chemin, "/sandbrot.png"),
       device = "png", height = 30, width = 30)
Graphe2 <- DrawMandelbrot(DataMandel, Color = list(Continu = TRUE))
ggsave(Graphe2, filename = paste0(Chemin, "/purplebrot.png"),
       device = "png", height = 30, width = 30)
