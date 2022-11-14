library(tidyverse)

# Paramètres à changer pour la figure
Increment <- 88 # angle entre chaque trait
NPoints <- 500 # Nombre de points
Couleurs <- c("#e23bff", "#ff0d00", "#308aff") # Les 3 couleurs pour le tracé
Longueur <- 1 # Taille du segment
Augmentation <- 1.5

# Pour manipuler les couleurs
Chiffres <- c(0:9, LETTERS[1:6])
HexaToBin <- function(col) {
  Hexa <- str_to_upper(col)
  Hexa <- 16 * (match(str_sub(Hexa, 1, 1), Chiffres) - 1) + (match(str_sub(Hexa, 2, 2), Chiffres) - 1)
  return(Hexa)
}
BinToHexa <- function(col) paste0(Chiffres[1 + col %/% 16], Chiffres[1 + col %% 16])

# Construction du tableau de données : on fait un segment de plus en plus grand avec un angle fixe entre chaque
Angle <- 0
if (NPoints %% 2 == 1) NPoints <- NPoints - 1
RGBCouleurs <- map(Couleurs, str_sub, c(2, 4, 6), c(3, 5, 7)) %>% 
  map(~ map_dbl(.x, HexaToBin))
VecteurCouleurs <- c(
  map_chr(seq_len(NPoints / 2), ~ rgb(RGBCouleurs[[1]][[1]] + (.x * 2 / NPoints) * (RGBCouleurs[[2]][1] - RGBCouleurs[[1]][[1]]),
                                      RGBCouleurs[[1]][[2]] + (.x * 2 / NPoints) * (RGBCouleurs[[2]][2] - RGBCouleurs[[1]][[2]]),
                                      RGBCouleurs[[1]][[3]] + (.x * 2 / NPoints) * (RGBCouleurs[[2]][3] - RGBCouleurs[[1]][[3]]),
                                      maxColorValue = 255)),
  map_chr(seq_len(NPoints / 2), ~ rgb(RGBCouleurs[[2]][[1]] + (.x * 2 / NPoints) * (RGBCouleurs[[3]][1] - RGBCouleurs[[2]][[1]]),
                                      RGBCouleurs[[2]][[2]] + (.x * 2 / NPoints) * (RGBCouleurs[[3]][2] - RGBCouleurs[[2]][[2]]),
                                      RGBCouleurs[[2]][[3]] + (.x * 2 / NPoints) * (RGBCouleurs[[3]][3] - RGBCouleurs[[2]][[3]]),
                                      maxColorValue = 255))
)  
SegmentsATracer <- tibble(num_pt = seq_len(NPoints),
                          x = NA_real_,
                          xend = NA_real_,
                          y = NA_real_,
                          yend = NA_real_,
                          couleur = NA_character_)
x0 <- 0
y0 <- 0
for (i in seq_len(NPoints)) {
  x <- x0 + Longueur * i ^ Augmentation * cos(Angle * pi / 180)
  y <- y0 + Longueur * i ^ Augmentation * sin(Angle * pi / 180)
  SegmentsATracer[i, 2:5] <- tibble_row(x0, x, y0, y)
  SegmentsATracer[i, 6] <- VecteurCouleurs[i]
  x0 <- x
  y0 <- y
  Angle <- Angle + 89
}

# Tracé et sauvegarde
(BeauGraphe <- ggplot(SegmentsATracer, aes(x = x, y = y, xend = xend, yend = yend, color = couleur)) +
    geom_segment() +
    theme_void() +
    scale_color_identity() +
    coord_fixed(ratio = 1) +
    theme(plot.background = element_rect(fill = "#050333"),
          panel.background = element_rect(fill = "#050333")))
ggsave(BeauGraphe, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/carre_tournant.png"),
       device = "png", height = 8, width = 8)
