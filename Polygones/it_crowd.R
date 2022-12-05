# ---------------------------------------------------------------------- #
# Script pour faire la figure du générique the it crowd pour ma marmotte #
# ---------------------------------------------------------------------- #


# I/ Les functions ----

library(tidyverse)

# Interpoler linéairement entre 2 points
InterpLin <- function(Coord1, Coord2, Poids) Poids * Coord1 + (1 - Poids) * Coord2 

# Créer un polygone régulier
InitiatePolygon <- function(NCotes = 4, Rayon = 10, Couleur) {
  Angles <- seq(0, 2 * pi, length.out = NCotes + 1)
  DonneesPoly <- matrix(c(Rayon * cos(Angles), Rayon * sin(Angles)), ncol = 2)
  if (NCotes %% 2 == 0) {
    DonneesPoly <- DonneesPoly %*% matrix(c(cos(pi / NCotes), sin(pi / NCotes), -sin(pi / NCotes), cos(pi / NCotes)), ncol = 2)
  } else {
    DonneesPoly <- DonneesPoly %*% matrix(c(cos(- pi / (2 * NCotes)), sin(- pi / (2 * NCotes)), -sin(- pi / (2 * NCotes)), cos(- pi / (2 * NCotes))), ncol = 2)
  }
  DonneesPoly <- DonneesPoly %>%
    as_tibble() %>% 
    rename(xx_1 = V1, yy_1 = V2) %>% 
    mutate(xx_2 = lead(xx_1), yy_2 = lead(yy_1), 
           generation = 0, ligne = row_number()) %>% 
    filter(!is.na(xx_2)) %>% 
    mutate(couleur = Couleur)
  return(DonneesPoly)
}

# Faire le petit polygone
LittlePolygon <- function(DonneesPoly, Poids, Couleur, Generation) {
  InterpolationX <- InterpLin(DonneesPoly$xx_1, DonneesPoly$xx_2, Poids)
  InterpolationY <- InterpLin(DonneesPoly$yy_1, DonneesPoly$yy_2, Poids)
  DonneesPoly <- data.frame(
    xx_1 = InterpolationX,
    yy_1 = InterpolationY,
    xx_2 = c(InterpolationX[length(InterpolationX)], lag(InterpolationX)[-1]),
    yy_2 = c(InterpolationY[length(InterpolationY)], lag(InterpolationY)[-1])
  ) %>% mutate(generation = Generation, ligne = row_number(), couleur = Couleur)
  return(DonneesPoly)
}

# Afficher le dessin
DrawPolygon <- function(DonneesPoly) {
  Graphique <- ggplot(DonneesPoly, aes(xx_1, yy_1, group = generation, color = couleur, fill = couleur)) +
    geom_polygon() +
    coord_fixed(ratio = 1) +
    theme_void() +
    scale_color_identity() +
    scale_fill_identity() +
    theme(plot.background = element_rect(fill = "#082045", color = "#082045"),
          panel.background = element_rect(fill = "#082045", color = "#082045"))
  plot(Graphique)
  invisible(Graphique)
}

# Créer le tableau de données de la figure
# Soit 2 couleurs, soit une liste de 2 vecteurs de la longueur du nombre de côtés du polygone
MakeFigure <- function(NGen = 50, NCotes = 4, Rayon = 10, Couleurs, Poids) {
  
  # Gérer les couleurs
  if (is.list(Couleurs)) {
    # Pas encore implémenté car il faudra changer comment je génère le polygone
    # Couleurs <- map(seq_len(NCotes),
    #     function (Cote) {
    #       CouleurDepart <- as.numeric(col2rgb(Couleurs[[1]][[Cote]]))
    #       CouleurArrivee <- as.numeric(col2rgb(Couleurs[[2]][[Cote]]))
    #       InterpCouleurs <- seq(0, 1, length.out = NGen + 1)
    #       map_chr(seq_along(InterpCouleurs),
    #           function (poids_col) {
    #             rgb(red = InterpLin(CouleurDepart[1], CouleurArrivee[1], InterpCouleurs[poids_col]),
    #                 green = InterpLin(CouleurDepart[2], CouleurArrivee[2], InterpCouleurs[poids_col]),
    #                 blue = InterpLin(CouleurDepart[3], CouleurArrivee[3], InterpCouleurs[poids_col]),
    #                 maxColorValue = 255)
    #           })
    #     })
  } else {
    Couleurs <- rep(Couleurs, times = ceiling((NGen + 1) / length(Couleurs)))[seq_len(NGen + 1)]
    Couleurs <- map(seq_len(NCotes), ~ Couleurs)
  }
  
  # Initier le polygone
  Polygone <- InitiatePolygon(NCotes = NCotes, Rayon = Rayon, Couleur = map_chr(Couleurs, 1))
  
  # Itérer les plus petits polygones
  Polygone <- accumulate(seq_len(NGen),
                         function(x, y) {
                           LittlePolygon(
                             DonneesPoly = x,
                             Poids = Poids,
                             Couleur = map_chr(Couleurs, y + 1),
                             Generation = y
                           )
                         },
                         .init = Polygone) %>% 
    bind_rows()
  
  # Afficher le résultat
  return(DrawPolygon(Polygone))
  
}


# II/ Les figures ----

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)
ItCrowd <- MakeFigure(NGen = 200, NCotes = 4, Rayon = 20, Couleurs = c("#272ef2", "#d31ce8", "#c83908"), Poids = .05)
ggsave(ItCrowd, filename = paste0(Chemin, "/carre_tournant.png"), device = "png", height = 10, width = 10)
Nonagone <- MakeFigure(NGen = 200, NCotes = 9, Rayon = 20, Couleurs = c("#272ef2", "#dff008"), Poids = .1)
ggsave(Nonagone, filename = paste0(Chemin, "/noeuf_tournant.png"), device = "png", height = 10, width = 10)
