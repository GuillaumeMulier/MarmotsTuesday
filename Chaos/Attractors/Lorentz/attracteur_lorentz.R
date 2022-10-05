# ----------------------------------------------- #
# Script pour faire l'attracteur de Lorentz en 3D #
# ----------------------------------------------- #


# Packages ----

library(deSolve) # Equations différentielles
library(plotly) # Plot en 3D
library(tidyverse)
library(NatParksPalettes) # Palette de couleurs
library(htmlwidgets) # Sauvegarder l'html

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)


# ODE ----

# On code la fonction pour l'équation différentielle
LorentzAttractor <- function (t, coord, parms) {
  
  # D'abord les paramètres d'état
  X <- coord[1]
  Y <- coord[2]
  Z <- coord[3]
  Sigma <- parms[1]
  Rho <- parms[2]
  Beta <- parms[3]
  
  # Equations différentielles
  dXdt <- Sigma * (Y - X)
  dYdt <- X * (Rho - Z) - Y
  dZdt <- X * Y - Beta * Z
  
  # On combine le tout en un vecteur
  dCoorddt <- c(dXdt,dYdt,dZdt)
  
  # Résultat sous forme de liste
  return(list(dCoorddt))
  
}
Resultat <- ode(
  func = LorentzAttractor,
  y = c(X = 1.1, Y = 2, Z = 7),
  times = seq(0, 250, .001),
  parms = c(Sigma = 10, Rho = 28, Beta = 8 / 3)
)

# Graphique
Figure <- plot_ly(data = as.data.frame(Resultat), x = ~ X, y = ~ Y, z = ~ Z) %>% 
  add_paths(color = ~ time, colors = as.character(natparks.pals("Arches"))[c(1, 9, 7, 2, 8, 3)], alpha = I(.8)) %>%  
  hide_colorbar() %>%
  layout(
    scene = list(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      zaxis = list(visible = FALSE),
      bgcolor = "#fff696",
      camera = list(eye = list(x = 1, y = -.75, z = 1))
    )
  )
Figure

# Sauvegarder 
saveWidget(Figure, file = paste0(Chemin, "/papillon_marmotte.html"), background = "#fff696")
# Je n'ai pas réussi à enregistrer directement en png, j'ai fait un html très lourd puis j'ai enregistré en png par la suite à la main
