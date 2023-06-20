library(deldir)
library(sf)
library(animation)
library(progressr)
library(ggplot2)

# Progress bar
handlers(handler_progress(
  format   = ":spin :current/:total [:bar] :percent",
  width    = 100,
  complete = "-",
  current = ">"
))

# Polygone des frontières
Angles <- seq(0, 2 * pi, length.out = 9)
Rayon <- 5
Polygone <- st_polygon(list(matrix(round(c(Rayon * cos(Angles), y = Rayon * sin(Angles)), 2), ncol = 2)))
Polygone <- st_sfc(Polygone)

# Couleur selon la distance avec le point (3, -1)
CouleursPoints <- function(dist) {
  Pallette <- colorRamp(c("#e8a90e", "#115c9e"))(dist)
  return(rgb(Pallette[1, 1], Pallette[1, 2], Pallette[1, 3], maxColorValue = 255))
}
CouleursPolygones <- function(dist) {
  Pallette <- colorRamp(c("#efe222", "#0ee899"))(dist)
  return(rgb(Pallette[1, 1], Pallette[1, 2], Pallette[1, 3], maxColorValue = 255))
}
# set.seed(121221)
# CoulPointsVec <- setNames(sample(c("#043271", "#055016", "#340550"), size = 30, replace = TRUE), 1:30)
# CoulPolyVec <- setNames(sample(c("#07a2af", "#07af2d", "#913cc3"), size = 30, replace = TRUE), 1:30)
# CouleursPoints <- function(id) CoulPointsVec[id]
# CouleursPolygones <- function(id) CoulPolyVec[id]

# Création des 30 points et de leurs vélocités
set.seed(121221)
Points <- data.frame(
  id = 1:30,
  x = runif(30, -5, 5),
  y = runif(30, -5, 5),
  velx = rnorm(30, 0, .1) / 10,
  vely = rnorm(30, 0, .1) / 5,
  sensx = 1, sensy = 1
)
Lumiere <- c(0, 0)

FaireTable <- function(BasePoints) {
  
  # Calculer diagramme de Voronoi
  tesselation <- deldir(BasePoints$x, BasePoints$y, rw = c(-5, 5, -5, 5))
  voronoi <- tile.list(tesselation)
  
  # Ajouter les polygones dans le tableau
  BasePoints$polygones <- lapply(voronoi, \(x) st_polygon(list(matrix(c(x$x, x$x[1], x$y, x$y[1]), ncol = 2))))
  
  # Transformation en simple feature et intersection avec le polygone défini
  BasePoints <- st_sf(BasePoints)
  BasePoints <- st_intersection(BasePoints, Polygone)
  Indices <- apply(BasePoints, 1, \(ligne) st_point(c(ligne$x, ligne$y)), simplify = FALSE)
  BasePoints$afficher <- unlist(lapply(Indices, \(x) st_intersects(x, Polygone, sparse = FALSE)))
  BasePoints$col_point <- vapply(sqrt((BasePoints$x - Lumiere[1]) ** 2 + (BasePoints$y - Lumiere[2]) ** 2) / DistMax, CouleursPoints, character(1))
  BasePoints$col_poly <- vapply(sqrt((BasePoints$x - Lumiere[1]) ** 2 + (BasePoints$y - Lumiere[2]) ** 2) / DistMax, CouleursPolygones, character(1))
  # BasePoints$col_point <- vapply(BasePoints$id, CouleursPoints, character(1))
  # BasePoints$col_poly <- vapply(BasePoints$id, CouleursPolygones, character(1))
  
  Graphe <- ggplot(BasePoints) +
    geom_sf(aes(geometry = polygones, fill = col_poly), color = "white") +
    geom_point(data = BasePoints[BasePoints$afficher, ], aes(x, y, color = col_point)) +
    scale_color_identity() +
    scale_fill_identity() +
    theme_void() +
    theme(plot.background = element_rect(fill = "#041c32")) +
    coord_sf(xlim = c(-5, 5), ylim = c(-5, 5))
  return(Graphe)
  
}

# Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)
Chemin <- dirname(sys.frame(1)$ofile)
with_progress({
  saveGIF(
    expr = {
      PBarre <- progressor(steps = 500)
      for (i in 1:500) {
        if (i != 1) {
          if (any(abs(Points$x + Points$sensx * Points$velx) > 5)) 
            Points$sensx[abs(Points$x + Points$sensx * Points$velx) > 5] <- -1 * Points$sensx[abs(Points$x + Points$sensx * Points$velx) > 5]
          if (any(abs(Points$y + Points$sensy * Points$vely) > 5)) 
            Points$sensy[abs(Points$y + Points$sensy * Points$vely) > 5] <- -1 * Points$sensy[abs(Points$y + Points$sensy * Points$vely) > 5]
          Points$x <- Points$x + Points$sensx * Points$velx
          Points$y <- Points$y + Points$sensy * Points$vely
        }
        DistMax <- max(sqrt((c(5, 5, -5, -5) - Lumiere[1]) ** 2 + (c(5, -5, -5, 5) - Lumiere[2]) ** 2))
        Graphique <- FaireTable(Points)
        Lumiere <- c(Lumiere[1] + rnorm(1, 0, .1), Lumiere[2] + runif(1, -.05, .05))
        PBarre()
        plot(Graphique)
      }
    },
    movie.name = paste0(Chemin, "/gif_voronoi1.gif"),
    interval = .02,
    ani.width = 600,
    ani.height = 600
  )
})
