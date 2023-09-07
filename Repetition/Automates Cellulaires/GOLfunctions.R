# ------------------------------- #
# Conway's game of life functions #
# ------------------------------- #

# Packages ----

library(R6)
library(tidyverse)
library(animation)
library(progressr)

handlers(
  handler_progress(
    format = ":spin :current/:total :percent (:bar)",
    complete = "=",
    incomplete = " ",
    current = "]"
  )
)

# Classe principale ----

GOL <- R6Class(
  classname = "GOL",
  public = list(
    longueur = NA,
    largeur = NA,
    couleurs = list(),
    field = matrix(NA, nrow = 1, ncol = 1),
    data = data.frame(NA),
    initialize = function(Longueur, Largeur, Couleurs = c("#c80a68", "#063170"),
                          Indexes = NULL, Graine = 121221, Proba = .3) {
      self$longueur <- Longueur
      self$largeur <- Largeur
      self$couleurs <- Couleurs
      self$field <- matrix(0, ncol = Largeur, nrow = Longueur)
      if (is.null(Indexes)) {
        set.seed(Graine)
        on.exit(set.seed(NULL), add = TRUE)
        VecBinom <- rbinom(Longueur * Largeur, 1, Proba)
        self$field[VecBinom == 1] <- 1
      } else {
        if (!is.matrix(Indexes) || ncol(Indexes) != 2)
          stop("Indexes should be an x by 2 matrix.", call. = FALSE)
        self$field[Indexes] <- 1
      }
      self$data <- self$process_data(self$field)
    },
    print = function(...) {
      cat("Conway's game of life :\n")
      cat(paste0(self$longueur, " by ", self$largeur, " grid with ",
                 sum(self$field), " living cells.\n"))
    },
    update = function(...) {
      VecNeighbours <-
        self$field[, c(seq(2, self$largeur), 1)] + # Right neighbour
        self$field[, c(seq(2, self$largeur), 1)][c(self$longueur, seq(1, self$longueur - 1)), ] + # Upright neighbour
        self$field[, c(seq(2, self$largeur), 1)][c(seq(2, self$longueur), 1), ] + # Downright neighbour
        self$field[, c(self$largeur, seq(1, self$largeur - 1))] + # Left neighbour
        self$field[, c(self$largeur, seq(1, self$largeur - 1))][c(self$longueur, seq(1, self$longueur - 1)), ] + # Upleft neighbour
        self$field[, c(self$largeur, seq(1, self$largeur - 1))][c(seq(2, self$longueur), 1), ] + # Downleft neighbour
        self$field[c(seq(2, self$longueur), 1), ] + # Down neighbour
        self$field[c(self$longueur, seq(1, self$longueur - 1)), ] # Up neighbour
      self$field[self$field == 1 & (VecNeighbours <= 1 | VecNeighbours >= 4)] <- 0 # Solitude and overcrowding
      self$field[self$field == 0 & VecNeighbours == 3] <- 1 # Population
      self$data <- self$process_data(self$field)
    },
    process_data = function(...) {
      self$data <- self$field %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        pivot_longer(-1) %>%
        mutate(name = str_remove_all(name, "^V"),
               across(c(rowname, name), ~ as.numeric(.x)),
               rowname = self$longueur + 1 - rowname,
               value = as.character(value))
    },
    plot = function(...) {
      plot(ggplot(self$data, aes(name, rowname, fill = value)) +
             geom_raster() +
             scale_fill_manual(values = self$couleurs) +
             theme_void() +
             theme(legend.position = "none") +
             scale_x_continuous(expand = c(0, 0)) +
             scale_y_continuous(expand = c(0, 0)) +
             coord_fixed(ratio = 1))
    }
  )
)


# Create animation ----

## A ship
SpaceShip <- GOL$new(24, 24,
                     Indexes = matrix(c(4, 11, 4, 12, 4, 14,
                                        5, 8, 5, 9, 5, 11, 5, 13, 5, 14, 5, 16, 5, 17, 5, 18,
                                        6, 5, 6, 6, 6, 7, 6, 8, 6, 11, 6, 12, 6, 19,
                                        7, 4, 7, 9, 7, 13, 7, 17, 7, 18,
                                        8, 5, 8, 6),
                                      ncol = 2, byrow = TRUE))
NFrames <- 143
saveGIF(expr = {
  with_progress({
    PBarre <- progressor(steps = NFrames)
    SpaceShip$plot()
    for (i in seq_len(NFrames)){
      SpaceShip$update()
      PBarre()
      SpaceShip$plot()
    }
  })
},
movie.name = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/spaceship.gif"),
interval = .1,
ani.width = 480, ani.height = 480)


## A random trial
## A ship
RandomGOL <- GOL$new(300, 300, Proba = .45, Graine = 13081989)
NFrames <- 400
Duree <- 20
saveGIF(expr = {
  with_progress({
    PBarre <- progressor(steps = NFrames)
    RandomGOL$plot()
    for (i in seq_len(NFrames)){
      RandomGOL$update()
      PBarre()
      RandomGOL$plot()
    }
  })
},
movie.name = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/randomgol3.gif"),
interval = Duree / NFrames,
ani.width = 600, ani.height = 600)

