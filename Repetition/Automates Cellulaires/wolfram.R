# ------------------------------- #
# Automates cellulaire de Wolfram #
# ------------------------------- #


# Packages ----

library(tidyverse)
library(data.table)
library(sysfonts)
library(progressr)
library(animation)
library(showtext)

font_add_google("Lato", "lato")
showtext_auto()


# Helper functions ----

NbToBin <- function(Nb) as.numeric(rev(str_sub(as.character(rawToBits(as.raw(Nb))), 2)))

BinToNb <- function(Nb) sum(2 ** (7:0) * Nb)

TransformerCell <- function(.Vecteur, Ligne, Regles) {
  .Vecteur <- str_c(c(0, .Vecteur, 0), collapse = "")
  Fins <- seq(3, str_length(.Vecteur), 1)
  Debuts <- Fins - 2
  Index <- map2_chr(Debuts, Fins, \(Deb, Fin) str_sub(.Vecteur, Deb, Fin))
  return(c(0, Regles[Index], 0))
}

MakeAutomate <- function(Numero, Longueur) {
  Regles <- setNames(NbToBin(Numero), c("000", "001", "010", "011", "100", "101", "110", "111"))
  Cellules <- accumulate(seq_len(Longueur), TransformerCell, Regles = Regles, .init = c(0, 1, 0))
  Donnees <- map_dfr(seq_along(Cellules), \(Ligne) setNames(c(rep(0, Longueur + 1 - Ligne), Cellules[[Ligne]], rep(0, Longueur + 1 - Ligne)), seq(1, 2 * Longueur + 3))) %>% 
    rownames_to_column(var = "ligne") %>% 
    pivot_longer(cols = -1, names_to = "colonnes") %>% 
    mutate(across(c(ligne, colonnes), ~ -as.numeric(.x)))
  Graphe <- ggplot(Donnees, aes(colonnes, ligne, fill = factor(value))) +
    geom_tile(show.legend = FALSE) +
    scale_fill_manual(values = c("#5d035d", "#02e113")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#5d035d", color = "transparent"),
          panel.background = element_rect(fill = "#5d035d", color = "transparent"),
          plot.title = element_text(face = "bold", size = 20, color = "white", family = "lato")) +
    coord_fixed(ratio = 1.5) +
    labs(title = paste0("Règle n° ", Numero))
  return(list(Graphe, Donnees))
}

SaveAutomata <- function(Numero, Longueur) {
  Graphe <- MakeAutomate(Numero, Longueur)
  ggsave(Graphe[[1]], filename = paste0("C:/Users/DRY12/Documents/deborahnana/wolfram/wolfram", Numero, ".png"),
         device = "png", height = 16, width = 16)
  PBarre()
  invisible(NULL)
}

GrapheFrame <- function(Frame, TableauAutomate) {
  Graphe <- TableauAutomate %>%
    filter(ligne >= -Frame, between(colonnes, -1 * (Longueur + 2 + Frame), -1 * (Longueur + 2 - Frame))) %>% 
    ggplot(aes(colonnes, ligne, fill = couleur)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_identity() +
    theme_void() +
    theme(plot.background = element_rect(fill = "#5d035d", color = "transparent"),
          panel.background = element_rect(fill = "#5d035d", color = "transparent")) +
    coord_fixed(ratio = 1.5)
  return(Graphe)
}


# Explorer les différentes configurations ----

handlers(handler_progress(format = "[:bar] :percent : :current/:total", 
                          complete = "+", current = ">", incomplete = " "))
with_progress({
  PBarre <- progressor(256)
  walk(seq(0, 255), SaveAutomata, Longueur = 750)
})


# Réaliser le gif ----

## On peut aussi changer le numéro
## Diminuer Longueur pour réduire le temps de calcul, car c'est de plus en plus long avec les itérations
Longueur <- 1000
JoliAuto <- MakeAutomate(233, Longueur)
PalletteCouleurs <- colorRampPalette(c("#1ff905", "#cf1e05", "#1823a5"), interpolate = "spline")
JoliAuto[[2]]$couleur <- "#5d035d"
JoliAuto[[2]]$couleur[JoliAuto[[2]]$value != 0] <- PalletteCouleurs(Longueur)[-JoliAuto[[2]]$ligne[JoliAuto[[2]]$value != 0]]
Graphe <- ggplot(JoliAuto[[2]], aes(colonnes, ligne, fill = couleur)) +
  geom_tile(show.legend = FALSE) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "#5d035d", color = "transparent"),
        panel.background = element_rect(fill = "#5d035d", color = "transparent"),
        plot.title = element_text(face = "bold", size = 20, color = "white", family = "lato")) +
  coord_fixed(ratio = 1.5)
with_progress({
  saveGIF(expr = {
    PBarre <- progressor((Longueur - 1) / 3)
    for (i in seq(1, Longueur, 3)) {
      Graphe <- GrapheFrame(i, JoliAuto[[2]])
      PBarre()
      plot(Graphe)
    }
  },
  movie.name = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "automata1.gif"),
  interval = .025, ani.width = 800, ani.height = 800, nmax = 1000)
})
