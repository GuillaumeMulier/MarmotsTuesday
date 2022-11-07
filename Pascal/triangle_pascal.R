library(tidyverse)
library(NatParksPalettes)
library(patchwork)

DonneePascal <- function(Taille, Modulo = 2) {
  Carre <- data.frame(
    x = c(0, 0, 1, 1),
    y = c(0, 1, 1, 0)
  )
  ListePascal <- accumulate(seq_len(Taille), function(x, y) c(0, x) + c(x, 0), .init = 1)
  ListeParite <- accumulate(seq_len(Taille), function(x, y) (c(0, x) + c(x, 0)) %% Modulo, .init = 1)
  NbColonnes <- length(ListePascal[[length(ListePascal)]])
  TabPascal <- map_dfr(rev(seq_along(ListePascal[[length(ListePascal)]])),
                       function(Ligne) {
                         map_dfr(seq_along(ListePascal[[Ligne]]),
                                 function(Chiffre) {
                                   Carre %>% 
                                     mutate(y = Ligne + y,
                                            x = x + Chiffre - 1 + .5 * (6 - Ligne),
                                            carre = paste0("Carré", Ligne, "-", Chiffre),
                                            label = ListePascal[[Ligne]][Chiffre],
                                            modulo = ListeParite[[Ligne]][Chiffre])
                                 })
                       })
  return(TabPascal)
}

TabPascal <- DonneePascal(15)
(Graphe1 <- ggplot(TabPascal, aes(x, y, group = carre, fill = label)) + 
    geom_polygon(color = "white", show.legend = FALSE) +
    geom_text(data = TabPascal %>% group_by(carre) %>% summarise(x = mean(x), y = mean(y), label = mean(label)),
              mapping = aes(x = x, y = y, label = label)) +
    scale_fill_natparks_c(name = "Arches") +
    scale_y_reverse() +
    coord_fixed(ratio = 1) +
    theme_void())

TabPascal <- DonneePascal(127, 4)  
(Graphe2 <- ggplot(TabPascal, aes(x, y, group = carre, fill = modulo)) + 
    geom_polygon(color = "white", show.legend = FALSE) +
    scale_fill_natparks_c(name = "Arches") +
    scale_y_reverse() +
    coord_fixed(ratio = 1) +
    theme_void())

(GrapheTotal <- (Graphe1 | Graphe2) +
  plot_annotation(title = "Représentation du triangle de Pascal",
                  caption = "A droite, coloration selon le reste de la division euclidienne par 4."))
ggsave(GrapheTotal, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/MarmottePascal.png"),
       device = "png", width = 14, height = 8)
