library(tidyverse)
library(progressr)

handlers(handler_progress(format = "  [:bar]  :percent", complete = "=", incomplete = " ", current = ">"))

# Créer des particules
set.seed(121221)
ListePart <- lapply(seq_len(500), \(x) list(positions = data.frame(iter = 0, xx = runif(1, 0, 100), yy = runif(1, 0, 100)),
                               velocite_x = runif(1, -1, 1), velocite_y = rnorm(1, 0, .7)))

# Update de la position
with_progress({
  PBarre <- progressor(steps = 2000)
  for (i in seq_len(2000)) {
    ListePart <- lapply(ListePart, \(x) {
      list(positions = rbind(x$positions, x$positions[nrow(x$positions), ] + c(1, x$velocite_x, x$velocite_y)), 
           velocite_x = x$velocite_x, velocite_y = x$velocite_y)
    })
    PBarre()
  }
})
ListePartDF <- map_dfr(ListePart, ~ .x$positions %>% mutate(xx = xx %% 100, yy = yy %% 100, vitesse = .x$velocite_x ** 2 + .x$velocite_y ** 2))

# On fait le gros graphique et on espère que c'est joli
Graphe <- ggplot() +
    theme_void() +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    map(0:500, ~ list(
      geom_point(data = ListePartDF %>% filter(iter == .x), 
                 aes(xx, yy), 
                 color = "#05055a", fill = "#05055a", shape = 21, size = 8.5, alpha = .7),
      geom_point(data = ListePartDF %>% filter(iter == .x), 
                 aes(xx, yy, fill = vitesse), 
                 color = "#05055a", shape = 21, size = 8, show.legend = FALSE, alpha = .7)
    )) +
    scale_fill_gradient(low = "#126603", high = "#0dd9f5")
ggsave(Graphe, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/particules_rundown.png"), device = "png", height = 10, width = 10)
  