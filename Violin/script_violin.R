library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

NPts <- 300
NLines <- 3
GridX <- 4
GridY <- 4

GenererDist <- function(moyenne, et) rnorm(NPts, moyenne, et)

set.seed(121221)
Graphe <- data.frame(
  grid_x = rep(seq(1, GridX), each = GridY * NLines * NPts),
  grid_y = rep(rep(seq(1, GridY), each = NPts * NLines), GridX),
  y = rep(rep(sample(seq(1, NLines), NLines), each = NPts), GridX * GridY),
  x = do.call("c", pmap(list(moy = rep(c(2, 2, 2), GridX * GridY), et = rep(c(12, 12, 12), GridX * GridY)), \(moy, et) GenererDist(moy, et)))
) %>% 
  ggplot(aes(x, y, fill = factor(y), color = factor(y))) +
  geom_violin(show.legend = FALSE) +
  facet_grid(grid_x ~ grid_y) +
  scale_fill_manual(values = c("#cfb708", "#0507a4", "#993a0e")) +
  scale_color_manual(values = c("#8a7a08", "#05064f", "#752703")) +
  theme_void() +
  theme(strip.text = element_blank()) +
  coord_cartesian(ylim = c(0, NLines + 1))
ggsave(Graphe, filename = paste0(Chemin, "/violin1.png"), device = "png", height = 8, width = 8)

Graphe <- data.frame(
  grid_x = rep(seq(1, GridX), each = GridY * NLines * NPts),
  grid_y = rep(rep(seq(1, GridY), each = NPts * NLines), GridX),
  y = rep(rep(sample(seq(1, NLines), NLines), each = NPts), GridX * GridY),
  x = do.call("c", pmap(list(moy = rep(c(12, 3, 6), GridX * GridY), et = rep(c(2, 2, 2), GridX * GridY)), \(moy, et) GenererDist(moy, et)))
) %>% 
  ggplot(aes(x, y, fill = factor(y), color = factor(y))) +
  geom_violin(show.legend = FALSE) +
  facet_grid(grid_x ~ grid_y) +
  scale_fill_manual(values = c("#cfb708", "#0507a4", "#993a0e")) +
  scale_color_manual(values = c("#8a7a08", "#05064f", "#752703")) +
  theme_void() +
  theme(strip.text = element_blank()) +
  coord_cartesian(ylim = c(0, NLines + 1))
ggsave(Graphe, filename = paste0(Chemin, "/violin2.png"), device = "png", height = 8, width = 8)

Graphe <- data.frame(
  grid_x = rep(seq(1, GridX), each = GridY * NLines * NPts),
  grid_y = rep(rep(seq(1, GridY), each = NPts * NLines), GridX),
  y = rep(rep(sample(seq(1, NLines), NLines), each = NPts), GridX * GridY),
  x = do.call("c", pmap(list(moy = do.call("c", map(seq_len(GridX * GridY), \(x) sample(c(3, 6, 12), NLines))), et = rep(c(2, 2, 2), GridX * GridY)), \(moy, et) GenererDist(moy, et)))
) %>% 
  ggplot(aes(x, y, fill = factor(y), color = factor(y))) +
  geom_violin(show.legend = FALSE) +
  facet_grid(grid_x ~ grid_y) +
  scale_fill_manual(values = c("#cfb708", "#0507a4", "#993a0e")) +
  scale_color_manual(values = c("#8a7a08", "#05064f", "#752703")) +
  theme_void() +
  theme(strip.text = element_blank()) +
  coord_cartesian(ylim = c(0, NLines + 1))
ggsave(Graphe, filename = paste0(Chemin, "/violin3.png"), device = "png", height = 8, width = 8)

Graphe <- data.frame(
  grid_x = rep(seq(1, GridX), each = GridY * NLines * NPts),
  grid_y = rep(rep(seq(1, GridY), each = NPts * NLines), GridX),
  y = rep(rep(seq(1, NLines), each = NPts), GridX * GridY),
  x = do.call("c", pmap(list(moy = do.call("c", map(seq_len(GridX) - 1, ~ rep(rev(seq(2, 2 + .x * 4, length.out = NLines)), GridY))), 
                             et = rep(do.call("c", map(seq_len(GridY) - 1, ~ rep(2 + .x * 1, 3))), GridX)), 
                             \(moy, et) GenererDist(moy, et)))
) %>% 
  ggplot(aes(x, y, fill = factor(y), color = factor(y))) +
  geom_violin(show.legend = FALSE) +
  facet_grid(grid_x ~ grid_y) +
  scale_fill_manual(values = c("#cfb708", "#0507a4", "#993a0e")) +
  scale_color_manual(values = c("#8a7a08", "#05064f", "#752703")) +
  theme_void() +
  theme(strip.text = element_blank()) +
  coord_cartesian(ylim = c(0, NLines + 1))
ggsave(Graphe, filename = paste0(Chemin, "/violin4.png"), device = "png", height = 8, width = 8)

Graphe <- data.frame(
  grid_x = rep(seq(1, GridX), each = GridY * NLines * NPts),
  grid_y = rep(rep(seq(1, GridY), each = NPts * NLines), GridX),
  y = rep(rep(sample(seq(1, NLines), NLines), each = NPts), GridX * GridY),
  x = do.call("c", pmap(list(moy = sample(do.call("c", map(seq_len(GridX) - 1, ~ rep(seq(2, 2.5 + .x * 4, length.out = NLines), GridY))), NLines * GridX * GridY), 
                             et = sample(rep(do.call("c", map(seq_len(GridY) - 1, ~ rep(2 + .x * .5, 3))), GridX), NLines * GridX * GridY)), 
                             \(moy, et) GenererDist(moy, et)))
) %>% 
  ggplot(aes(x, y, fill = factor(y), color = factor(y))) +
  geom_violin(show.legend = FALSE) +
  facet_grid(grid_x ~ grid_y) +
  scale_fill_manual(values = c("#cfb708", "#0507a4", "#993a0e")) +
  scale_color_manual(values = c("#8a7a08", "#05064f", "#752703")) +
  theme_void() +
  theme(strip.text = element_blank()) +
  coord_cartesian(ylim = c(0, NLines + 1))
ggsave(Graphe, filename = paste0(Chemin, "/violin5.png"), device = "png", height = 8, width = 8)

Graphe <- data.frame(
  grid_x = rep(seq(1, GridX), each = GridY * NLines * NPts),
  grid_y = rep(rep(seq(1, GridY), each = NPts * NLines), GridX),
  y = rep(rep(sample(seq(1, NLines), NLines), each = NPts), GridX * GridY),
  x = do.call("c", pmap(list(moy = rep(c(2, 2, 2), GridX * GridY), et = rep(c(12, 12, 12), GridX * GridY)), \(moy, et) GenererDist(moy, et)))
) %>% 
  mutate(couleur = y) %>% 
  group_by(grid_x, grid_y) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% mutate(couleur = factor(couleur, levels = 1:3, labels = sample(1:3, 3)) %>% as.character() %>% as.numeric()))) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(fill = c("#cfb708", "#0507a4", "#993a0e")[couleur], couleur = c("#8a7a08", "#05064f", "#752703")[couleur]) %>% 
  ggplot(aes(x, y, fill = fill, color = couleur, group = y)) +
  geom_violin(show.legend = FALSE) +
  facet_grid(grid_x ~ grid_y) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(strip.text = element_blank()) +
  coord_cartesian(ylim = c(0, NLines + 1))
ggsave(Graphe, filename = paste0(Chemin, "/violin6.png"), device = "png", height = 8, width = 8)

Graphe <- data.frame(
  grid_x = rep(seq(1, GridX), each = GridY * NLines * NPts),
  grid_y = rep(rep(seq(1, GridY), each = NPts * NLines), GridX),
  y = rep(rep(sample(seq(1, NLines), NLines), each = NPts), GridX * GridY),
  x = do.call("c", pmap(list(moy = do.call("c", map(seq_len(GridX * GridY), \(x) sample(c(3, 6, 12), NLines))), et = rep(c(2, 2, 2), GridX * GridY)), \(moy, et) GenererDist(moy, et)))
) %>% 
  mutate(couleur = y) %>% 
  group_by(grid_x, grid_y) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% mutate(couleur = factor(couleur, levels = 1:3, labels = sample(1:3, 3)) %>% as.character() %>% as.numeric()))) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(fill = c("#cfb708", "#0507a4", "#993a0e")[couleur], couleur = c("#8a7a08", "#05064f", "#752703")[couleur]) %>% 
  ggplot(aes(x, y, fill = fill, color = couleur, group = y)) +
  geom_violin(show.legend = FALSE) +
  facet_grid(grid_x ~ grid_y) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(strip.text = element_blank()) +
  coord_cartesian(ylim = c(0, NLines + 1))
ggsave(Graphe, filename = paste0(Chemin, "/violin7.png"), device = "png", height = 8, width = 8)

Graphe <- data.frame(
  grid_x = rep(seq(1, GridX), each = GridY * NLines * NPts),
  grid_y = rep(rep(seq(1, GridY), each = NPts * NLines), GridX),
  y = rep(rep(seq(1, NLines), each = NPts), GridX * GridY),
  x = do.call("c", pmap(list(moy = do.call("c", map(seq_len(GridX) - 1, ~ rep(rev(seq(2, 2 + .x * 4, length.out = NLines)), GridY))), 
                             et = rep(do.call("c", map(seq_len(GridY) - 1, ~ rep(2 + .x * 1, 3))), GridX)), 
                        \(moy, et) GenererDist(moy, et)))
) %>% 
  mutate(couleur = y) %>% 
  group_by(grid_x, grid_y) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% mutate(couleur = factor(couleur, levels = 1:3, labels = sample(1:3, 3)) %>% as.character() %>% as.numeric()))) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(fill = c("#cfb708", "#0507a4", "#993a0e")[couleur], couleur = c("#8a7a08", "#05064f", "#752703")[couleur]) %>% 
  ggplot(aes(x, y, fill = fill, color = couleur, group = y)) +
  geom_violin(show.legend = FALSE) +
  facet_grid(grid_x ~ grid_y) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(strip.text = element_blank()) +
  coord_cartesian(ylim = c(0, NLines + 1))
ggsave(Graphe, filename = paste0(Chemin, "/violin8.png"), device = "png", height = 8, width = 8)
