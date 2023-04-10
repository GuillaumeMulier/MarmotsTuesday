library(rwhatsapp)
library(data.table)
library(ggwordcloud)
library(tidyverse)

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)

gaston <- rwa_read(paste0(Chemin, "/historique_marmotte.txt"))

setDT(gaston)
gaston[, emoji_name := map2(emoji_name, emoji, \(x, y) if (is.null(x)) NULL else setNames(map_chr(x, \(vec) paste0(" ", gsub(" ", "-", vec))), y))]
gaston[, longueur := lapply(emoji_name, length)]
gaston[longueur != 0, text := map2_chr(text, emoji_name, ~ str_replace_all(.x, .y))]
gaston <- gaston[!is.na(author) & !(text %in% c("<Médias omis>", "Les messages et les appels sont chiffrés de bout en bout. Aucun tiers, pas même WhatsApp, ne peut les lire ou les écouter. Appuyez pour en savoir plus.")), 
                 .(time, author, text)]
gaston[, text := strsplit(text, " |\\n|\\.")]
gaston <- gaston[, .(text = unlist(text)), by = .(time, author)]
gaston <- gaston[!text %in% c("", " ")]
gaston[, text := str_to_lower(text)]
gaston[, author := factor(author, levels = c("Axelle", "guillaume mulier62220"), labels = c("Marmotte", "Possoum"))]
gaston <- gaston[str_length(text) > 5, .N, by = .(text, author)][order(N, decreasing = TRUE)][, head(.SD, 250), by = author]
gaston[author == "Possoum", N := N ** (1 / .95)]
gaston[author == "Marmotte", N := N ** (1 / .8)]
set.seed(121221)
gaston[author == "Possoum", couleur := sample(c("#06BFA3", "#18B911", "#B36207"), .N, replace = TRUE)]
gaston[author == "Marmotte", couleur := sample(c("#0E04D5", "#7C19AD", "#B40B40"), .N, replace = TRUE)]
gaston[, angle := runif(.N, -5, 5) + 45 * sample(-3:4, .N, replace = TRUE, prob = c(.5, 1, .5, 4, .5, 2, .5, 1)), ]
(Nuage <- ggplot(gaston, aes(x = author, label = text, size = N, color = couleur, angle = angle)) +
  geom_text_wordcloud_area(rm_outside = TRUE, eccentricity = .5, shape = "diamond") +
  theme_void() +
  scale_color_identity() +
  scale_size_area(max_size = 25))
ggsave(paste0(Chemin, "/wordcloud.png"), width = 7, height = 5, device = "png", bg = "white")
