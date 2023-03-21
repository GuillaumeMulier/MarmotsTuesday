
library(ambient)
set.seed(1212)
df <- data.table(
  fil = rep(c(1, 2), each = 10000),
  id = rep(seq_len(10000), 2)
)
df <- df[, .(id, xx = gen_perlin(x = id, y = 1 + 500 * (fil == 1), frequency = 1.22), yy = cos(id) * 2 * gen_perlin(x = 1 + 500 * (fil == 1), y = id, frequency = .1)), by = fil]
df <- df[, .(id, xx = cumsum(xx), yy = cumsum(yy)), by = fil]
df[, id := 1 + id %/% 100]
Palette1 <- colorRampPalette(c("#111ee0", "#eaf906"), interpolate = "spline")(101)
Palette2 <- colorRampPalette(c("#d7081e", "#882a8a"), interpolate = "spline")(101)
df[fil == 1, couleur := Palette1[id]]
df[fil == 2, couleur := Palette2[id]]
ggplot(df, aes(xx, yy, color = couleur, size = factor(fil))) +
  geom_path(show.legend = FALSE) +
  scale_color_identity() +
  scale_size_manual(values = c(.5, .3)) +
  coord_fixed(ratio = 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#05073f"),
        panel.background = element_rect(fill = "#05073f"))


