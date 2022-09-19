# -------------------------------------------- #
# Script de test des fonctions de Bézier Curve #
# Créé le 19/09/2022, modifié le 19/09/2022    #
# -------------------------------------------- #

source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/script_anim_bezier.R"), encoding = "UTF-8")

# Animation ----

# On choisit des points (de préférence du même ordre de grandeur pour les limites X et Y car j'ai mis une échelle fixe)
XX <- c(-2, -2, 2, 8)
YY <- c(0, -8, -9, 1)

# On utilise la fonction pour obtenir le tableau
TabAnim <- ComputeBezier(XX, YY, Degre = 3L, NbInterpolation = 200L)

# Et on fait une animation qu'on peut saubegarder :p
AnimerBezier(TabAnim, Duree = 10, FichierGIF = "bezier_marmanime")


# Image rainbow ----

# Pour l'instant c'est fait à la main, mais j'espère automatiser le process (Procédure ?)
# pour la prochaine fois

XX <- c(0, 2, 6, 8, 10)
YY <- c(0, -2, 1, 3, 0)

TabImg <- ComputeBezier(XX, YY, NbInterpolation = 25L)

EssaiImgBezier <- ggplot(data = NULL) +
  geom_segment(data = bind_cols(TabImg[TabImg$deg == 1 & TabImg$ind == 1, ] |> 
                                  as_tibble() |> 
                                  rename(x = bezier_abscisse, y = bezier_ordonnee) |> 
                                  mutate(couleur = natparks.pals("Arches", 25, "continuous", 1)),
                             TabImg[TabImg$deg == 1 & TabImg$ind == 2, ] |>
                               as_tibble() |> 
                               rename(xend = bezier_abscisse, yend = bezier_ordonnee)),
            mapping = aes(x = x, xend = xend, y = y, yend = yend, color = couleur), size = .9) +
  geom_segment(data = bind_cols(TabImg[TabImg$deg == 1 & TabImg$ind == 3, ] |> 
                                  as_tibble() |> 
                                  rename(x = bezier_abscisse, y = bezier_ordonnee) |> 
                                  mutate(couleur = natparks.pals("Arches", 25, "continuous", -1)),
                                TabImg[TabImg$deg == 1 & TabImg$ind == 4, ] |>
                                  as_tibble() |> 
                                  rename(xend = bezier_abscisse, yend = bezier_ordonnee)),
               mapping = aes(x = x, xend = xend, y = y, yend = yend, color = couleur), size = .9) +
  scale_color_identity() +
  annotate("point", x = XX, y = YY, size = 2.5, shape = 18) +
  annotate("path", x = XX, y = YY) +
  geom_path(data = TabImg[TabImg$deg == 2, ], aes(bezier_abscisse, bezier_ordonnee), size = 1.1) +
  theme_void() +
  theme(plot.background = element_rect(fill = PallettePerso[4], color = PallettePerso[4])) +
  coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(-2.5, 4.5))
ggsave(EssaiImgBezier, filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/bezier_marmotte_raibow.png"),
       device = "png", height = 8, width = 8)
