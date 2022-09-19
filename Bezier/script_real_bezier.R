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
