# -------------------------------------------- #
# Script de test des fonctions de Bézier Curve #
# Créé le 19/09/2022, modifié le 19/09/2022    #
# -------------------------------------------- #

Chemin <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste0(Chemin, "/script_anim_bezier.R"), encoding = "UTF-8")

# Animation ----

# On choisit des points (de préférence du même ordre de grandeur pour les limites X et Y car j'ai mis une échelle fixe)
XX <- c(-2, -2, 2, 8)
YY <- c(0, -8, -9, 1)

# On utilise la fonction pour obtenir le tableau
TabAnim <- ComputeBezier(XX, YY, Degre = 3L, NbInterpolation = 200L)

# Et on fait une animation qu'on peut sauvegarder :p
AnimerBezier(TabAnim, Duree = 10, FichierGIF = "bezier_marmanime")


# Image rainbow ----

# Pour l'instant c'est fait à la main, mais j'espère automatiser le process (Procédure ?)
# pour la prochaine fois

XX <- c(0, 2, 4.5, 2, 0)
XX <- c(XX, -XX)
YY <- c(0, 2, 0, -3, -5)
YY <- c(YY, YY)

TabImg <- ComputeBezier(XX, YY, NbInterpolation = 200L, Degre = 4L)

CourbeBezier <- ImagerBezier(TabImg)
ggsave(CourbeBezier, 
       filename = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/bezier_marmotte_coeur.png"),
       device = "png", height = 8, width = 8)

# Essayer de l'animer pour voir
AnimerBezier(TabImg, Duree = 10, FichierGIF = "bezier_queur")
