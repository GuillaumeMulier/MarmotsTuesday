
import os
from PIL import Image as img
import math
import numpy as np
import matplotlib.pyplot as plt

CheminImage = "img_marmottes.jpg"
Largeur = 500
Hauteur = 500

# Fixer le dossier dans lequel on travaille
os.chdir("C:\\Users\\gmulier\\Documents\\GitHub\\MarmotsTuesday\\Images\\")

def PrepImg (Image, Largeur, Hauteur) :
    """
    Fonction qui croppe et redimensionne l'image
    :param Image: l'image qu'on veut redimensionner
    :param Largeur: la nouvelle largeur
    :param Hauteur: la nouvelle hauteur
    :return: l'image redimensionnée
    """
    # Stocker les dimensions
    LImg = Image.width
    HImg = Image.height
    RatioImage = LImg / HImg
    RatioUpdated = Largeur / Hauteur

    # Redimensionner l'image
    if RatioImage > RatioUpdated:
        # L'image est plus large que nécessaire, on rogne sur les côtés
        LargeurUpdated = int(HImg * RatioUpdated)
        LargeurARetirer = (LImg - LargeurUpdated) // 2
        ImageCropped = Image.crop((LargeurARetirer, 0, LargeurUpdated + LargeurARetirer, HImg))
    else:
        # L'image est plus haute que nécessaire, on rogne le haut et le bas
        HauteurUpdated = int(LImg / RatioUpdated)
        HauteurARetirer = (HImg - HauteurUpdated) // 2
        ImageCropped = Image.crop((0, HauteurARetirer, LImg, HauteurUpdated + HauteurARetirer))
    Image = ImageCropped.resize((Largeur, Hauteur))

    return Image

def GenererPins (NPins, Dim, Forme="cercle"):
    """
    Fonction pour déterminer où les clous seront placés
    :param NPins: Nombre de clous
    :param Dim: Tuple donnant la largeur et la hauteur de la photo
    :param Forme: "cercle" ou "carré"
    :return: Liste de tuples de coordonnées (x, y) des clous
    """
    Largeur = Dim[0]
    Hauteur = Dim[1]
    if Forme == "carré":
        # On convertit le nombre de clous pour un chiffre pair
        NPins = NPins // 2 * 2
        RatioL = Largeur / (Largeur + Hauteur)
        NPinsHauteur = int(NPins / 2 * (1 - RatioL))
        NPinsLargeur = int(NPins / 2 - NPinsHauteur)

        # Les 4 vecteurs à concaténer en une liste de coordonnées
        Haut = [(int(i * Largeur / NPinsLargeur), 0) for i in range(NPinsLargeur + 1) if i != 0]
        Droite = [(Largeur, int(i * Hauteur / NPinsHauteur)) for i in range(NPinsHauteur + 1) if i != 0]
        Bas = [(int(i * Largeur / NPinsLargeur), Hauteur) for i in range(NPinsLargeur + 1) if i != NPinsLargeur][::-1]
        Gauche = [(0, int(i * Hauteur / NPinsHauteur)) for i in range(NPinsHauteur + 1) if i != NPinsHauteur][::-1]
        ListeClous = Haut + Droite + Bas + Gauche

    elif Forme == "cercle":
        Rayon = min(Largeur, Hauteur) / 2
        Centre = (Largeur / 2, Hauteur / 2)
        Angles = [i / NPins * 2 * math.pi for i in range(NPins)]
        ListeClous = [(Rayon * math.cos(a) + Centre[0], Rayon * math.sin(a) + Centre[1]) for a in Angles]

    return ListeClous

# Charger une image à partir du fichier
Image = img.open(CheminImage)

# Redimensionner l'image tout en conservant le rapport d'aspect
Image = PrepImg(Image, 700, 700)

"""
# Sauvegarder l'image résultante
resized_image.save("chemin/vers/votre/image_redimensionnee.jpg")


# Afficher l'image
Image.show()
ImageCropped.show()
Image.size
"""

resultats_cos_sin = GenererPins(100, (100, 200), "carré")
x, y = zip(*resultats_cos_sin)

# Créer un graphique de dispersion
plt.scatter(x, y)
plt.show()
