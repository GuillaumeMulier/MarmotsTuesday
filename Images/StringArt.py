
import os
from PIL import Image as img
import math
import numpy as np
import skimage.draw as dessin
import matplotlib.pyplot as plt
import random
import time

# Enregistrez le temps de début
debut = time.time()

CheminImage = "output.png"
Largeur = 900
Hauteur = 900

# Fixer le dossier dans lequel on travaille
#os.chdir("C:\\Users\\gmulier\\Documents\\GitHub\\MarmotsTuesday\\Images\\")
os.chdir("C:\\Users\\DRY12\\Documents\\GitHub\\MarmotsTuesday\\Images\\")

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
    Largeur = Dim[1]
    Hauteur = Dim[0]
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

    x = [int(l[0]) for l in ListeClous]
    y = [int(l[1]) for l in ListeClous]

    return x, y

def ConvertGris (Image):
    """
    Convertir l'image en noir et blanc
    :param Image: l'image à convertir
    :return: Matrice en noir et blanc (0 = noir et 1 = blanc)
    """
    return np.array(Image.convert("L")) / 255

def InitierTableau (Dimensions, XPins, YPins):
    """
    Initie le tableau avec les clous
    :param Dimensions: tuple (hauteur, largeur) des dimensions de la matrice pour représenter l'image
    :param XPins: coordonnées x des clous
    :param YPins: coordonnées y des clous
    :return: La matrice de l'image
    """
    ImageConstruction = np.ones(Dimensions)
    ImageConstruction[[int(yy) for yy in YPins], [int(xx) for xx in XPins]] = 0
    return ImageConstruction

def MakeLines (XDebut, YDebut, XFin, YFin, ImageDepartLignes, Puissance=1, Technique="aa_line"):
    if Technique == "aa_line":
        lignes, colonnes, valeurs = dessin.line_aa(YDebut, XDebut, YFin, XFin)
    return lignes, colonnes, valeurs

def ListePossibleClous (ClouDepart, NClous, Tolerance, Dimensions, Forme="cercle"):
    if Forme == "cercle":
        excluded_set = {(ClouDepart + i) % NClous for i in range(-Tolerance, Tolerance + 1)}
        return [value for value in range(NClous) if value not in excluded_set]

def EvalDiff (Image1, Image2, Fct="square"):
    if Fct == "square":
        return np.sum((Image2 - Image1) ** 2)
    elif Fct == "abs":
        return np.sum(abs(Image2 - Image1))

def MeilleureLigne (ClouDepart, NClous, Tolerance, Dimensions, ImageDepart, ImageCible,
                    XPins, YPins, Puissance=.3, Forme="cercle", FctDiff="square"):

    # Faire la liste des différents clous possibles
    ListeClous = ListePossibleClous(ClouDepart, NClous, Tolerance, Dimensions, Forme)

    # Etat des lieux au départ avant de boucler sur les clous possibles
    DifferenceBase = EvalDiff(ImageDepart, ImageCible, FctDiff)
    DifferenceApres = -math.inf
    ClouSuivant = -99
    XDepart = XPins[ClouDepart]
    YDepart = YPins[ClouDepart]
    LignesSuiv = []
    ColonnesSuiv = []
    ValeursSuiv = []

    # Passer sur tous les clous possibles et choisir le meilleur s'il existe
    for c in ListeClous:
        Lignes, Colonnes, Valeurs = MakeLines(int(XDepart), int(YDepart), int(XPins[c]), int(YPins[c]), ImageDepart, Puissance)
        DifferencePre = EvalDiff(ImageDepart[Lignes, Colonnes], ImageCible[Lignes, Colonnes], FctDiff)
        DifferencePost = EvalDiff(np.clip(ImageDepart[Lignes, Colonnes] - Valeurs * Puissance, a_min=0, a_max=1), ImageCible[Lignes, Colonnes], FctDiff)
        Amelioration = np.sum(DifferencePre - DifferencePost)
        if Amelioration > DifferenceApres:
            DifferenceApres = Amelioration
            ClouSuivant = c
            LignesSuiv = Lignes
            ColonnesSuiv = Colonnes
            ValeursSuiv = Valeurs

    if ClouSuivant == -99:
        return ClouSuivant, ImageDepart
    else:
        ImageDepart[LignesSuiv, ColonnesSuiv] = np.clip(ImageDepart[LignesSuiv, ColonnesSuiv] - ValeursSuiv * Puissance, a_min=0, a_max=1)
        return ClouSuivant, ImageDepart




# Charger une image à partir du fichier
Image = img.open(CheminImage)

# Redimensionner l'image tout en conservant le rapport d'aspect
Image = PrepImg(Image, Hauteur, Largeur)

# Convertir en gris (0 = noir et 255 = blanc)
ImageCible = ConvertGris(Image)

# Initier l'image en construction
XPins, YPins = GenererPins(240, (Hauteur - 1, Largeur - 1))
ImageConstruction = InitierTableau((Hauteur, Largeur), XPins, YPins)



#ImageConstruction = MakeLines(XPins[0], YPins[0], XPins[Clou1], YPins[Clou1], ImageConstruction, .3)
#ImageConstruction = MakeLines(XPins[Clou1], YPins[Clou1], XPins[Clou2], YPins[Clou2], ImageConstruction, .5)
#ImageConstruction = MakeLines(XPins[Clou2], YPins[Clou2], XPins[Clou3], YPins[Clou3], ImageConstruction, 1)

# Choix du 1er clou
Clou1 = random.randint(0, 239)
N_ITER = 5000
ListeClous = [Clou1]

i = 0
stop = False
while i < N_ITER and not stop:
    if i % 250 == 0:
        print(i)
    ClouSuivant, ImageConstruction = MeilleureLigne(Clou1, 240, 10, (Hauteur, Largeur), ImageConstruction, ImageCible,
                    XPins, YPins, Puissance=.3)
    if ClouSuivant == -99:
        stop = True
    Clou1 = ClouSuivant
    ListeClous.append(ClouSuivant)
    i = i + 1

# Enregistrez le temps de fin
fin = time.time()

# Calculez la durée d'exécution
duree = fin - debut

print("Temps d'exécution:", duree, "secondes")
"""
# Sauvegarder l'image résultante
resized_image.save("chemin/vers/votre/image_redimensionnee.jpg")


# Afficher l'image
Image.show()
ImageCropped.show()
Image.size

resultats_cos_sin = GenererPins(100, (100, 200), "carré")
x, y = zip(*resultats_cos_sin)

# Créer un graphique de dispersion
plt.scatter(x, y)
plt.show()
"""



plt.imshow(ImageConstruction, cmap='gray')  # 'gray' pour indiquer le colormap en niveaux de gris
plt.title('Image en niveaux de gris')
plt.colorbar()  # Ajouter une barre de couleur pour représenter les valeurs
plt.show()
