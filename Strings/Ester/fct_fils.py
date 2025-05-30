"""
Script de fonctions pour trouver les fils à tirer pour reproduire l'image en question
Auteur : G. Mulier
"""

# Libraries requises

from skimage.draw import line, line_aa
import numpy as np
import matplotlib.pyplot as plt
from random import sample

# Définition des fonctions

def CoordLines(XDeb, YDeb, XFin, YFin, TypeLigne = "Bresenham", Intensite = 1.0):
    """
    Fonction pour trouver les coordonnées et les niveaux de gris des pixels traversés par la ligne.
    Ligne type Bresenham ou anti-alias de Wu (scikit-image).
    Attention, pour anti-alias, les valeurs sorties seront des floats !
    Sortie : array des y, array des x et array des valeurs (si anti-alias, si Bresenham, juste les 2 1ers)
    """
    TypeSupportes = ["Bresenham", "Anti-alias"]
    if TypeLigne not in TypeSupportes:
        raise ValueError(f"Méthode non supportée : {TypeLigne}. Sélectionner parmi {' - '.join(TypeSupportes)} !")
    elif TypeLigne == "Bresenham":
        Ligne = line(YDeb, XDeb, YFin, XFin)
        Ligne = Ligne + (Intensite, )
    elif TypeLigne == "Anti-alias":
        Ligne = line_aa(YDeb, XDeb, YFin, XFin)
        Ligne= Ligne[:2] + (Ligne[2] * Intensite, )
    return Ligne

def GenererCercle(Centre, Rayon, NbPts):
    """
    Renvoie les coordonnées des clous disposés en cercle.
    Sortie : Liste de tuples de coordonnées (x, y) = (Colonne, Ligne)
    """
    Angle = np.linspace(0, 2 * np.pi, num = NbPts + 1)[1:][::-1]
    return [[int(Rayon * np.cos(angle) + Centre[0]) for angle in Angle],
            [int(Rayon * np.sin(angle) + Centre[1]) for angle in Angle]]

def GenererDicoFils(XClous, YClous, TypeLigne = "Bresenham", Intensite = 1.0):
    """
    Renvoit le dictionnaire de tous les fils possibles.
    Sortie : dictionnaire avec dictionnaire[x][y] les valeurs des pixels entre les fils x et y.
    """
    if len(XClous) != len(YClous):
        raise ValueError(f"L'array de colonnes des clous (longueur = {len(XClous)}) et celui de lignes des clous (longueur = {len(YClous)}) n'ont pas la même longueur !")
    NbClous = len(XClous)
    ListeFils = dict()
    for fil1 in range(NbClous - 1):
        TempDict = dict()
        for fil2 in range(fil1 + 1, NbClous):
            TempDict[fil2] = CoordLines(XClous[fil1], YClous[fil1], XClous[fil2], YClous[fil2], TypeLigne, Intensite)
        ListeFils[fil1] = TempDict
    return ListeFils

def FctPerte(ValReelles, ValRecons, Poids = 1.0, Fun = np.abs):
    """
    Fonction de perte qui sera à minimiser : représente l'erreur entre l'image et la reconstruction
    Calcul = Fun(Poids * (ValReelles - ValRecons))
    Sortie : valeur d'erreur, float
    """
    if type(Poids) != list and type(Poids) != np.ndarray: # Si juste list, mauvais calcul avec les arrays numpy
        Poids = [Poids] * len(ValReelles)
    return np.sum([poids * Fun(reel - recons) for reel, recons, poids in zip(ValReelles, ValRecons, Poids)])
    
def GenererPlanFils(NbClous, DicoFils, NbMaxFils, FunPerte, ImgBase, ImgPoids, FilNoir = True, NbFilsParEtape = np.inf, Verbose = True):
    """
    Fonction principale. A partir de l'image en niveaux de gris, va optimiser un trajet de fils.
    Contrainte : C'est un seul fil donc on démarre de l'arrivée précédente pour les fils.
    Si c'est un fil noir, on part d'une image blanche avec 1 partout et on déduit la valeur dans les pixels, mais si c'est un fil blanc, c'est l'inverse
    """
    # Initialiser l'image reconstruite
    if FilNoir: # Fil noir sur fond blanc
        ImgRecons = np.full(ImgBase.shape, 1.0)
        Mult = -1.0
    else: # Fil blanc sur fond noir
        ImgRecons = np.full(ImgBase.shape, 0.0)
        Mult = 1.0
    
    # Setup des variables
    FilG = -1
    FilD = -1
    CurFil = -1
    Erreur = round(FctPerte(ImgBase, ImgRecons, ImgPoids, FunPerte), 3)
    CurXs = []
    CurYs = []
    CurVals = []
    Chemin = []
    print(f"Avant le moindre fil, l'erreur entre image vide et image cible est de {str(Erreur)} !")

    # Trouver le premier fil : boucler sur toutes les paires et rendre celle qui donne l'erreur la plus faible qui est inférieure à celle de l'image vide
    CurGain = 0
    for fil1 in range(NbClous - 1):
        for fil2 in range(fil1 + 1, NbClous):
            Xs, Ys, Vals = DicoFils[fil1][fil2]
            GainTemp = round(FctPerte(ImgBase[Xs, Ys], ImgRecons[Xs, Ys], ImgPoids[Xs, Ys], FunPerte) - FctPerte(ImgBase[Xs, Ys], ImgRecons[Xs, Ys] + Mult * Vals, ImgPoids[Xs, Ys], FunPerte), 3)
            if GainTemp > CurGain:
                CurGain = GainTemp
                FilG = fil1
                FilD = fil2
                CurXs = Xs
                CurYs = Ys
                CurVals = Vals
    Erreur = Erreur - CurGain
    CurFil = sample([FilG, FilD], 2)
    Chemin.append(CurFil[0])
    Chemin.append(CurFil[1])
    CurFil = CurFil[1]
    ImgRecons[CurXs, CurYs] = ImgRecons[CurXs, CurYs] + Mult * CurVals
    print(f"Premier fil entre le clou {str(FilG)} et le clou {str(FilD)} avec maintenant une erreur de {str(Erreur)} ! Début de la recherche des fils suivants...")

    # Maintenant il faut trouver les fils suivants en continuant le fil déjà tendu
    for i in range(2, NbMaxFils):
        if Verbose and (i % 100) == 0: print(f"Fil n°{str(i)} : erreur résiduelle = {str(Erreur)} !")
        CurGain = 0
        FilG = CurFil
        if NbFilsParEtape > (NbClous - 1): # Ici on regarde tous les fils (séparation des boucles car range est plus efficient que de construire un array)
            for fil in range(NbClous):
                if fil != FilG: # On prend un clou différent de celui sur lequel on est
                    if fil < FilG:
                        Xs, Ys, Vals = DicoFils[fil][FilG]
                    else:
                        Xs, Ys, Vals = DicoFils[FilG][fil]
                    GainTemp = round(FctPerte(ImgBase[Xs, Ys], ImgRecons[Xs, Ys], ImgPoids[Xs, Ys], FunPerte) - FctPerte(ImgBase[Xs, Ys], ImgRecons[Xs, Ys] + Mult * Vals, ImgPoids[Xs, Ys], FunPerte), 3)
                    if GainTemp > CurGain:
                        CurGain = GainTemp
                        FilD = fil
                        CurXs = Xs
                        CurYs = Ys
                        CurVals = Vals
            # Si on n'a trouvé aucun clou on s'arrête
            if CurGain == 0:
                print(f"Aucun fil ne minimise l'erreur, on s'arrête au clou n°{i}...")
                break
            else: # On a trouvé un clou donc on met tout à jour
                Erreur = Erreur - CurGain
                Chemin.append(FilD)
                FilG = FilD
                ImgRecons[CurXs, CurYs] = ImgRecons[CurXs, CurYs] + Mult * CurVals
        else: # On prend un certain nombre de clous au hasard (pour aller plus vite)
            pass 

    return (ImgBase, ImgRecons, Chemin)




