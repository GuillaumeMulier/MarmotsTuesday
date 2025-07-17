
# Import des librairies Python

import skimage as SkImg # Manipuler les images
import numpy as np # Outils pour les arrays
import matplotlib.pyplot as plt # Graphiques
import fct_fils as perso

NbreClous = 133

# Import de l'image d'Ester

print("Import de l'image et transformation...")
Pupuce = SkImg.io.imread("C:/Users/DRY12/Documents/GitHub/MarmotsTuesday/Strings/Ester/PupuceProcessed.png")
Pupuce = Pupuce[700:2700, 400:2400, :3] # Resize de l'image
#Pupuce = Pupuce[1400:2600, 900:2100, :3] # Resize de l'image
Pupuce = SkImg.transform.resize(Pupuce, (800, 800, 3))
MasquePupuce = (Pupuce[:, :, 0] > .8) & (Pupuce[:, :, 1] > .6) & (Pupuce[:, :, 2] < .7) # Masque de tout ce qui n'est pas pupuce
MatricePoids = np.full((800, 800), .7)
#MatricePoids = np.ones((800, 800))
MatricePoids[~MasquePupuce] = 1 # Matrice de poids pour défavoriser les erreurs dans la tête
Pupuce = SkImg.color.rgb2gray(Pupuce)

PupuceBruite = SkImg.util.random_noise(Pupuce, mode = "gaussian", var = 0.05)
ContoursPupuce = SkImg.filters.sobel(PupuceBruite)

# Tentative d'augmenter le contraste par égalisation d'histogramme et puissance

PupuceContraste = SkImg.exposure.equalize_adapthist(Pupuce, clip_limit = 0.1)
PupuceContraste[MasquePupuce] = PupuceContraste[MasquePupuce] ** .25
#PupuceContraste[~MasquePupuce] = PupuceContraste[~MasquePupuce] ** 1.5
#PupuceContraste[(~MasquePupuce) & (Pupuce > .6)] = np.clip(PupuceContraste[(~MasquePupuce) & (Pupuce > .6)] + .2, 0, 1)

# Générer le cercle de clous et les différents fils possibles

print(f"Génération du cercle de {NbreClous} clous et des coordonnées des différents fils possibles...")
#CercleClous = perso.GenererCercle((400, 400), 399, NbreClous)
CercleClous = GenererCercle((400, 400), 399, NbreClous)
DicoFils = GenererDicoFils(CercleClous[0], CercleClous[1], TypeLigne = "Anti-alias", Intensite = .15)
#DicoFils = perso.GenererDicoFils(CercleClous[0], CercleClous[1], TypeLigne = "Bresenham", Intensite = 1.0)

Test = GenererPlanFils(
    NbClous = NbreClous, DicoFils = DicoFils, NbMaxFils = 4000, FunPerte = lambda x: np.power(x, 2), 
    ImgBase = Pupuce, ImgPoids = MatricePoids, 
    FilNoir = True, NbFilsParEtape = np.inf, Verbose = True
)

# Afficher l'image modifiée

matrice = np.random.randint(0, 2, size=(4, 4))

fig, axes = plt.subplots(1, 3)

axes[0].scatter(CercleClous[0], CercleClous[1], color = "steelblue")
for i in range(len(Test[2]) - 1):
    axes[0].plot([CercleClous[0][Test[2][i]], CercleClous[0][Test[2][i + 1]]], [CercleClous[1][Test[2][i]], CercleClous[1][Test[2][i + 1]]], color = "black", alpha = .15)
axes[1].imshow(Test[0], cmap = "gray")
axes[2].imshow(Test[1], cmap = "gray")

plt.show()




