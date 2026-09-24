import tkinter as tk
from tkinter import ttk
from functions import *
from PIL import Image, ImageTk

# Variables globales
Recording = False
ListeCoords = []

# Application en fullscreen
Appli = tk.Tk()
Appli.title("Génération d'un chemin à partir d'une image")
Appli.geometry("1400x1000")

# Génération des différents onglets avec un notebook tkinter
OngletsAppli = ttk.Notebook(Appli)
OngletsAppli.pack(expand = True, fill = "both")

# Onglet d'import de l'image
OngletImport = ttk.Frame(OngletsAppli)
LabelImport = tk.Label(OngletImport, text = "Import de l'image et recueil du chemin",
                       font = ("Times New Roman", 20, "bold"), fg = "darkblue", justify = "center")
LabelImport.pack(fill = "x")
## Fonction pour importer et afficher l'image
def ChargerDisplayImg():
    CheminImg = DialogueOpenImage()
    if CheminImg:
        Img = Image.open(CheminImg)
        Img.thumbnail((800, 800))
        TkImg = ImageTk.PhotoImage(Img.convert("RGB"))
        # Affichage de l'image dans l'interface de l'appli
        AffichageImg.configure(image = TkImg)
        AffichageImg.image = TkImg
    return
## Sélectionner et afficher l'image choisie
BoutonSelection = tk.Button(OngletImport, text = "Sélectionner une image", command = ChargerDisplayImg,
                            font = ("Times New Roman", 14, "bold"), fg = "darkblue")
BoutonSelection.pack(pady = 20)
## Enregistrer les clics
def ResetClics():
    global ListeCoords
    ListeCoords = []
    # Remettre à 0 aussi le canevas
    return
def StartStopClics():
    global Recording
    Recording = not Recording
    if Recording:
        BoutonStartStop.configure(text = "Stop")
    else:
        BoutonStartStop.configure(text = "Start")
    return
def AffichageClics():
    print(ListeCoords)
    return
def ClicSouris(event):
    global ListeCoords
    if Recording:
        ListeCoords.append((event.x, event.y))
    return
MenuClics = ttk.Frame(OngletImport)
MenuClics.pack(side = "left", fill = "y", pady = 10, padx = 10)
BoutonReset = tk.Button(MenuClics, text = "Reset", command = ResetClics,
                            font = ("Times New Roman", 10, "bold"), fg = "darkblue")
BoutonReset.pack(pady = 10)
BoutonStartStop = tk.Button(MenuClics, text = "Start", command = StartStopClics,
                            font = ("Times New Roman", 10, "bold"), fg = "darkblue")
BoutonStartStop.pack(pady = 10)
BoutonDisplay = tk.Button(MenuClics, text = "Afficher", command = AffichageClics,
                            font = ("Times New Roman", 10, "bold"), fg = "darkblue")
BoutonDisplay.pack(pady = 10)
## Affichage de l'image
MenuImg = ttk.Frame(OngletImport)
MenuImg.pack(side = "right", fill = "both", pady = 10, padx = 10)
AffichageImg = tk.Label(MenuImg)
AffichageImg.pack()
AffichageImg.bind("<Button-1>", ClicSouris)
OngletsAppli.add(OngletImport, text = "Import/Clics")


# Onglet de calcul de la transformée de Fourier
OngletFourier = ttk.Frame(OngletsAppli)
LabelFourier = tk.Label(OngletFourier, text = "Génération du chemin par transformée de Fourier",
                       font = ("Times New Roman", 20, "bold"), fg = "darkblue", justify = "center")
LabelFourier.pack(fill = "x")
OngletsAppli.add(OngletFourier, text = "Fourier")

# Lancer l'application
Appli.mainloop()