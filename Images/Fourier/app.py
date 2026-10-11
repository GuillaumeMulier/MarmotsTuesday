import tkinter as tk
from tkinter import ttk
from functions import *
from PIL import Image, ImageTk
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure

# Initialisation de l'application
Appli = tk.Tk()
Appli.title("Génération d'un chemin à partir d'une image")
Appli.geometry("1400x1000")

# Variables globales
Recording = False
ListeCoords = [[], [], [], [], []]
IndexChemin = tk.IntVar(value = 0)
Couleurs = ["red", "blue", "green", "orange", "yellow", "purple"]

# Génération des différents onglets avec un notebook tkinter
OngletsAppli = ttk.Notebook(Appli)
OngletsAppli.pack(expand = True, fill = "both")

# Onglet d'import de l'image
OngletImport = ttk.Frame(OngletsAppli)
tk.Label(OngletImport, text = "Import de l'image et recueil du chemin",
                       font = ("Times New Roman", 20, "bold"), fg = "darkblue", justify = "center").pack(fill = "x")
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
tk.Button(OngletImport, text = "Sélectionner une image", command = ChargerDisplayImg,
                            font = ("Times New Roman", 14, "bold"), fg = "darkblue").pack(pady = 20)
## Enregistrer les clics
def ResetClics():
    global ListeCoords
    ListeCoords = [[], [], [], [], []]
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
    print(IndexChemin.get())
    return
def ClicSouris(event):
    global ListeCoords
    if Recording:
        ListeCoords[IndexChemin.get()].append((event.x, event.y))
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
tk.Label(MenuClics, text = "Chemin n°", font = ("Times New Roman", 10, "bold"), fg = "darkblue").pack(pady = 10)
for i in range(5):
    tk.Radiobutton(MenuClics, text = str(i), value = i, font = ("Times New Roman", 10), fg = "darkblue", variable = IndexChemin).pack(pady = 0)
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
## Menu pour calculer la transformée de Fourier
MenuFourier = ttk.Frame(OngletFourier)
MenuFourier.pack(side = "left", fill = "y", pady = 10, padx = 10)
tk.Button(MenuFourier, text = "Export CSV", command = lambda: ExportChemins(ListeCoords),
          font = ("Times New Roman", 20, "bold"), fg = "darkblue", justify = "center").pack(pady = 10)
## Affichage du résultat
MenuGraphique = ttk.Frame(OngletFourier)
MenuGraphique.pack(side = "right", fill = "both", pady = 10, padx = 10)
## Figure des points et de la transformée
fig = Figure(figsize = (8, 6), dpi = 100)
ax = fig.add_subplot(111)
ax.set_title("Reconstruction de l'image")
ax.set_xlabel("X")
ax.set_ylabel("Y")
ax.invert_yaxis()
AffichageGraphe = FigureCanvasTkAgg(fig, master = MenuGraphique)
AffichageGraphe.draw()
AffichageGraphe.get_tk_widget().pack(fill = "both", expand = True, side = "top")
def AfficherChemins():
    ax.clear()
    for num_chemin, coord_chemin in enumerate(ListeCoords):
        if coord_chemin:
            x_vals = [p[0] for p in coord_chemin]
            y_vals = [p[1] for p in coord_chemin]
            ax.plot(x_vals, y_vals, color = Couleurs[num_chemin], label = f"Chemin {num_chemin}")
    ax.set_title("Reconstruction de l'image")
    ax.set_xlabel("X")
    ax.set_ylabel("Y")
    ax.invert_yaxis()
    ax.legend()
    AffichageGraphe.draw()
tk.Button(MenuGraphique, text = "Actualiser", command = AfficherChemins,
          font = ("Times New Roman", 10, "bold"), fg = "darkblue", justify = "center").pack(pady = 10)
OngletsAppli.add(OngletFourier, text = "Fourier")

# Lancer l'application
Appli.mainloop()