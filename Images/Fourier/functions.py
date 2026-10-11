from tkinter import ttk, filedialog
import pandas as pd

def DialogueOpenImage():
    """
    Ouvre une boîte à dialogue pour sélectionner une image
    Retourne le chemin de cette image
    """
    typefile = [
        ("Image", "*.png *.jpg *.jpeg"),
        ("Tous les fichiers", "*.*"),
    ]
    pathfile = filedialog.askopenfilename(title = "Sélectionner une image", filetypes = typefile)
    return pathfile

def ExportChemins(ListeCoordonnees):
    Directory = filedialog.askdirectory(title = "Sélectionner un dossier pour enregister les données")
    Donnees = []
    for num_chemin, coordonnees in enumerate(ListeCoordonnees):
        for point, (x,y) in enumerate(coordonnees):
            Donnees.append({"index_chemin": num_chemin, "index_point": point, "x": x, "y": y})
    Donnees = pd.DataFrame(Donnees)
    Donnees.to_csv(Directory + "/DonneesChemins.csv", index = False)
    return

