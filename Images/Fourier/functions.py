from tkinter import ttk, filedialog

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


