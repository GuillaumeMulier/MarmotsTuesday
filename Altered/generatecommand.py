import pandas as pd

TabBase = pd.read_excel("C:/Users/DRY12/Documents/GitHub/MarmotsTuesday/Altered/liste_cartes_20250617.xlsx")

TabBase = TabBase[TabBase["Possédé (physique)"] < 3]

Commande = ""
TotalCartes = 0
TotalPrix = 0
for faction in ["Axiom", "Bravos", "Lyra", "Muna", "Ordis", "Yzmir"]:
    CartesFaction = 0
    TotalFaction = 0
    Commande += "- Faction : " + faction + "\n"
    for edition in ["Au-delà des portes", "Épreuve du froid", "Les Graines de l'Unité",
                        "Murmures du Labyrinthe", "Odyssée des cieux"]:
        TabFiltre = TabBase[(TabBase["Edition"] == edition) & (TabBase["Faction"] == faction)]
        Commande += "\n" + "  - Edition : " + edition + " :\n"
        if edition == "Les Graines de l'Unité":
            price = 1
        else:
            price = .5
        for _, r in TabFiltre.iterrows():
            Commande += "    - " + r["Nom"] + " : " + str(r["Manque (physique)"]) + '\n'
            TotalFaction += r["Manque (physique)"] * price
            CartesFaction += r["Manque (physique)"]
    Commande += "\nNombre de cartes dans la faction : " + str(CartesFaction) + "\nTotal de la faction : " + str(TotalFaction) + "€\n\n"
    TotalCartes += CartesFaction
    TotalPrix += TotalFaction
Commande += "\nAu total, " + str(TotalCartes) + " cartes pour " + str(TotalPrix) + "€"
        
with open("C:/Users/DRY12/Documents/GitHub/MarmotsTuesday/Altered/commande_guillaume.txt", "w", encoding = "utf-8") as f:
    f.write(Commande)
