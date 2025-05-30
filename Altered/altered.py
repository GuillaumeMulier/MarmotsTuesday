import matplotlib.pyplot as plt
import numpy as np
import itertools as it

# Réarrangement des tumultes
Tumultes = ["23", "45", "67"]
Possibilites = list(it.permutations(Tumultes, 3))
Res = []
for p in Possibilites:
    ResTemp = [""]
    for t in p:
        ResTemp = [r + t for r in ResTemp] + [r + t[::-1] for r in ResTemp]
    Res.extend(ResTemp)
for r in range(len(Res)):
    Res[r] = "1" + Res[r] + "8"
print("On trouve " + str(len(Res)) + " tumultes différents.")

# Trouver les positions possibles des héros et compagnons
Positions = [(h, c) for h in range(8) for c in range(8) if c > h]
print("On trouve " + str(len(Positions)) + " positions possibles pour un joueur donné.")

# Simuler tous les cas possibles de combats
def PasteResult(p1, p2, tum):
    return tum[p1[0]] + tum[p1[1]] + tum[p2[0]] + tum[p2[1]]
CombatsPossibles = {}
for tumulte in Res:
    for player1 in Positions:
        for player2 in Positions:
            Combat = PasteResult(player1, player2, tumulte)
            if Combat in CombatsPossibles:
                CombatsPossibles[Combat] += 1
            else:
                CombatsPossibles[Combat] = 1
print("Il y a au total " + str(len(CombatsPossibles)) + " configurations possibles héros/compagnons pour les 2 joueurs, soit " + str(16 * len(CombatsPossibles)) + " combats possibles avec 2 cartes dans la main de chaque joueur.")

keys = sorted(CombatsPossibles.keys())
vals = [CombatsPossibles[clef] for clef in keys]
plt.bar(keys, vals, color = "steelblue")
plt.xlabel("Combat")
plt.ylabel("Effectif")
plt.title("Répartition des différents combats possibles")
plt.show()

