import random as rd
import numpy as np

Tuiles = ["OK"] * 3 + ['Pas OK'] * 21

rd.seed(121221)

NSimu = int(1e+5)
NbMains = 0

for _ in range(NSimu):
    Main12 = rd.sample(Tuiles, k = 12)
    if Main12.count("OK") == 3: NbMains += 1

print(f"Il y a eu {NbMains} mains de 12 cartes avec les 3 cartes en question, soit {round(100 * NbMains / NSimu, 1)}% des mains !")