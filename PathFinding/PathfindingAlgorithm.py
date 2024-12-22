"""
Functions for pathfinding algorithms
"""

import numpy as np
import heapq

class PathFinder:
    """
    Pathfinding class
    :param TypeAlgo: Type of pathfinding algorithm (dijkstra or astar)
    :param FunVois: Function to retrieve neighbours
    :param Carte: Map of the world (# for walls impossible to cross, double number for weight). A numpy array.
    :param Rayon: Radius to search for neighbours
    """
    def __init__(self, TypeAlgo, FunVois, Carte, Rayon):
        if TypeAlgo not in ["dijkstra", "astar"]:
            raise ValueError("Unsupported algorithm")
        self.MapWeights = Carte
        self.algo = TypeAlgo
        self.FunVois = FunVois
        self.nrows, self.ncols = Carte.shape
        self.radius = Rayon
        self.adjacencies = dict()
        for i in range(self.nrows):
            for j in range(self.ncols):
                

def VoisinsCroix(rr, cc, RMax, CMax, RMin=0, CMin=0, Longueur=1):
    """
    Function that find coordinates of neighbours in cross
    :param rr: row
    :param cc: column
    :param RMin, CMin: Minimum index of row and column
    :param RMax, CMax: Maximum index of row and column
    :return: list of tuples (row, column) of the neighbours
    """
    Res = []
    Directions = []
    for i in range(-Longueur, Longueur + 1):
        for j in range(-Longueur, Longueur + 1):
            if (i == 0) ^ (j == 0): Directions.append((i, j))
    for re, ce in Directions:
        rvois = rr + re
        cvois = cc + ce
        if rvois in range(RMin, RMax) and cvois in range(CMin, CMax):
            Res.append((rvois, cvois))
    return Res

def VoisinsAnneau(rr, cc, RMax, CMax, RMin=0, CMin=0, Longueur=1):
    """
    Function that find coordinates of neighbours in cross
    :param rr: row
    :param cc: column
    :param RMin, CMin: Minimum index of row and column
    :param RMax, CMax: Maximum index of row and column
    :param Longueur: distance of the ring from center
    :return: list of tuples (row, column) of the neighbours
    """
    Res = []
    Directions = []
    for i in range(-Longueur, Longueur + 1):
        for j in range(-Longueur, Longueur + 1):
            if (i, j) != (0, 0): Directions.append((i, j))
    for re, ce in Directions:
        rvois = rr + re
        cvois = cc + ce
        if rvois in range(RMin, RMax) and cvois in range(CMin, CMax):
            Res.append((rvois, cvois))
    return Res

import matplotlib.pyplot as plt
import numpy as np
Map = np.array([[0 for _ in range(10)] for _ in range(10)])
Ex = VoisinsCroix(9, 5, 10, 10, 0, 0, 2)
Map[(9, 5)] = 2
for i in Ex:
    Map[i] = 1
plt.imshow(Map)
plt.show()