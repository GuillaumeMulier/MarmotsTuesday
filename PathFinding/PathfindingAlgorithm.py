"""
Functions for pathfinding algorithms
"""

import numpy as np
import heapq

class PathFinder:
    """
    Pathfinding class
    :param Start, End: start and end positions
    :param TypeAlgo: Type of pathfinding algorithm (dijkstra or astar)
    :param FunVois: Function to retrieve neighbours
    :param Carte: Map of the world (# for walls impossible to cross, double number for weight). A numpy array.
    :param Rayon: Radius to search for neighbours
    :param UniquePath: Boolean, want to return a unique path or the list of paths that are the smallest paths
    :param StoreOrder: Boolean, want to store the ordre of visit of nodes
    """
    def __init__(self, Start, End, TypeAlgo, FunVois, Carte, Rayon):

        # Store all values and control if there are errors
        if TypeAlgo not in ["dijkstra", "double-dijkstra", "astar"]:
            raise ValueError("Unsupported algorithm")
        self.MapWeights = Carte
        self.algo = TypeAlgo
        self.FunVois = FunVois
        self.nrows, self.ncols = Carte.shape
        self.start = Start
        self.end = End
        if self.start[0] not in range(self.nrows) or self.start[1] not in range(self.ncols):
            raise ValueError("Starting point not in the supplied grid. Supply coherent values")
        if self.end[0] not in range(self.nrows) or self.end[1] not in range(self.ncols):
            raise ValueError("Ending point not in the supplied grid. Supply coherent values")
        self.radius = Rayon
        self.optimized = False

        # Create the dictionary that will store the neighbours for each cell with weights
        self.adjacencies = dict()
        for i in range(self.nrows):
            for j in range(self.ncols):
                if self.MapWeights[i, j] != "#":
                    Voisins = self.FunVois(i, j, self.nrows, self.ncols, 0, 0, self.radius)
                    self.adjacencies[(i, j)] = dict()
                    for ii, jj in Voisins:
                        if self.MapWeights[ii, jj] != "#":
                            self.adjacencies[(i, j)][(ii, jj)] = int(self.MapWeights[ii, jj])

    def ResolvePath(self, UniquePath=True, StoreOrder=False):

        # Actually perform the pathfinding algorithm
        if self.algo == "dijkstra": # Dijkstra algorithm from starting point
            CheminEnCours = []
            heapq.heappush(CheminEnCours, (0, self.start, [self.start]))
            if StoreOrder: self.ordre = []
            Distances = dict()
            OptimalDist = float("inf")
            while len(CheminEnCours) > 0:
                Distance, Position, Chemin = heapq.heappop(CheminEnCours)
                if StoreOrder: self.ordre.append(Position)
                # If we have a minimal distance that is greater than our shortest path, no need to continue
                if Distance > OptimalDist: break
                if Position == self.end: # We found a solution
                    if Distance < OptimalDist: # We found the first shortest path
                        Chemin.append(self.end)
                        CheminFini = [(Distance, Position, Chemin)]
                    elif Distance == OptimalDist: # We found another shortest path
                        Chemin.append(self.end)
                        CheminFini.append((Distance, Position, Chemin))
                else:
                    for node, (dist, dir) in AdjDict[Position].items():
                        if node in Distances:
                            if Distance + dist > Distances[node]: continue
                            Distances[node] = Distance + dist
                            heapq.heappush(TouchesEnCours, (Distance + dist, node, Chemin + dir))
                        else:
                            Distances[node] = Distance + dist
                            heapq.heappush(TouchesEnCours, (Distance + dist, node, Chemin + dir))
            return CheminFini

    def __repr__(self):
        if not self.optimized:
            return f"Chosen algorithm: {self.algo}.\nFrom {self.start} to {self.end} in a {self.nrows}x{self.ncols} grid.\nUse ResolvePath() to find best way!"



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
Map = np.array([[1 for _ in range(3)] for _ in range(3)])
Map[(1, 2)] = 2

Ex = VoisinsCroix(9, 5, 10, 10, 0, 0, 2)
Map[(9, 5)] = 2
for i in Ex:
    Map[i] = 1
plt.imshow(Map)
plt.show()