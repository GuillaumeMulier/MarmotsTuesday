"""
Functions for pathfinding algorithms
"""

import numpy as np
import heapq
import matplotlib.pyplot as plt
import random as rd
from matplotlib.colors import LinearSegmentedColormap, ListedColormap
import io
from PIL import Image

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
            raise ValueError("Unsupported algorithm. Select one from 'dijkstra', 'double-dijkstra' or 'astar'!")
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
        self.ordre = []

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

        # Create map of weights for plotting purposes
        self.weights = np.array([[0 for _ in range(self.MapWeights.shape[1])] for _ in range(self.MapWeights.shape[0])])
        for j in range(self.weights.shape[1]):
            for i in range(self.weights.shape[0]):
                if self.MapWeights[i, j] == "#":
                    self.weights[i, j] = -1
                else:
                    self.weights[i, j] = int(self.MapWeights[i, j])

    def ResolvePath(self, StoreOrder = False):

        # Actually perform the pathfinding algorithm
        if self.algo == "dijkstra": # Dijkstra algorithm from starting point
            CheminEnCours = []
            CheminFini = []
            heapq.heappush(CheminEnCours, (0, self.start, [self.start]))
            Distances = dict()
            OptimalDist = float("inf")
            if StoreOrder: frames = []
            while len(CheminEnCours) > 0:
                Distance, Position, Chemin = heapq.heappop(CheminEnCours)
                if StoreOrder: frames.append(Chemin)
                # If we have a minimal distance that is greater than our shortest path, no need to continue
                if Distance > OptimalDist: break
                if Position == self.end: # We found a solution
                    if Distance < OptimalDist: # We found the first shortest path
                        CheminFini = [(Distance, [*Chemin, self.end])]
                    elif Distance == OptimalDist: # We found another shortest path
                        CheminFini.append((Distance, [*Chemin, self.end]))
                else: # We did not find a solution so we continue with neighbours of the node
                    for node, dist in self.adjacencies[Position].items():
                        if node in Distances:
                            if Distance + dist >= Distances[node]: continue
                            Distances[node] = Distance + dist
                        else:
                            Distances[node] = Distance + dist
                        heapq.heappush(CheminEnCours, (Distance + dist, node, [*Chemin, node]))
            self.optimized = True
            self.optimized_path = CheminFini
            if StoreOrder: self.ordre = frames
        elif self.algo == "double-dijkstra": # Dijsktra from start and end simultaneously
            CheminEnCours = []
            CheminFini = []
            heapq.heappush(CheminEnCours, (0, self.start, [self.start], True))
            heapq.heappush(CheminEnCours, (0, self.end, [self.end], False))
            VisitedStart = {self.start: (0, [])}
            VisitedEnd = {self.end: (0, [])}
            Distances = dict()
            OptimalDist = float("inf")
            if StoreOrder: frames = []
            while len(CheminEnCours) > 0:
                Distance, Position, Chemin, FromStart = heapq.heappop(CheminEnCours)
                if StoreOrder: frames.append(Chemin)
                # If we have a minimal distance that is greater than our shortest path, no need to continue
                if Distance > OptimalDist: break
                if (FromStart and Position in VisitedEnd) or (not FromStart and Position in VisitedStart):  # We found a solution (Both trace come back together)
                    if FromStart: # Compute distance of the whole path
                        DistanceTot = Distance + VisitedEnd[Position][0]
                    else:
                        DistanceTot = Distance + VisitedStart[Position][0]
                    if DistanceTot < OptimalDist:  # We found the first shortest path
                        if FromStart:
                            CheminFini = [(DistanceTot, [*Chemin, *VisitedEnd[Position][1]])]
                        else:
                            CheminFini = [(DistanceTot, [*VisitedStart[Position][1], *Chemin[::-1]])]
                    elif DistanceTot == OptimalDist:  # We found another shortest path
                        CheminFini.append((Distance, Position, [*Chemin, self.end]))
                else:  # We did not find a solution so we continue with neighbours of the node
                    for node, dist in self.adjacencies[Position].items():
                        if node in Distances:
                            if Distance + dist >= Distances[node]: continue
                            Distances[node] = Distance + dist
                        else:
                            Distances[node] = Distance + dist
                        heapq.heappush(CheminEnCours, (Distance + dist, node, [*Chemin, node]))
            self.optimized = True
            self.optimized_path = CheminFini
            if StoreOrder: self.ordre = frames


    def __repr__(self):
        if not self.optimized:
            return f"Chosen algorithm: {self.algo}.\nFrom {self.start} to {self.end} in a {self.nrows}x{self.ncols} grid.\nUse ResolvePath() to find best way!"
        else:
            return f"Chosen algorithm: {self.algo}.\nFrom {self.start} to {self.end} in a {self.nrows}x{self.ncols} grid.\nFound path of length {self.optimized_path[0]}."

    def VisualizePath(self, ColorPalette, Path = None, GifPath = None):
        if not self.optimized_path:
            print(f"The path has not yet been found. Use ResolvePath() method to find it!")
        elif len(self.optimized_path) == 0:
            print(f"There is no path between {self.start} and {self.end}...")
        else:
            x, y = zip(*self.optimized_path[0][1])
            fig, ax = plt.subplots()
            fig.subplots_adjust(left = .05, right = .95, bottom = .05, top = .9)
            im = ax.imshow(self.weights, cmap = ColorPalette, alpha = .5)
            ax.axis("off")
            ax.set_title(f"Optimal path from {self.start} to {self.end} with {self.algo} algorithm.")
            ax.plot(y, x, color = "steelblue", linewidth = 1)
            plt.show()
            print(f"Static image has been rendered")
            if not Path is None:
                fig.savefig(Path, dpi = 300, bbox_inches = "tight", transparent = False, facecolor = "white")
                print(f"Image saved at {Path}")
            if not GifPath is None:
                if len(self.ordre) == 0:
                    print(f"No stored order, unable to make a gif...")
                else:
                    Progression = set([int(x) for x in np.linspace(1, len(Ex.ordre), 30)])
                    fig, ax = plt.subplots()
                    ax.imshow(self.weights, cmap = ColorPalette, alpha = .5)
                    ax.axis("off")
                    ax.set_title(f"Optimal path from {self.start} to {self.end} with {self.algo} algorithm.")
                    line, = ax.plot([], [], color="steelblue", linewidth=1)
                    frames = []
                    for i in range(len(self.ordre)):
                        points = self.ordre[i]
                        if i in Progression: print(f"Frame {i} of the total of {len(self.ordre)} ({round(100 * i / len(self.ordre), 2)})%!")
                        x, y = zip(*points)
                        line.set_data(y, x)
                        # Sauvegarder l'image courante dans un tampon
                        buf = io.BytesIO()
                        plt.savefig(buf, format="png", bbox_inches="tight")
                        buf.seek(0)
                        frames.append(Image.open(buf))
                frames[0].save(
                    GifPath,
                    save_all=True,
                    append_images=frames[1:],
                    duration=1,
                    loop=0
                )
                print(f"Gif saved at {GifPath}")



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


Map = np.array([["1" for _ in range(100)] for _ in range(120)])
rd.seed(121221)
for i in range(5000):
    x = rd.randint(0, 99)
    y = rd.randint(0, 119)
    Map[y, x] = "2"
for i in range(1000):
    x = rd.randint(0, 99)
    y = rd.randint(0, 119)
    Map[y, x] = "4"
for i in range(2000):
    x = rd.randint(0, 99)
    y = rd.randint(0, 119)
    Map[y, x] = "#"
Map[0, 0] = "1"


PaletteCustom = LinearSegmentedColormap.from_list("PaletteCustom", ["#30c208", "#1c38ea"])
PaletteCustom = ListedColormap(["#502b03"] + [PaletteCustom(i) for i in range(PaletteCustom.N)])


Ex = PathFinder((0, 0), (119, 99), "dijkstra", VoisinsCroix, Map, 1)
Ex.ResolvePath(StoreOrder = True)

Ex.VisualizePath(PaletteCustom,
                 Path = "C:/Users/DRY12/Documents/GitHub/MarmotsTuesday/PathFinding/exemple_path.png",
                 GifPath = "C:/Users/DRY12/Documents/GitHub/MarmotsTuesday/PathFinding/exemple_path.gif")

