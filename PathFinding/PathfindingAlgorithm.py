"""
Functions for pathfinding algorithms
"""

class PathFinder:
    """
    Pathfinding class
    :param TypeAlgo: Type of pathfinding algorithm (dijkstra or astar)
    """
    def __init__(self, TypeAlgo):
        if TypeAlgo == "dijkstra":
            pass
        elif TypeAlgo == "astar":
            pass
        else:
            raise ValueError("Unsupported algorithm")

def VoisinsCroix(rr, cc, RMax, CMax, RMin=0, CMin=0):
    """
    Function that find coordinates of neighbours in cross
    :param rr: row
    :param cc: column
    :param RMin, CMin: Minimum index of row and column
    :param RMax, CMax: Maximum index of row and column
    :return: list of tuples (row, column) of the neighbours
    """
    Res = []
    Directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    for re, ce in Directions:
        rvois = rr + re
        cvois = cc + ce
        if 
