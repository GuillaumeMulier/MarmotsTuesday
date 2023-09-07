import pygame as pg
import numpy as np

# Variables globales
Dimensions = (900, 600)
fps = 30
CouleurFond = (4, 10, 45)
AigueMarine = (121, 248, 248)
Noir = (0, 0, 0)
Blanc = (255, 255, 255)
BtnSombre = (2, 55, 10)
BtnClair = (32, 120, 45)

# Configuration
pg.init()
fpsClock = pg.time.Clock()

def ConvertirCoord(x, y):
    return x, Dimensions[1] - y

class Bouton:
    def __init__(self, x, y, h, l, label, police="georgia", couleur=BtnSombre, couleur_act=BtnClair):
        self.x, self.y = ConvertirCoord(x, y)
        self.h = h
        self.l = l
        self.police = police
        self.label = label
        self.couleur = couleur
        self.couleur_act = couleur_act
        self.action = False
        self.last_time = pg.time.get_ticks()
        return None
    def draw(self, souris, btn_click, couleur_police = Noir):
        if self.x < souris[0] < self.x + self.l and self.y < souris[1] < self.y + self.h:
            Couleur = self.couleur_act
            if btn_click:
                self.action = not self.action
                pg.time.wait(250)
        else:
            Couleur = self.couleur
        PoliceText = pg.font.SysFont(self.police, int(self.h / 2))
        SurfaceText = PoliceText.render(self.label, True, couleur_police)
        RectangleText = SurfaceText.get_rect()
        RectangleText.center = (self.x + self.l / 2, self.y + self.h / 2)
        pg.draw.rect(Ecran, Couleur, (self.x, self.y, self.l, self.h))
        pg.draw.lines(Ecran, (0, 0, 0), True,
                      [(self.x, self.y), (self.x + self.l, self.y),
                       (self.x + self.l, self.y + self.h), (self.x, self.y + self.h)])
        Ecran.blit(SurfaceText, RectangleText)
        return None

class ChaosGame:
    def __init__(self, pointcentral=(min(Dimensions) / 2, min(Dimensions) / 2)):
        self.pointsx = np.array([])
        self.pointsy = np.array([])
        self.pointcurrent = pointcentral
        return None
    def add_pts(self, point):
        if len(self.pointsx) == 3:
            self.pointsx = np.append(self.pointsx[1:3], point[0])
            self.pointsy = np.append(self.pointsy[1:3], point[1])
        else:
            self.pointsx = np.append(self.pointsx, point[0])
            self.pointsy = np.append(self.pointsy, point[1])
        return None
    def draw_pts(self):
        self.draw_scene()
        for px, py in zip(self.pointsx, self.pointsy):
            pg.draw.circle(Ecran, AigueMarine, (px, py), 1)
        return None
    def draw_scene(self):
        Cote = min(Dimensions) - 100
        Ecran.fill(CouleurFond)
        pg.draw.rect(PetitEcran, (255, 0, 0), (0, 0, Cote, Cote), 2)
        return None
    def generate_pts(self):
        Choix = np.random.randint(0, len(self.pointsx))
        NvPt = ((self.pointsx[Choix] + self.pointcurrent[0]) / 2, (self.pointsy[Choix] + self.pointcurrent[1]) / 2)
        pg.draw.circle(Ecran, AigueMarine, NvPt, 1)
        self.pointcurrent = NvPt
        return None


Btn = Bouton(600, 100, 40, 150, "On teste")
Jeu = ChaosGame()
Ecran = pg.display.set_mode(Dimensions)
PetitEcran = Ecran.subsurface((50, 50, min(Dimensions) - 100, min(Dimensions) - 100))
Ecran.fill(CouleurFond)
Jeu.draw_scene()

while True:
    Souris = pg.mouse.get_pos()
    for event in pg.event.get():
        if event.type == pg.QUIT:
            pg.quit()
        if event.type == pg.MOUSEBUTTONDOWN:
            if (50 < Souris[0] < 50 + min(Dimensions) - 100) and (50 < Souris[1] < 50 + min(Dimensions) - 100):
                Jeu.add_pts(pg.mouse.get_pos())
                Jeu.draw_pts()
    Btn.draw(Souris, pg.mouse.get_pressed()[0])
    if Btn.action:
        for i in range(5):
            Jeu.generate_pts()
    fpsClock.tick(fps)
    pg.display.flip()





"""
import alive_progress as ap
import time as time

t1 = time.time()
np.random.seed(121221)
compte = 0
with ap.alive_bar(int(1e+7)) as PBarre:
    for i in range(int(1e+7)):
        if np.random.uniform(0, 1, 1) < .5:
            compte += 1
        PBarre()
print(f"En moyenne, on en a {round(100 * compte / 1e+7, 4)}% inférieurs à 0.5 !")
print(f"En {time.time() - t1} secondes.")

t1 = time.time()
np.random.seed(121221)
print(f"En moyenne, on en a {round(100 * sum(np.random.uniform(0, 1, int(1e+7)) < .5) / 1e+7, 4)}% inférieurs à 0.5 !")
print(f"En {time.time() - t1} secondes cette fois.")

"""