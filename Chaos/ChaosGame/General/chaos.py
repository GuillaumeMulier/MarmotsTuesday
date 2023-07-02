"""import pygame
import time

pygame.init()

display_width = 800
display_height = 600

black = (0, 0, 0)
white = (255, 255, 255)
red = (255, 0, 0)
green = (0, 255, 0)

car_width = 73

gameDisplay = pygame.display.set_mode((display_width, display_height))
pygame.display.set_caption('A bit Racey')
clock = pygame.time.Clock()

#carImg = pygame.image.load('racecar.png')


def car(x, y):
    gameDisplay.blit(carImg, (x, y))


def text_objects(text, font):
    textSurface = font.render(text, True, black)
    return textSurface, textSurface.get_rect()


def message_display(text):
    largeText = pygame.font.Font('freesansbold.ttf', 115)
    TextSurf, TextRect = text_objects(text, largeText)
    TextRect.center = ((display_width / 2), (display_height / 2))
    gameDisplay.blit(TextSurf, TextRect)

    pygame.display.update()

    time.sleep(2)

    game_loop()


def crash():
    message_display('You Crashed')
def game_intro():
    intro = True

    while intro:
        for event in pygame.event.get():
            print(event)
            if event.type == pygame.QUIT:
                pygame.quit()
                quit()

        gameDisplay.fill(white)
        largeText = pygame.font.Font('freesansbold.ttf', 115)
        TextSurf, TextRect = text_objects("A bit Racey", largeText)
        TextRect.center = ((50 / 2), (50 / 2))
        gameDisplay.blit(TextSurf, TextRect)

        pygame.draw.rect(gameDisplay, green, (150, 450, 100, 50))
        pygame.draw.rect(gameDisplay, red, (550, 450, 100, 50))

        pygame.display.update()
        clock.tick(15)

game_intro()

"""

import pygame as pg

# Variables globales
Dimensions = (800, 600)

# Configuration
pg.init()
fps = 60
fpsClock = pg.time.Clock()

font = pg.font.SysFont('gillsans', 40)

class Bouton:
    def __init__(self, x, y, h, l, police, label, couleur):
        self.x = x
        self.y = y
        self.h = h
        self.l = l
        self.police = police
        self.label = label
        self.couleur = couleur
        return None
    def draw(self):
        pg.draw.rect(Ecran, self.couleur, (self.x, self.y, self.l, self.h))
        return None

Btn = Bouton(20, 20, 20, 20, "gillsans", "On teste", (255, 0, 0))
Btn2 = Bouton(20, 60, 20, 20, "gillsans", "On teste", (0, 0, 0))
Btn3 = Bouton(60, 20, 60, 20, "gillsans", "On teste", (0, 255, 0))
Btn4 = Bouton(60, 60, 20, 60, "gillsans", "On teste", (0, 0, 255))


objects = []
polices = pg.font.get_fonts()


width, height = 1000, 800
Ecran = pg.display.set_mode((width, height))

while True:
    Ecran.fill((255, 255, 255))
    for event in pg.event.get():
        if event.type == pg.QUIT:
            pg.quit()
            sys.exit()

    Btn.draw()
    Btn2.draw()
    Btn3.draw()
    Btn4.draw()
    pg.display.flip()
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