library(tidyverse)
library(imager)
library(Rcpp)
library(magick)
library(torchvision)

# sourceCpp("C:/Users/gmulier/Documents/Github/MarmotsTuesday/Images/string_algorithm_v2.cpp")
sourceCpp("C:/Users/DRY12/Documents/Github/MarmotsTuesday/Images/string_algorithm_v2.cpp")

CirclePins <- function(NbPins, DimX, DimY) {
  Angles <- seq(0, 2 * pi, length.out = NbPins + 1)
  Angles <- Angles[-length(Angles)]
  tibble(num = seq_along(Angles), x = cos(Angles), y = sin(Angles)) %>% 
    mutate(x = map_dbl(x, ~ round((.x + 1) / 2 * DimX, 0)), 
           y = map_dbl(y, ~ round((.x + 1) / 2 * DimY, 0)))
}

# ImgEster <- load.image("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/photo_ester.JPG")
ImgEster <- load.image("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/ester3.jpg")
# 
# transform_adjust_contrast(image_scale(ImgEster, "600x600"), 1)
# ImgMarmotte <- load.image("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/tete_panthere.jpg")
# ImgEster <- imsub(ImgEster, y > 700 & y < 3500)
ImgEster <- imsub(ImgEster, y > 100 & y < 1450)
# ImgEster <- imsub(ImgEster, x > 1800 & x < 4500)
ImgEster <- imsub(ImgEster, x > 50 & x < 1550)
ImgEster <- resize(ImgEster, size_x = 800, size_y = 800)
ImgEster <- grayscale(ImgEster) %>% as.matrix()
# ImgMarmotte <- ImgMarmotte ** 2.5 # Pour réhausser le contraste
ImgEster <- 1 - ImgEster
ImgEster <- ImgEster ** .5
par(pty = "s")
image(ImgEster, zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))

Cercle <- CirclePins(140, 799, 799)

StringEster <- GenThread(ImgEster, matrix(1, 800, 800), 
                         Cercle$x, Cercle$y, FALSE,
                         .15, 4000, 560 / 800,
                         "anti-alias")
image(matrix(StringEster$Img, 800, 800), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
print(StringEster$Dist / 1000)
StringEster$Instructions[rowSums(StringEster$Instructions) != 0, ] %>% write.csv("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/instruction_ester_v2.csv")

ImgKing <- image_read("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/pngegg.png")
ImgKing <- magick2cimg(image_background(ImgKing, "#646af0"))
ImgKing <- imsub(ImgKing, y < 450)
ImgKing <- imsub(ImgKing, x < 550 & x > 100)
ImgKing <- resize(ImgKing, size_x = 400, size_y = 400)
ImgKing <- grayscale(ImgKing) %>% as.matrix()
ImgKing <- 1 - ImgKing
ImgKing <- ImgKing ** .35
par(pty = "s")
image(ImgKing, zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
StringKing <- GenThread(ImgKing,
                        Cercle$x, Cercle$y, TRUE,
                        .15, 4000, 560 / 400,
                        "anti-alias")
image(matrix(StringKing$Img, 400, 400), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
print(StringKing$Dist / 1000)
StringEster$Instructions[rowSums(StringEster$Instructions) != 0, ] %>% write.csv("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/instruction_ester.csv")



ImgPanthere <- load.image("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/tigre.jpg")
# ImgPanthere <- imsub(ImgPanthere, x > 20 & x < 160)
# ImgPanthere <- imsub(ImgPanthere, y > 40 & y < 180)
ImgPanthere <- resize(ImgPanthere, size_x = 800, size_y = 800)
ImgPanthere <- grayscale(ImgPanthere) %>% as.matrix()
# ImgMarmotte <- ImgMarmotte ** 2.5 # Pour réhausser le contraste
ImgPanthere <- 1 - ImgPanthere
ImgPanthere <- ImgPanthere ** .5
image(ImgPanthere, zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))

Cercle <- CirclePins(140, 799, 799)

StringPant <- GenThread(ImgPanthere, matrix(1, 800, 800),
                  Cercle$x, Cercle$y, FALSE,
                  .2, 5000, 560 / 800,
                  "anti-alias")
image(matrix(StringPant$Img, 800, 800), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
print(StringPant$Dist / 1000)
StringPant$Instructions[rowSums(StringPant$Instructions) != 0, ] %>% write.csv("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/instruction_panthere.csv")

ImgKarma <- load.image("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/jinx.jpg")
# ImgKarma <- imsub(ImgKarma, x > 50 & x < 210)
ImgKarma <- imsub(ImgKarma, y < 2000)
ImgKarma <- resize(ImgKarma, size_x = 800, size_y = 800)
ImgKarma <- grayscale(ImgKarma) %>% as.matrix()
# ImgMarmotte <- ImgMarmotte ** 2.5 # Pour réhausser le contraste
ImgKarma <- 1 - ImgKarma
ImgKarma <- ImgKarma ** .75
image(ImgKarma, zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))

Cercle <- CirclePins(180, 799, 799)

Test <- GenThread(ImgKarma,
                  Cercle$x, Cercle$y, TRUE,
                  .3, 5000, 560 / 800,
                  "anti-alias")
image(matrix(Test$Img, 800, 800), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
print(Test$Dist / 1000)
Test$Instructions[rowSums(Test$Instructions) != 0, ] %>% write.csv("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/instruction_jinx.csv")

# On charge l'image et on la met au bon format / en noir et blanc
Levi <- load.image("C:/Users/DRY12/Documents/GitHub/MarmotsTuesday/Images/levi.jpg")
Levi <- grayscale(Levi) %>% 
  imsub(y < 150) %>% 
  imsub(x > 20 & x < 220) %>% 
  resize(size_x = 800, size_y = 800)
# On va essayer de réhausser le contraste sur les endroits de transition
PoidsLevi <- Levi
PoidsLevi <- PoidsLevi %>% isoblur(2) %>% imgradient("xy")
PoidsLevi <- sqrt(PoidsLevi$x ** 2 + PoidsLevi$y ** 2)
PoidsLevi <- (as.matrix(PoidsLevi) > .01) * .5 + .5
# ErenTitan[Eren2 > .03] <- ErenTitan[Eren2 > .03] ** 2 # Abandon de cette partie qui n'a pas l'air d'apporter de précision en plus
# On transforme l'image en matrice que la fonction comprend
Levi <- as.matrix(Levi)
Levi <- 1 - Levi
Levi <- Levi ** .7
# # Définition de la matrice des poids. Je pense prendre la matrice des gradients pour ça
# Eren2 <- as.matrix(Eren2)
# Eren2 <- ifelse(Eren2 > .03, 1, ((Eren2 - min(Eren2)) / (.03 - min(Eren2)) + 9) / 10)
# On lance le calcul des fils
Cercle <- CirclePins(133, 799, 799)
StringEren <- GenThread(ErenTitan, Eren2, 
                         Cercle$x, Cercle$y, TRUE,
                         .15, 4000, 560 / 800,
                         "anti-alias")
StringEren <- GenThread(ErenTitan, matrix(1, 800, 800), 
                         Cercle$x, Cercle$y, TRUE,
                         .15, 4000, 560 / 800,
                         "anti-alias")






image(matrix(StringEren$Img, 800, 800), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
print(StringEren$Dist / 1000)

hist(Eren2) 
min(Eren2)
max(Eren2)

image(Levi, zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
image(transform_adjust_contrast(Levi, 2), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))

plot(ErenTitan)
plot(Eren2 > .01)
