library(tidyverse)
library(imager)
library(Rcpp)
library(magick)
library(shiny)
library(plotly)
library(progressr)

sourceCpp("C:/Users/gmulier/Documents/Github/MarmotsTuesday/Images/string_algorithm_v2.cpp")
# sourceCpp("C:/Users/DRY12/Documents/Github/MarmotsTuesday/Images/string_algorithm_v2.cpp")

CirclePins <- function(NbPins, DimX, DimY) {
  Angles <- seq(0, 2 * pi, length.out = NbPins + 1)
  Angles <- Angles[-length(Angles)]
  tibble(num = seq_along(Angles), x = cos(Angles), y = sin(Angles)) %>% 
    mutate(x = map_dbl(x, ~ round((.x + 1) / 2 * DimX, 0)), 
           y = map_dbl(y, ~ round((.x + 1) / 2 * DimY, 0)))
}

AppliProcessImage <- function() {
  
  ui <- fluidPage(
    
    title = "Processing des images avant d'appliquer l'algorithme pour les fils",
    
    sidebarLayout(
      # Les paramtètres à changer et boutons pour valider
      sidebarPanel(
        h3("Importer l'image"),
        fileInput("fichier", "Fichier:", accept = c(".jpg", ".jpeg", ".png")),
        br(),
        h3("Sélectionner la plage à conserver"),
        textOutput("infos"),
        br(),
        h3("Output"),
        numericInput("hauteur", "Hauteur (px)", min = 0, value = 800),
        numericInput("largeur", "Largeur (px)", min = 0, value = 800),
        sliderInput("contraste", "Ajustement de contraste", min = .05, max = 10, step = .05, value = 1),
        checkboxInput("histo", "Egalisation de l'histogramme des couleurs"),
        sliderInput("puissance", "Exposant pour l'ajustement de luminosité", value = 1, min = 0, max = 10, step = .25),
        actionButton("process", "Processing de l'image"),
        br(),
        h3("Quand on a fini"),
        actionButton("arret", "Fin du processing")
      ),
      # L'affichage de l'image en plotly
      mainPanel(
        h3("Image de base"),
        plotlyOutput("image_base", height = 500, width = 500),
        br(),
        h3("Image modifiée"),
        plotOutput("plot", height = 500, width = 500)
      )
    )
    
  )
  
  server <- function(input, output, session) {
    
    # 3 objects réactifs : 1 pour l'image, 1 pour les coordonnées, et 1 pour la matrice après processing (contraste et refaire les bordures)
    ImageBase <- reactiveVal(NULL)
    MatriceImage <- reactiveVal(NULL)
    Coordonnees <- reactiveVal(NULL)
    
    # Charger l'image dans ImageBase()
    observeEvent(input$fichier, {
      if (!is.null(input$fichier)) {
        ImageBase(load.image(input$fichier$datapath))
      }
    })
    
    # Quand on change d'image, ça remet à 0 la matrice
    observeEvent(ImageBase(), {
      MatriceImage(NULL)
    })
    
    # Afficher l'image non modifiée
    EchelleCouleurs <- setNames(data.frame(seq(0, 1, length.out = 100), rev(colorRampPalette(c("white", "black"))(100))), NULL)
    output$image_base <- renderPlotly({
      if (!is.null(ImageBase())) {
        plot_ly(z = t(as.matrix(grayscale(ImageBase()))), type = "heatmap", colorscale = EchelleCouleurs, showscale = FALSE) %>% 
          layout(yaxis = list(autorange = "reversed"), dragmode = "select") %>%
          config(modeBarButtonsToAdd = list("drawrect", "eraseshape"))
      }
    })
    
    # Coordonnées des points du rectangle sélectionné
    output$infos <- renderText({
      d <- event_data("plotly_relayout")
      if (is.null(d)) {
        "Pas de zone sélectionnée !"
      } else {
        if (is.null(d$shapes)) {
          "Pas de zone sélectionnée !"
        } else {
          Coordonnees(c(floor(d$shapes$x0), ceiling(d$shapes$x1), floor(d$shapes$y0), ceiling(d$shapes$y1)))
          paste0("Zone sélectionnée : x entre ", Coordonnees()[1], " et ", Coordonnees()[2],
                 " / y entre ", Coordonnees()[3], " et ", Coordonnees()[4],
                 ". Soit un rectangle de ", Coordonnees()[2] - Coordonnees()[1], " x ", Coordonnees()[4] - Coordonnees()[3])
        }
      }
    })
    
    # On fait tout le processing le l'image
    observeEvent(input$process, {
      req(input$process)
      ImageTemp <- load.image(input$fichier$datapath)
      ImageTemp <- imsub(ImageTemp, y > Coordonnees()[3] & y < Coordonnees()[4])
      ImageTemp <- imsub(ImageTemp, x > Coordonnees()[1] & x < Coordonnees()[2])
      ImageTemp <- resize(ImageTemp, size_x = input$largeur, size_y = input$hauteur)
      ImageTemp <- grayscale(ImageTemp) 
      ImageTemp <- cimg2magick(ImageTemp)
      ImageTemp <- image_contrast(ImageTemp, sharpen = input$contraste)
      if (input$histo) ImageTemp <- image_equalize(ImageTemp)
      ImageTemp <- magick2cimg(ImageTemp)
      ImageTemp <- as.matrix(ImageTemp)
      ImageTemp <- 1 - ImageTemp
      ImageTemp <- ImageTemp ** input$puissance
      MatriceImage(ImageTemp)
    })
    
    # On plot le résultat (réactivité de la reactiveVal qui aide)
    output$plot <- renderPlot({
      
      if (is.null(MatriceImage())) {
          ggplot() + theme_void()
      } else {
        image(MatriceImage(), zlim = c(0, 1), col = rev(EchelleCouleurs[, 2]), ylim = c(1, 0), xlim = c(1, 0))
      }
      
    })
    
    # Si on quitte l'appli, sauvegarde de la matrice des pixels pour l'image modifiée
    # Je ne sais pas pourquoi, je suis obligé de passer comme cela, sinon l'appli ne se lance pas
    observeEvent(input$arret, {
      assign("Resultat", MatriceImage(), envir = globalenv())
      stopApp()
    })
    
  }
  shinyApp(ui, server)
  
}

Resultat <- load.image("C:/Users/gmulier/Documents/Github/MarmotsTuesday/Images/ester4.jpg")
# Resultat <- resize(Resultat, size_x = 1000, size_y = 1969)
Resultat <- imsub(Resultat, y > 900 & y < 2550)
Resultat <- imsub(Resultat, x > 700 & x < 2500)
Resultat <- resize(Resultat, size_x = 800, size_y = 800)
Resultat <- grayscale(Resultat) 
Resultat <- cimg2magick(Resultat)
# Resultat <- image_contrast(Resultat, sharpen = 10)
Resultat <- image_equalize(Resultat)
Resultat <- magick2cimg(Resultat)
Resultat <- as.matrix(Resultat)
Resultat <- 1 - Resultat
Resultat <- Resultat ** .65


# save.image(Image, "C:/Users/gmulier/Documents/Github/MarmotsTuesday/Images/ester5_m.jpg", quality = 1)
# plot(Image)

# AppliProcessImage()

PoidsImg <- as.cimg(Resultat)
PoidsImg <- PoidsImg %>% isoblur(4) %>% imgradient("xy")
PoidsImg <- sqrt(PoidsImg$x ** 2 + PoidsImg$y ** 2)
Indices <- which(as.matrix(PoidsImg > .012), arr.ind = TRUE)
Cherche <- expand.grid(x = -4:4, y = -4:4)
Cherche <- Cherche[(abs(Cherche$x) + abs(Cherche$y)) == 4, ]
CasesFlood <- data.frame(lig = integer(nrow(Cherche) * nrow(Indices)), col = integer(nrow(Cherche) * nrow(Indices)))
Debut <- 1
with_progress({
  PBarre <- progressor(nrow(Indices))
  for (i in seq_len(nrow(Indices))) {
    Ligne <- Indices[i, 1]
    Colonne <- Indices[i, 2]
    Colonne <- Cherche$x + Colonne
    Ligne <- Cherche$y + Ligne
    Temp <- data.frame(Ligne, Colonne)
    names(Temp) <- c("lig", "col")
    Temp <- Temp[Temp$col > 0 & Temp$col < 801, ]
    Temp <- Temp[Temp$lig > 0 & Temp$lig < 801, ]
    CasesFlood[seq(Debut, Debut + nrow(Temp) - 1), ] <- Temp
    Debut <- Debut + nrow(Temp)
    PBarre()
  }
})
CasesFlood <- CasesFlood[!is.na(CasesFlood$lig), ]
CasesFlood <- CasesFlood[!duplicated(paste0(CasesFlood$lig, "-", CasesFlood$col)), ]
MatPoids <- matrix(FALSE, 800, 800)
MatPoids[as.matrix(CasesFlood)] <- TRUE
PoidsEster <- .6 + .4 * MatPoids
rm(Indices, Temp, Debut, PBarre, Ligne, Colonne)

Cercle <- CirclePins(133, 799, 799)

StringEster <- GenThread(Resultat, matrix(1, 800, 800), 
                         Cercle$x, Cercle$y, TRUE,
                         .15, 4000, 560 / 800,
                         "anti-alias")
image(matrix(StringEster$Img, 800, 800), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
StringEster <- GenThread(Resultat, PoidsEster, 
                         Cercle$x, Cercle$y, TRUE,
                         .15, 4000, 560 / 800,
                         "anti-alias")
image(matrix(StringEster$Img, 800, 800), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
print(StringEster$Dist / 1000)
StringEster$Instructions[rowSums(StringEster$Instructions) != 0, ] %>% write.csv("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/instruction_ester.csv")

# ImgKing <- image_read("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/pngegg.png")
# ImgKing <- magick2cimg(image_background(ImgKing, "#646af0"))
# ImgKing <- imsub(ImgKing, y < 450)
# ImgKing <- imsub(ImgKing, x < 550 & x > 100)
# ImgKing <- resize(ImgKing, size_x = 400, size_y = 400)
# ImgKing <- grayscale(ImgKing) %>% as.matrix()
# ImgKing <- 1 - ImgKing
# ImgKing <- ImgKing ** .35
# par(pty = "s")
# image(ImgKing, zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
# StringKing <- GenThread(ImgKing,
#                         Cercle$x, Cercle$y, TRUE,
#                         .15, 4000, 560 / 400,
#                         "anti-alias")
# image(matrix(StringKing$Img, 400, 400), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
# print(StringKing$Dist / 1000)
# StringEster$Instructions[rowSums(StringEster$Instructions) != 0, ] %>% write.csv("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/instruction_ester.csv")
# 
# 
# 
# ImgPanthere <- load.image("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/tigre.jpg")
# # ImgPanthere <- imsub(ImgPanthere, x > 20 & x < 160)
# # ImgPanthere <- imsub(ImgPanthere, y > 40 & y < 180)
# ImgPanthere <- resize(ImgPanthere, size_x = 800, size_y = 800)
# ImgPanthere <- grayscale(ImgPanthere) %>% as.matrix()
# # ImgMarmotte <- ImgMarmotte ** 2.5 # Pour réhausser le contraste
# ImgPanthere <- 1 - ImgPanthere
# ImgPanthere <- ImgPanthere ** .5
# image(ImgPanthere, zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
# 
# Cercle <- CirclePins(140, 799, 799)
# 
# StringPant <- GenThread(ImgPanthere, matrix(1, 800, 800),
#                         Cercle$x, Cercle$y, FALSE,
#                         .2, 5000, 560 / 800,
#                         "anti-alias")
# image(matrix(StringPant$Img, 800, 800), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
# print(StringPant$Dist / 1000)
# StringPant$Instructions[rowSums(StringPant$Instructions) != 0, ] %>% write.csv("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/instruction_panthere.csv")
# 
# ImgKarma <- load.image("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/jinx.jpg")
# # ImgKarma <- imsub(ImgKarma, x > 50 & x < 210)
# ImgKarma <- imsub(ImgKarma, y < 2000)
# ImgKarma <- resize(ImgKarma, size_x = 800, size_y = 800)
# ImgKarma <- grayscale(ImgKarma) %>% as.matrix()
# # ImgMarmotte <- ImgMarmotte ** 2.5 # Pour réhausser le contraste
# ImgKarma <- 1 - ImgKarma
# ImgKarma <- ImgKarma ** .75
# image(ImgKarma, zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
# 
# Cercle <- CirclePins(180, 799, 799)
# 
# Test <- GenThread(ImgKarma,
#                   Cercle$x, Cercle$y, TRUE,
#                   .3, 5000, 560 / 800,
#                   "anti-alias")
# image(matrix(Test$Img, 800, 800), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
# print(Test$Dist / 1000)
# Test$Instructions[rowSums(Test$Instructions) != 0, ] %>% write.csv("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/instruction_jinx.csv")
# 
# # On charge l'image et on la met au bon format / en noir et blanc
# Levi <- load.image("C:/Users/DRY12/Documents/GitHub/MarmotsTuesday/Images/eren.jpeg")
# Levi <- grayscale(Levi) %>% 
#   imsub(y < 150) %>% 
#   imsub(x > 20 & x < 220) %>% 
#   resize(size_x = 800, size_y = 800)
# # On va essayer de réhausser le contraste sur les endroits de transition
# PoidsLevi <- Levi
# PoidsLevi <- PoidsLevi %>% isoblur(2) %>% imgradient("xy")
# PoidsLevi <- sqrt(PoidsLevi$x ** 2 + PoidsLevi$y ** 2)
# PoidsLevi <- (as.matrix(PoidsLevi) > .01) * .5 + .5
# # ErenTitan[Eren2 > .03] <- ErenTitan[Eren2 > .03] ** 2 # Abandon de cette partie qui n'a pas l'air d'apporter de précision en plus
# # On transforme l'image en matrice que la fonction comprend
# Levi <- as.matrix(Levi)
# Levi <- 1 - Levi
# Levi <- Levi ** .7
# # # Définition de la matrice des poids. Je pense prendre la matrice des gradients pour ça
# # Eren2 <- as.matrix(Eren2)
# # Eren2 <- ifelse(Eren2 > .03, 1, ((Eren2 - min(Eren2)) / (.03 - min(Eren2)) + 9) / 10)
# # On lance le calcul des fils
# Cercle <- CirclePins(133, 799, 799)
# StringEren <- GenThread(ErenTitan, Eren2, 
#                         Cercle$x, Cercle$y, TRUE,
#                         .15, 4000, 560 / 800,
#                         "anti-alias")
# StringEren <- GenThread(ErenTitan, matrix(1, 800, 800), 
#                         Cercle$x, Cercle$y, TRUE,
#                         .15, 4000, 560 / 800,
#                         "anti-alias")
# 
# 
# 
# 
# 
# 
# image(matrix(StringEren$Img, 800, 800), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
# print(StringEren$Dist / 1000)
# 
# hist(Eren2) 
# min(Eren2)
# max(Eren2)
# 
# image(Levi, zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
# image(transform_adjust_contrast(Levi, 2), zlim = c(0, 1), col = rev(gray.colors(100)), ylim = c(1, 0))
# 
# plot(ErenTitan)
# plot(Eren2 > .01)
