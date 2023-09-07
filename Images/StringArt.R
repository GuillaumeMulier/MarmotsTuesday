library(tidyverse)
library(imager)
library(progressr)
library(glmnet)

handlers(handler_progress(format = "(:bar) :current/:total :percent",
                          complete = "=", incomplete = " ", current = ">"))

# Bresenham alogorithm

FindPixels <- function(X0, Y0, X1, Y1, GridX, GridY, Method = "bresenham") {
  
  Obj <- structure(
    list(X0 = X0, Y0 = Y0,
         X1 = X1, Y1 = Y1, 
         GridX = GridX, GridY = GridY),
    class = Method
  )
  
  Res <- BlackenPixels(Obj)
  return(Res)
  
}

BlackenPixels <- function(Obj) {
  UseMethod("BlackenPixels")
}

BlackenPixels.bresenham <- function(Obj) {
  
  PixLine <- function(x0, y0, x1, y1) {
    DeltaX <- x1 - x0
    DeltaY <- y1 - y0
    Increment<- 1
    if (DeltaY < 0) {
      Increment <- -1
      DeltaY <- -DeltaY
    }
    D <- 2 * DeltaY - DeltaX
    y = y0
    Resultats <- matrix(NA_integer_, ncol = 2, nrow = 0)
    for (x in seq(x0, x1)) {
      Resultats <- rbind(Resultats, c(x, y))
      if (D > 0) {
        y <- y + Increment
        D <- D + 2 * (DeltaY - DeltaX)
      } else {
        D <- D + 2 * DeltaY
      }
    }
    return(Resultats)
  }
  
  MatX <- seq_along(Obj$GridX[-1])
  MatY <- seq_along(Obj$GridY[-1])
  X0 <- which.min(abs(Obj$X0 - Obj$GridX))[1]
  X1 <- which.min(abs(Obj$X1 - Obj$GridX))[1]
  Y0 <- which.min(abs(Obj$Y0 - Obj$GridY))[1]
  Y1 <- which.min(abs(Obj$Y1 - Obj$GridY))[1]
  
  EchangeCoord <- FALSE
  if (abs(Y1 - Y0) > abs(X1 - X0)) {
    EchangeCoord <- TRUE
    Temp <- X0
    X0 <- Y0
    Y0 <- Temp
    Temp <- X1
    X1 <- Y1
    Y1 <- Temp
  }
  
  if (X0 < X1) {
    Xg <- X0
    Yg <- Y0
    Xd <- X1
    Yd <- Y1
  } else {
    Xg <- X1
    Yg <- Y1
    Xd <- X0
    Yd <- Y0
  }
  
  Resultats <- PixLine(Xg, Yg, Xd, Yd)
  if (EchangeCoord) Resultats <- Resultats[, c(2, 1)]
  
  return(Resultats)
  
}

CirclePins <- function(Rayon, NbPins, GridX, GridY) {
  Angles <- seq(0, 2 * pi, length.out = NbPins + 1)
  Angles <- Angles[-length(Angles)]
  tibble(num = seq_along(Angles), x = Rayon * cos(Angles), y = Rayon * sin(Angles)) %>% 
    mutate(x = map_dbl(x, ~ which.min(abs(.x - GridX))[1]), y = map_dbl(y, ~ which.min(abs(.x - GridY))[1]))
}

ImgMarmotte <- load.image("C:/Users/gmulier/Documents/GitHub/MarmotsTuesday/Images/aquarelle_marmotte.jpg")
ImgMarmotte <- imsub(ImgMarmotte, y > 120 & y < 650)
ImgMarmotte <- resize(ImgMarmotte, size_x = 501, size_y = 501)
ImgMarmotte <- grayscale(ImgMarmotte) %>% as.matrix()
ImgMarmotte <- ImgMarmotte ** 2.5 # Pour réhausser le contraste
ImgMarmotte <- (1 - ImgMarmotte) * 255

system.time({
  Cercle <- CirclePins(5, 300, seq(-5, 5, .02), seq(-5, 5, .02))
  Ex <- GetAllLines(Cercle$x, Cercle$y)
})

points(ci)

MatBase <- matrix(0, 11, 11)

Err <- map_dbl(seq_along(Ex),
        \(i) {
          if (length(Ex[[i]]) == 0) {
            return(Inf)
          } else {
            MatTemp <- MatBase
            MatTemp[Ex[[i]]] <- 40
            return(sum((ImgMarmotte - MatTemp) ** 2) / 121)
          }
        })
which.min(Err)
i = which.min(Err)
MatBase[Ex[[i]]] <- MatBase[Ex[[i]]] + 40
Ex

Instructions <- GenThread(ImgMarmotte, Ex, 300, 501, 501, 40, 4000)
Instructions
plot(Cercle$x, -Cercle$y, col = "red", cex = 1, type = "p", pch = 20)

TabLignes <- map_dfr(1:nrow(Instructions), ~ c("x" = Cercle$x[Instructions[.x, 1] + 1], "y" = -Cercle$y[Instructions[.x, 1] + 1], "xend" = Cercle$x[Instructions[.x, 2] + 1], "yend" = -Cercle$y[Instructions[.x, 2] + 1]))
Graphe <- ggplot(Cercle, aes(x, -y)) +
  geom_point(color = "blue") +
  coord_fixed(ratio = 1) +
  theme_void() +
  geom_segment(data = TabLignes, aes(x, y, xend = xend, yend = yend))

for (i in 1:nrow(Instructions)) {
  segments(Cercle$x[Instructions[i, 1]], -Cercle$y[Instructions[i, 1]], Cercle$x[Instructions[i, 2]], -Cercle$y[Instructions[i, 2]], col = "black")
}

i = 39
  points(Ex[[i]][, 1], Ex[[i]][, 2], type = "p", col = "darkblue", pch = 15, cex = 1)
Ex$Correspondances[20, ]
lines(Cercle$x[c(3, 6)], Cercle$y[c(3, 6)])
?image



with_progress({
  PBarre <- progressor(steps = choose(50, 2))
  Lignes <- map(seq_len(nrow(Cercle) - 1),
                \(p1) {
                  X0 <- Cercle$x[p1]
                  Y0 <- Cercle$y[p1]
                  map(seq(p1 + 1, nrow(Cercle)),
                      \(p2) {
                        X1 <- Cercle$x[p2]
                        Y1 <- Cercle$y[p2]
                        Lignes <- FindPixels(X0, Y0, X1, Y1, 1:250, 1:250)
                        MatLigne <- MatBase
                        MatLigne[Lignes] <- .25
                        PBarre()
                        return(MatLigne)
                      })
                })
})

Data <- data.frame(Y = as.numeric(ImgMarmotte))
Lignes <- map_dfc(seq_len(nrow(Cercle) - 1),
                  \(p1) {
                    map_dfc(seq_along(Lignes[[p1]]),
                            \(index2) {
                              p2 <- p1 + index2
                              Res <- data.frame(V1 = as.numeric(Lignes[[p1]][[index2]]))
                              names(Res) <- paste0("L", p1, "_", p2)
                              return(Res)
                            })
                  })

Coeffs <- solve(t(Lignes) %*% Lignes) %*% t(Lignes) %*% Data$Y 

Lignes <- data.matrix(Lignes)
Data <- cbind(Data, Lignes)
lm(Y ~ ., data = Data)
CrossVal <- cv.glmnet(x = Lignes, y = Data$Y, alpha = 1, intercept = FALSE, family = "gaussian")
plot(CrossVal)
CrossVal$lambda.1se
Mod <- glmnet(x = Lignes, y = Data$Y, intercept = FALSE, alpha = 1, family = "gaussian", lambda = CrossVal$lambda.1se)
summary(Mod)

colnames(Lignes)[lines_draw == 1] %>% 
  map(
    \(col) {
      clou1 <- str_replace_all(col, "^L_\\d+_(\\d+)$")
    }
  )

Lignes <- FindPixels(1, 1, 500, 500, 1:500, 1:500)
MatLigne <- MatBase
MatLigne[Lignes] <- .25

image(MatLigne)

image(1:500, 1:500, ImgMarmotte[, 500:1])
points(Cercle$x, Cercle$y)

imsub(ImgMarmotte, y > 660) %>% plot()

Test <- CirclePins(10, 200)
plot(Test$x, Test$y)

ggplot(Test, aes(x, y)) + geom_point() + coord_fixed(ratio = 1)

Test <- FindPixels(9, 2, 2, 14, seq(0, 10, .01), seq(0, 15, .01), Method = "bresenham")

TestMat <- matrix(0, ncol = 1500, nrow = 1000)
TestMat[Test] <- 1
image(seq(0, 10, .01), seq(0, 15, .01), TestMat)
library(tidyverse)
Test <- Test %>% 
  as.data.frame() %>% 
  mutate(across(c(V1, V2), ~ .x + .005), couleur = "black") %>% 
  complete(expand.grid(V1 = seq(.005, 9.995, .01), V2 = seq(.005, 14.995, .01)), fill = list(couleur = "white"))
ggplot(Test, aes(V1, V2, fill = couleur)) +
  geom_raster() +
  scale_fill_identity() +
  annotate("segment", x = 2, xend = 7, y = 2, yend = 12, size = 1.2)


bresenham_line <- function(x1, y1, x2, y2) {
  points <- data.frame(x = integer(0), y = integer(0))
  
  dx <- abs(x2 - x1)
  dy <- abs(y2 - y1)
  
  inc_x <- ifelse(x1 < x2, 1, -1)
  inc_y <- ifelse(y1 < y2, 1, -1)
  
  x <- x1
  y <- y1
  
  if (dx >= dy) {
    p <- 2 * dy - dx
    
    for (i in seq_len(dx)) {
      points <- rbind(points, data.frame(x = x, y = y))
      x <- x + inc_x
      if (p >= 0) {
        y <- y + inc_y
        p <- p - 2 * dx
      }
      p <- p + 2 * dy
    }
  } else {
    p <- 2 * dx - dy
    
    for (i in seq_len(dy)) {
      points <- rbind(points, data.frame(x = x, y = y))
      y <- y + inc_y
      if (p >= 0) {
        x <- x + inc_x
        p <- p - 2 * dy
      }
      p <- p + 2 * dx
    }
  }
  
  return(points)
}

# Example usage
x2 <- 1
y2 <- 8
x1 <- 4
y1 <- 2
line_points <- bresenham_line(x1, y1, x2, y2)

# Display the line points
print(line_points)


