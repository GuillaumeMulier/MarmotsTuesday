library(parallel)
library(foreach)
library(doParallel)
library(doRNG)

SimulEssai <- function(n, N, p) {
  
  # Simuler les données
  IdPat <- setNames(sample.int(N, n, replace = TRUE), seq_len(n))
  Pat <- rep(seq_len(n), times = sample(c(rep(2, floor(p * n)), rep(1, n - floor(p * n)))))
  IdPatEch <- IdPat[Pat]
  
  # Compter les doublons
  NbPaires <- n * (n - 1) / 2
  Paires <- combn(seq_along(IdPatEch), 2)
  MemeId <- IdPatEch[Paires[1, ]] == IdPatEch[Paires[2, ]]
  MemePat <- Pat[Paires[1, ]] == Pat[Paires[2, ]]
  NbDoublons <- sum(MemeId)
  NbCollisions <- sum(MemeId & !MemePat)
  
  # Retourner les résultats
  return(c(NbDoublons, NbCollisions, NbPaires))
  
}

n <- 1400
N <- 8199360
p = 30 / n

cl <- makeCluster(50)
registerDoParallel(cl)

cat("Début du programme\n\n", file = "~/collisions/log.txt")

Res <- foreach(i = seq_len(1e+5),
               .export = c("SimulEssai", "n", "N", "p"),
               .options.RNG = 121221,
               .combine = "rbind") %dorng% {
                 
                 if (i %% 1000 == 0) cat(paste0("Simulation n°", i, "\n"), append = TRUE, file = "~/collisions/log.txt")
                 Resultat <- SimulEssai(n, N, p)
                 return(Resultat)
                 
               }
save(Res, file = "~/collisions/res.RData")

stopCluster(cl)