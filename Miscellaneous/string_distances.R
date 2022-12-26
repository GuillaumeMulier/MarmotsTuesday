DistanceMots <- function(Mots1,
                         Mots2,
                         Methode = "Levenshtein",
                         Poids = CreerPoids(),
                         Casse = FALSE,
                         Sortie = c("vector", "data.frame")) {
  
  Methode <- match.arg(Methode, c("Hamming", "Levenshtein", "DamerauLevenshtein"))
  Sortie <- match.arg(Sortie, c("vector", "data.frame"))
  
  # Convert inputs to vectors of characters
  if (is.list(Mots1)) Mots1 <- unlist(Mots1)
  if (is.list(Mots2)) Mots2 <- unlist(Mots2)
  if (!Casse) { # If you don't want to respect case
    Mots1 <- stringi::stri_trans_toupper(stringi::stri_trans_general(as.character(Mots1), "Latin-ASCII"))
    Mots2 <- stringi::stri_trans_toupper(stringi::stri_trans_general(as.character(Mots2), "Latin-ASCII"))
  } else {
    Mots1 <- as.character(Mots1)
    Mots2 <- as.character(Mots2)
  }
  
  # Check if there is only 1 word to compare to all others or in there is a difference of length of vectors
  if (length(Mots1) != length(Mots2)) {
    if (length(Mots1) == 1) {
      Mots1 <- rep(Mots1, length(Mots2))
    } else if (length(Mots2) == 1) {
      Mots2 <- rep(Mots2, length(Mots1))
    } else {
      stop("The 2 vectors to compare must be of same length.", call. = FALSE)
    }
  }
  
  # Check the weights structure
  if (!inherits(Poids, "Weights"))
    stop("Poids should be the weights for the different operations and created with function CreerPoids.", call. = FALSE)
  
  # S3 object to apply methods
  Comparaisons <- structure(
    list(Mots1 = Mots1, Mots2 = Mots2, Poids = Poids),
    class = c("ObjetDist", Methode)
  )
  Distances <- Distances(Comparaisons)
  
  # The result of the function (if you want a data.frame for comparison, set Sortie to data.frame)
  if (Sortie == "data.frame") Distances <- data.frame(mot1 = Mots1, mot2 = Mots2, distance = Distances)
  return(Distances)
  
}

CreerPoids <- function(Insertion = 1,
                       Deletion = 1,
                       Transposition = 1,
                       Substitution = 1,
                       Regles = NULL) {
  
  # Initialize the substitution matrix
  MatSubs <- matrix(Substitution, ncol = 224, nrow = 224)
  diag(MatSubs) <- 0
  colnames(MatSubs) <- rownames(MatSubs) <- rawToChar(as.raw(32:255), multiple = TRUE)
  
  # Incorporate rules if there are
  if (!is.null(Regles)) {
    for (i in length(Regles)) {
      Lettres <- strsplit(names(Regles)[i], "")[[1]]
      MatSubset <- t(combn(Lettres, 2))
      MatSubs[MatSubset] <- Regles[[i]]
      MatSubs[MatSubset[, c(2, 1)]] <- Regles[[i]]
    }
  }
  
  # Create the Weight object
  return(structure(
    list(insertion = Insertion,
         deletion = Deletion,
         transposition = Transposition,
         substitution = MatSubs),
    class = "Weights"
  ))
  
}

Distances <- function(ObjetDist) {
  
  stopifnot(inherits(ObjetDist, "ObjetDist"))
  UseMethod("Distances")
  
}

Distances.Hamming <- function(ObjetDist) {
  
  Resultats <- purrr::map2_dbl(
    ObjetDist$Mots1, ObjetDist$Mots2,
    function(x, y) {
      if (nchar(x) != nchar(y)) stop(paste0("The words ",
                                            x, " and ", y,
                                            " don't have the same number of characters for Hamming distance."),
                                     call. = FALSE)
      x <- strsplit(x = x, split = "")[[1]]
      y <- strsplit(x = y, split = "")[[1]]
      return(sum(x != y))
    }
  )
  
  return(Resultats)
  
}

Distances.Levenshtein <- function(ObjetDist) {
  
  Resultats <- purrr::pmap_dbl(
    list(x = ObjetDist$Mots1, y = ObjetDist$Mots2),
    function(x, y) {
      x <- strsplit(x = x, split = "")[[1]]
      y <- strsplit(x = y, split = "")[[1]]
      MatDist <- matrix(NA_real_, ncol = length(y) + 1, nrow = length(x) + 1)
      MatDist[1, ] <- seq_len(length(y) + 1) - 1
      MatDist[, 1] <- seq_len(length(x) + 1) - 1
      for (i in seq_along(x) + 1) { # Rows
        for (j in seq_along(y) + 1) { # Columns
          MatDist[i, j] <- min(
            MatDist[i - 1, j] + ObjetDist[["Poids"]][["deletion"]], # Deletion of last character from previous row
            MatDist[i, j - 1] + ObjetDist[["Poids"]][["insertion"]], # Insertion of character to the previous column
            MatDist[i - 1, j - 1] + ObjetDist[["Poids"]][["substitution"]][x[i - 1], y[j - 1]] # Substitution of last character of x[i-1] to last character of y[j-1]
          )
        }
      }
      return(MatDist[length(x) + 1, length(y) + 1])
    }
  )
  
  return(Resultats)
  
}

Distances.DamerauLevenshtein <- function(ObjetDist) {
  
  Resultats <- purrr::pmap_dbl(
    list(x = ObjetDist$Mots1, y = ObjetDist$Mots2),
    function(x, y) {
      x <- strsplit(x = x, split = "")[[1]]
      y <- strsplit(x = y, split = "")[[1]]
      MatDist <- matrix(NA_real_, ncol = length(y) + 1, nrow = length(x) + 1)
      MatDist[1, ] <- seq_len(length(y) + 1) - 1
      MatDist[, 1] <- seq_len(length(x) + 1) - 1
      for (i in seq_along(x) + 1) { # Rows
        for (j in seq_along(y) + 1) { # Columns
          MatDist[i, j] <- min(
            MatDist[i - 1, j] + ObjetDist[["Poids"]][["deletion"]], # Deletion of last character from previous row
            MatDist[i, j - 1] + ObjetDist[["Poids"]][["insertion"]], # Insertion of character to the previous column
            MatDist[i - 1, j - 1] + ObjetDist[["Poids"]][["substitution"]][x[i - 1], y[j - 1]] # Substitution of last character of x[i-1] to last character of y[j-1]
          )
          if (i > 2 && j > 2 && x[i - 1] == y[j - 2] && x[i - 2] == y[j - 1]) { # Adjacent transposition
            MatDist[i, j] <- min(
              MatDist[i, j],
              MatDist[i - 2, j - 2] + ObjetDist[["Poids"]][["transposition"]] * (x[i - 1] != y[j - 1])
              # Add the cost of transposition only if the characters are different
            )
          }
        }
      }
      return(MatDist[length(x) + 1, length(y) + 1])
    }
  )
  
  return(Resultats)
  
}
