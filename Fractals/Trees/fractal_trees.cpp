#include <Rcpp.h>
using namespace Rcpp;


// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix SubsetMat(NumericMatrix X, LogicalVector Condition) { 
  int Lignes = X.nrow();
  int Colonnes = X.ncol();
  int j = 0;
  NumericMatrix Output(sum(Condition), Colonnes);
  for (int i = 0; i < Lignes; i++) {
    if(Condition[i]) {
      Output(j, _) = X(i, _);
      j += 1;
    }
  }
  return(Output);
}

// [[Rcpp::export]]
NumericMatrix MakeBranches(double X0, double X1, 
                           double Y0, double Y1, 
                           double Angle0, NumericVector Angles,
                           NumericVector Shrinkage,
                           double Ep0, NumericVector PerteEpaisseur,
                           NumericVector ProbaRester,
                           NumericVector ProbaAlea, double CoefAlea) {
  
  int Longueur = Angles.length();
  double Distance = std::sqrt(pow(X1 - X0, 2) + pow(Y1 - Y0, 2));
  NumericMatrix Matrice(Longueur, 6);
  
  // Ajouter l'aléatoire sur l'angle
  NumericVector UnifProb2 = Rcpp::runif(Longueur, 0, 1);
  NumericVector Multiplicateur = rep(0.0, Longueur);
  LogicalVector VecLig2 = UnifProb2 <= ProbaAlea;
  if (sum(VecLig2) > 0) {
    NumericVector MultNorm = CoefAlea * Rcpp::rnorm(sum(VecLig2));
    Multiplicateur[VecLig2] = MultNorm;
    Angles = Angles + Multiplicateur;
  }
  
  Matrice(_, 0) = rep(X1, Longueur);
  Matrice(_, 1) = rep(Y1, Longueur);
  Matrice(_, 2) = rep(Angle0, Longueur) + Angles;
  Matrice(_, 3) = rep(X1, Longueur) + Distance * Shrinkage * sin(Matrice(_, 2));
  Matrice(_, 4) = rep(Y1, Longueur) + Distance * Shrinkage * cos(Matrice(_, 2));
  Matrice(_, 5) = rep(Ep0, Longueur) * PerteEpaisseur;
  
  // Remplir la matrice en fonction de si on garde ou non la branche
  NumericVector UnifProb = Rcpp::runif(Longueur, 0, 1);
  LogicalVector VecLig = UnifProb <= ProbaRester;
  NumericMatrix Output = SubsetMat(Matrice, VecLig);
  
  return Output;
  
}

// [[Rcpp::export]]
NumericMatrix IterateTree(int Iterations, 
                          double X0, double Y0,
                          double X1, double Y1,
                          NumericVector Angles, NumericVector Shrinkage,
                          NumericVector PerteEpaisseur,
                          NumericVector ProbaRester, double PuissRester,
                          NumericVector ProbaAlea, double CoefAlea, double PuissAlea) {
  
  int Longueur = Angles.length();
  int NMax = 0;
  for (int i = 0; i < (Iterations + 1); i++) {
    NMax += pow(Longueur, i);
  }
  NumericMatrix Matrice(NMax, 7);
  
  // Remplir la 1ère branche de l'arbre : la racine
  Matrice(0, 0) = 0;
  Matrice(0, 1) = X0;
  Matrice(0, 2) = Y0;
  Matrice(0, 3) = 0;
  Matrice(0, 4) = X1;
  Matrice(0, 5) = Y1;
  Matrice(0, 6) = 1;
  int NBranches = 1;
  int DebutBranches = 0;
  int DebutLigne = 1;
  
  for (int i = 1; i < (Iterations + 1); i++) { // On réalise les itérations
    
    int NouvBranches = 0;
    
    for (int j = 0; j < NBranches; j++) { // Boucle sur les branches de l'étape précédente
      
      // On trouve les branches filles pour la branche en cours d'étude
      NumericMatrix TempMat = MakeBranches(Matrice(DebutBranches + j, 1), Matrice(DebutBranches + j, 4),
                                           Matrice(DebutBranches + j, 2), Matrice(DebutBranches + j, 5),
                                           Matrice(DebutBranches + j, 3), Angles, Shrinkage,
                                           Matrice(DebutBranches + j, 6), PerteEpaisseur, ProbaRester,
                                           ProbaAlea, CoefAlea);
      int LignesMat = TempMat.nrow();
      int ColMat = TempMat.ncol();
      
      // On remplit les lignes avec les nouvelles branches
      for (int l = 0; l < LignesMat; l++) {
        Matrice(DebutLigne + l, 0) = i;
        for (int c = 0; c < ColMat; c++) {
          Matrice(DebutLigne + l, c + 1) = TempMat(l, c);
        }
      }
      
      // Mettre à jour les informations pour remplir la nouvelle matrice des branches
      DebutLigne += LignesMat;
      NouvBranches += LignesMat;
      
    } // Fin de la boucle sur les branches
    
    // Changement des variables pour passer à l'itération suivante
    DebutBranches += NBranches;
    NBranches = NouvBranches;
    ProbaRester = pow(ProbaRester, PuissRester);
    ProbaAlea = pow(ProbaAlea, PuissAlea);
    
  } // Fin de la boucle qui fait les itérations
  
  return Matrice(Range(0, DebutLigne - 1), _);
  
}

