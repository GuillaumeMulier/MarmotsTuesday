#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double Const01(double x) {
  if (x < 0) {
    return 0;
  } else if (x > 1) {
    return 1;
  } else {
    return(x);
  }
}


// [[Rcpp::export]]
IntegerMatrix BresehamLine(int X0, int Y0, int X1, int Y1) {
  
  // Change of coordinates if line is too steep
  bool EchangeCoord = false;
  int DiffX = abs(X1 - X0);
  int DiffY = abs(Y1 - Y0);
  if (DiffY > DiffX) {
    EchangeCoord = true;
    std::swap(X0, Y0);
    std::swap(X1, Y1);
  }
  
  int Xg;
  int Yg;
  int Xd;
  int Yd;
  
  if (X0 < X1) {
    Xg = X0;
    Yg = Y0;
    Xd = X1;
    Yd = Y1;
  } else {
    Xg = X1;
    Yg = Y1;
    Xd = X0;
    Yd = Y0;
  }
  
    
  int DeltaX = Xd - Xg;
  int DeltaY = Yd - Yg;
  int Increment = 1;
  if (DeltaY < 0) {
    Increment = -1;
    DeltaY = -DeltaY;
  }
  int D = 2 * DeltaY - DeltaX;
  int y = Yg;
  IntegerVector Xs (0);
  IntegerVector Ys (0);
  
  for (int x = Xg; x < (Xd + 1); x++) {
    Xs.push_back(x);
    Ys.push_back(y);
    if (D > 0) {
      y = y + Increment;
      D = D + 2 * (DeltaY - DeltaX);
    } else {
      D = D + 2 * DeltaY;
    }
  }
  
  int NLignes = Xs.length();
  IntegerMatrix Resultats(NLignes, 2);
  Resultats(_, 0) = Xs;
  Resultats(_, 1) = Ys;
  
    if (EchangeCoord) {
      IntegerVector TmpCol = Resultats(_, 0);
      Resultats(_, 0) = Resultats(_, 1);
      Resultats(_, 1) = TmpCol;
    }
  
  return Resultats;
  
}

// [[Rcpp::export]]
List AntiAliasLines(int X0, int Y0, int X1, int Y1) {
  
  IntegerVector Xs (0);
  IntegerVector Ys (0);
  NumericVector Vals (0);
  
  int DeltaX = abs(X1 - X0);
  double DX = abs(X1 - X0);
  int DeltaY = abs(Y1 - Y0);
  double DY = abs(Y1 - Y0);
  int SommeX = X0 < X1 ? 1 : -1;
  int SommeY = Y0 < Y1 ? 1 : -1;
  double Erreur = DX - DY;
  int XPrime;
  double EPrime;
  double Ed = DeltaX + DeltaY == 0 ? 1 : sqrt(DX * DX + DY * DY);
  
  int Y = Y0;
  int X = X0;
  
  while (true) {
  // for (int i = 0; i<20; i++) {
    Xs.push_back(X);
    Ys.push_back(Y);
    Vals.push_back(1 - abs(Erreur - DeltaX + DeltaY) / Ed);
    XPrime = X;
    EPrime = Erreur;
    if ((2 * EPrime) >= -DeltaX) {
      if (X == X1) break;
      if ((EPrime + DeltaY) < Ed) {
        Xs.push_back(X);
        Ys.push_back(Y + SommeY);
        Vals.push_back(1 - abs(EPrime + DeltaY) / Ed);
      }
      Erreur -= DeltaY;
      X += SommeX;
    }
    if ((EPrime * 2) <= DeltaY) {
      if (Y == Y1) break;
      if ((DeltaX - EPrime) < Ed) {
        Xs.push_back(XPrime + SommeX);
        Ys.push_back(Y);
        Vals.push_back(1 - abs(DeltaX - EPrime) / Ed);
      }
      Erreur += DeltaX;
      Y += SommeY;
    }
  }
  
  return List::create(Ys, Xs, Vals);
  
}


// [[Rcpp::export]]
double ComputeError(NumericVector ImgBase, NumericVector ImgNouv,
                    NumericVector Poids,
                    String Fct = "square") {
  double erreur;
  if (Fct == "square") {
    erreur = sum(Poids * pow(ImgNouv - ImgBase, 2));
  } else if (Fct == "absolute") {
    erreur = sum(Poids * abs(ImgNouv - ImgBase));
  }
  return erreur;
}

// [[Rcpp::export]]
List GenThread(NumericMatrix Img, NumericMatrix Poids,
               IntegerVector XPins, IntegerVector YPins,
               bool Black,
               double ForceString, int MaxPins, double MmPerPix,
               String Method = "bresenham") {
  
  int LongueurMat = Img.nrow();
  int LargeurMat = Img.ncol();
  double Distance = 0.0;
  NumericVector ImgBase = as<NumericVector>(Img);
  NumericVector PoidsVec = as<NumericVector>(Poids);
  int NPins = XPins.length();
  // NumericVector ImgModif = clone(Img);
  NumericVector ImgVide (LongueurMat * LargeurMat);
  double Multiplicateur = 1.0;
  if (Black == false) {
    ImgVide = rep(1.0, LongueurMat * LargeurMat);
    Multiplicateur = -1.0;
  }
  
  
  IntegerMatrix Instructions(MaxPins, 2);
  double ErrorCandidate = R_PosInf;
  double ErrorAct;
  int Pin1;
  int Pin2;
  
  // Find first string
  for (int i = 0; i < (NPins - 1); i++) {
    for (int j = (i + 1); j < NPins; j++) {
      NumericVector NouvImg = clone(ImgVide);
      if (Method == "bresenham") {
        IntegerMatrix Pixels = BresehamLine(XPins[i], YPins[i], XPins[j], YPins[j]);
        for (int k = 0; k < Pixels.nrow(); k++) {
          int xx = Pixels(k, 0);
          int yy = Pixels(k, 1);
          NouvImg[xx * LongueurMat + yy] = (NouvImg[xx * LongueurMat + yy] + Multiplicateur * ForceString);
        }
      } else if (Method == "anti-alias") {
        List Pixels = AntiAliasLines(XPins[i], YPins[i], XPins[j], YPins[j]);
        IntegerVector Lignes = Pixels[0];
        IntegerVector Colonnes = Pixels[1];
        NumericVector Valeurs = Pixels[2];
        for (int k = 0; k < Lignes.length(); k++) {
          int xx = Colonnes[k];
          int yy = Lignes[k];
          double valeur = Valeurs[k];
          NouvImg[xx * LongueurMat + yy] = (NouvImg[xx * LongueurMat + yy] + Multiplicateur * ForceString * valeur);
        }
      }
      ErrorAct = ComputeError(ImgBase, NouvImg, PoidsVec);
      if (ErrorAct < ErrorCandidate) {
        Pin1 = i;
        Pin2 = j;
        ErrorCandidate = ErrorAct;
      }
      
    }
  }
  if (Method == "bresenham") {
    IntegerMatrix Pixels = BresehamLine(XPins[Pin1], YPins[Pin1], XPins[Pin2], YPins[Pin2]);
    for (int k = 0; k < Pixels.nrow(); k ++) {
      int xx = Pixels(k, 0);
      int yy = Pixels(k, 1);
      ImgVide[xx * LongueurMat + yy] = (ImgVide[xx * LongueurMat + yy] + Multiplicateur * ForceString);
    }
  } else if (Method == "anti-alias") {
    List Pixels = AntiAliasLines(XPins[Pin1], YPins[Pin1], XPins[Pin2], YPins[Pin2]);
    IntegerVector Lignes = Pixels[0];
    IntegerVector Colonnes = Pixels[1];
    NumericVector Valeurs = Pixels[2];
    for (int k = 0; k < Lignes.length(); k++) {
      int xx = Colonnes[k];
      int yy = Lignes[k];
      double valeur = Valeurs[k];
      ImgVide[xx * LongueurMat + yy] = (ImgVide[xx * LongueurMat + yy] + Multiplicateur * ForceString * valeur);
    }
  }
  Instructions(0, 0) = Pin1;
  Instructions(0, 1) = Pin2;
  
  // Loop for other pins to find
  bool EnCours = true;
  int LigneTab = 1;
  double ErrPrec = ErrorCandidate;
  while (EnCours) {
    if (fmod(static_cast<double>(LigneTab), 100.0) == 0) {
      Rcout << "Clou n°" << LigneTab << "\n";
    }
    Pin1 = Pin2;
    for (int i = 0; i < NPins; i++) {
      NumericVector NouvImg = clone(ImgVide);
      if (Method == "bresenham") {
        IntegerMatrix Pixels = BresehamLine(XPins[Pin1], YPins[Pin1], XPins[i], YPins[i]);
        for (int k = 0; k < Pixels.nrow(); k++) {
          int xx = Pixels(k, 0);
          int yy = Pixels(k, 1);
          NouvImg[xx * LongueurMat + yy] = Const01(NouvImg[xx * LongueurMat + yy] + Multiplicateur * ForceString);
        }
      } else if (Method == "anti-alias") {
        List Pixels = AntiAliasLines(XPins[Pin1], YPins[Pin1], XPins[i], YPins[i]);
        IntegerVector Lignes = Pixels[0];
        IntegerVector Colonnes = Pixels[1];
        NumericVector Valeurs = Pixels[2];
        for (int k = 0; k < Lignes.length(); k++) {
          int xx = Colonnes[k];
          int yy = Lignes[k];
          double valeur = Valeurs[k];
          NouvImg[xx * LongueurMat + yy] = Const01(NouvImg[xx * LongueurMat + yy] + Multiplicateur * ForceString * valeur);
        }
      }
      ErrorAct = ComputeError(ImgBase, NouvImg, PoidsVec);
      if (ErrorAct < ErrorCandidate) {
        Pin2 = i;
        ErrorCandidate = ErrorAct;
      }
    }
    if (ErrorCandidate == ErrPrec) {
      EnCours = false;
    }
    if (Method == "bresenham") {
      IntegerMatrix Pixels = BresehamLine(XPins[Pin1], YPins[Pin1], XPins[Pin2], YPins[Pin2]);
      for (int k = 0; k < Pixels.nrow(); k ++) {
        int xx = Pixels(k, 0);
        int yy = Pixels(k, 1);
        ImgVide[xx * LongueurMat + yy] = Const01(ImgVide[xx * LongueurMat + yy] + Multiplicateur * ForceString);
      }
    } else if (Method == "anti-alias") {
      List Pixels = AntiAliasLines(XPins[Pin1], YPins[Pin1], XPins[Pin2], YPins[Pin2]);
      IntegerVector Lignes = Pixels[0];
      IntegerVector Colonnes = Pixels[1];
      NumericVector Valeurs = Pixels[2];
      for (int k = 0; k < Lignes.length(); k++) {
        int xx = Colonnes[k];
        int yy = Lignes[k];
        double valeur = Valeurs[k];
        ImgVide[xx * LongueurMat + yy] = Const01(ImgVide[xx * LongueurMat + yy] + Multiplicateur * ForceString * valeur);
      }
    }
    Instructions(LigneTab, 0) = Pin1;
    Instructions(LigneTab, 1) = Pin2;
    LigneTab ++;
    Distance += MmPerPix * sqrt((XPins[Pin2] - XPins[Pin1]) * (XPins[Pin2] - XPins[Pin1]) + (YPins[Pin2] - YPins[Pin1]) * (YPins[Pin2] - YPins[Pin1]));
    ErrPrec = ErrorCandidate;
    if (LigneTab > (MaxPins - 1)) {
      EnCours = false;
    }
  }
  
  return List::create(Named("Instructions") = Instructions, Named("Img") = ImgVide, Named("Dist") = Distance);
  
}


// [[Rcpp::export]]
List Testeur(NumericMatrix Img, bool Black) {
  
  int LongueurMat = Img.nrow();
  int LargeurMat = Img.ncol();
  NumericVector ImgBase = as<NumericVector>(Img);
  NumericVector ImgVide (LongueurMat * LargeurMat);
  double Multiplicateur = 1.0;
  if (Black == false) {
    ImgVide = rep(1.0, LongueurMat * LargeurMat);
    Multiplicateur = -1.0;
  }
  return List::create(Named("Vide") = ImgVide, Named("Mult") = Multiplicateur);
  
}

