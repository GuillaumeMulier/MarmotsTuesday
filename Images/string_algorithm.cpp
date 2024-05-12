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
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

// [[Rcpp::export]]
IntegerMatrix PixLine(int x0, int y0, int x1, int y1) {
  
  int DeltaX = x1 - x0;
  int DeltaY = y1 - y0;
  int Increment = 1;
  if (DeltaY < 0) {
    Increment = -1;
    DeltaY = -DeltaY;
  }
  int D = 2 * DeltaY - DeltaX;
  int y = y0;
  IntegerVector Xs (0);
  IntegerVector Ys (0);
  
  for (int x = x0; x < (x1 + 1); x++) {
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
  
  return Resultats;
  
}


// [[Rcpp::export]]
int AliasLine(int x0, int y0, int x1, int y1, bool EchangeCoord) {
  
  double DeltaX = x1 - x0;
  double DeltaY = y1 - y0;
  double Gradient = 1;
  if (DeltaX != 0) {
    Gradient = DeltaY / DeltaX;
  }
  NumericVector Xs (0);
  NumericVector Ys (0);
  NumericVector Strengths (0);
  
  // First endpoint
  double xend = x0;
  double yend = y0;
  double xgap = 0.5;
  Xs.push_back(xend);
  Xs.push_back(xend);
  // Ys.push_back(y);
  // Ys.push_back(y);
  double Xpxl1 = xend;
  
  
  // int D = 2 * DeltaY - DeltaX;
  // int y = y0;
  // IntegerVector Xs (0);
  // IntegerVector Ys (0);
  // 
  // for (int x = x0; x < (x1 + 1); x++) {
  //   Xs.push_back(x);
  //   Ys.push_back(y);
  //   if (D > 0) {
  //     y = y + Increment;
  //     D = D + 2 * (DeltaY - DeltaX);
  //   } else {
  //     D = D + 2 * DeltaY;
  //   }
  // }
  // 
  // int NLignes = Xs.length();
  // IntegerMatrix Resultats(NLignes, 2);
  // Resultats(_, 0) = Xs;
  // Resultats(_, 1) = Ys;
  // 
  // return Resultats;
  return xend;
  
}

// [[Rcpp::export]]
IntegerMatrix GetCoordPixels(int X0, int Y0, int X1, int Y1, 
                             String Method = "bresenham") {
  
  List ListeResultats;
  IntegerMatrix Resultats;
  
  if (Method == "bresenham") {
    
    bool EchangeCoord = false;
    int DiffX = abs(X1 - X0);
    int DiffY = abs(Y1 - Y0);
    int Temp;
    if (DiffY > DiffX) {
      EchangeCoord = true;
      Temp = X0;
      X0 = Y0;
      Y0 = Temp;
      Temp = X1;
      X1 = Y1;
      Y1 = Temp;
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
    
    int DeltaX = x1 - x0;
  int DeltaY = y1 - y0;
  int Increment = 1;
  if (DeltaY < 0) {
    Increment = -1;
    DeltaY = -DeltaY;
  }
  int D = 2 * DeltaY - DeltaX;
  int y = y0;
  IntegerVector Xs (0);
  IntegerVector Ys (0);
  
  for (int x = x0; x < (x1 + 1); x++) {
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
    
  } else if (Method == "antialias") {
    
    
    
  }
  
  return Resultats;
  
}


// [[Rcpp::export]]
IntegerMatrix BresehamLine(int X0, int Y0, int X1, int Y1, 
                             String Method = "bresenham") {
  
  IntegerMatrix Resultats;
  
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
  
    
    int DeltaX = x1 - x0;
  int DeltaY = y1 - y0;
  int Increment = 1;
  if (DeltaY < 0) {
    Increment = -1;
    DeltaY = -DeltaY;
  }
  int D = 2 * DeltaY - DeltaX;
  int y = y0;
  IntegerVector Xs (0);
  IntegerVector Ys (0);
  
  for (int x = x0; x < (x1 + 1); x++) {
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
IntegerMatrix AntiAliasLines(int X0, int Y0, int X1, int Y1) {
  
  IntegerMatrix Resultats;
  
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
  
  // Resultats = AliasLine(Xg, Yg, Xd, Yd);
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
    
  
  return Resultats;
  
}

// [[Rcpp::export]]
List GetAllLines(IntegerVector Xs, IntegerVector Ys, 
                 String Method = "bresenham") {
  
  int NPins = Xs.length();
  int NEntries = NPins * NPins;
  List Resultats(NEntries);
  // IntegerMatrix Correspondances (NEntries, 3);
  
  for (int i = 0; i < NPins; i++) {
    
    for (int j = 0; j < NPins; j++) {
      
      if (i < j) {
        IntegerMatrix Ligne = BresehamLine(Xs[i], Ys[i], Xs[j], Ys[j]);
        Resultats[i * NPins + j] = Ligne;
      } else if (i == j) {
        IntegerVector Pixies = IntegerVector::create(i, j);
        IntegerMatrix Ligne (1, 2, Pixies.begin());
        Resultats[i * NPins + j] = Ligne;
      } else {
        Resultats[i * NPins + j] = Resultats[j * NPins + i];
      }
      
      // Correspondances(i * NPins + j, _) = IntegerVector::create(i * NPins + j, i, j);
      
    }
    
  }
  
  // List Res = List::create(Named("Correspondances") = Correspondances, Named("Pixels") = Resultats);
  
  // return Res;
  return Resultats;
  
}

// [[Rcpp::export]]
double ComputeError(NumericVector ImgBase, NumericVector ImgNouv, 
                    String Fct = "square") {
  double erreur;
  if (Fct == "square") {
    erreur = sum(pow(ImgNouv - ImgBase, 2));
  }
  return erreur;
}

// [[Rcpp::export]]
List GenThread(NumericMatrix Img,
               IntegerVector XPins, IntegerVector YPins,
               int LargeurMat, int LongueurMat, bool Black,
               double ForceString, int MaxPins,
               String Method = "bresenham") {
  
  int NRows = Img.nrow();
  int NCols = Img.nrow();
  NumericVector ImgBase = as<NumericVector>(Img);
  int NPins = XPins.length();
  // NumericVector ImgModif = clone(Img);
  NumericVector ImgVide (NRows * NCols);
  // if (Black == false) {
  //   ImgVide = rep(1, NRows * NCols);
  // }
  
  IntegerMatrix Instructions(MaxPins, 2);
  double ErrorCandidate = R_PosInf;
  double ErrorAct;
  int Pin1;
  int Pin2;
  
  // Find first string
  for (int i = 0; i < (NPins - 1); i++) {
    for (int j = (i + 1); j < NPins; j++) {
      Rcout << i << j << "\n";
      NumericVector NouvImg = clone(ImgVide);
      IntegerMatrix Pixels = BresehamLine(XPins[i], YPins[i], XPins[j], YPins[j], Method);
      for (int k = 0; k < Pixels.nrow(); k ++) {
        int xx = Pixels(k, 0);
        int yy = Pixels(k, 1);
        NouvImg[yy * NRows + xx] = NouvImg[yy * LongueurMat + xx] + ForceString;
      }
      ErrorAct = ComputeError(ImgBase, NouvImg);
      if (ErrorAct < ErrorCandidate) {
        Pin1 = i;
        Pin2 = j;
        ErrorCandidate = ErrorAct;
      }
    }
  }
  IntegerMatrix Pixels = BresehamLine(XPins[Pin1], YPins[Pin1], XPins[Pin2], YPins[Pin2], Method);
  for (int k = 0; k < Pixels.nrow(); k ++) {
    int xx = Pixels(k, 0);
    int yy = Pixels(k, 1);
    ImgVide[yy * NRows + xx] = ImgVide[yy * LongueurMat + xx] + ForceString;
  }
  Instructions(0, 0) = Pin1;
  Instructions(0, 1) = Pin2;
  
  
  
  // int NLignes = ToutesLignes.length();
  // NumericVector Error (NLignes, R_PosInf);
  // int PinDep;
  // // int PinArr;
  // 
  // // First string to put will check every possible configuration
  // for (int i = 0; i < (NPins - 1); i++) {
  //   
  //   for (int j = (i + 1); j < NPins; j++) {
  //     
  //     NumericVector NouvImg = clone(ImgVide);
  //     IntegerMatrix Pixels = ToutesLignes[i * NPins + j];
  //     for (int k = 0; k < Pixels.nrow(); k ++) {
  //       int xx = Pixels(k, 0);
  //       int yy = Pixels(k, 1);
  //       NouvImg[yy * LongueurMat + xx] = NouvImg[yy * LongueurMat + xx] + ForceString;
  //     }
  //     Error[i * NPins + j] = sum(pow(NouvImg - Img, 2)) / (LongueurMat * LargeurMat);
  //     
  //   }
  //   
  // }
  // // Find the pin and update the image
  // int GoodPin = which_min(Error);
  // double ErrorMin = Error[GoodPin];
  // Pin1 = GoodPin / NPins;
  // Pin2 = GoodPin - NPins * Pin1;
  // Instructions(0, _) = IntegerVector::create(Pin1, Pin2);
  // IntegerMatrix Pixels = ToutesLignes[Pin1 * NPins + Pin2];
  // for (int k = 0; k < Pixels.nrow(); k ++) {
  //   int xx = Pixels(k, 0);
  //   int yy = Pixels(k, 1);
  //   ImgVide[yy * LongueurMat + xx] = ImgVide[yy * LongueurMat + xx] + ForceString;
  // }
  // 
  // bool EnCours = true; 
  // int Index = 1;
  // // Reset the starting pin to previous ending pin and optimize again
  // PinDep = Pin2;
  // 
  // while (EnCours) {
  //   
  //   if (Index == MaxPins) {
  //     Rcout << "Number of iterations: " << Index << ". Total error: " << ErrorCandidate << ".\n";
  //     break;
  //   } else if (remainder(Index, 50) == 0) {
  //     Rcout << "Number of iterations: " << Index << ". Total error: " << ErrorCandidate << ".\n";
  //   }
  //   
  //   // Rcout << PinDep << " / " << PinArr << " / " << Pin1 << " / " << Pin2 << "\n";
  //   NumericVector Error (NPins, R_PosInf); 
  //   for (int i = 0; i < NPins; i++) {
  //     if (i == PinDep) {
  //       continue;
  //     } 
  //     // else if (i < PinDep) {
  //     //   Pin1 = i;
  //     //   Pin2 = PinDep;
  //     // } else {
  //     //   Pin1 = PinDep;
  //     //   Pin2 = i;
  //     // }
  //     NumericVector NouvImg = clone(ImgVide);
  //     IntegerMatrix Pixels = ToutesLignes[PinDep * NPins + i];
  //     for (int k = 0; k < Pixels.nrow(); k ++) {
  //       int xx = Pixels(k, 0);
  //       int yy = Pixels(k, 1);
  //       NouvImg[yy * LongueurMat + xx] = NouvImg[yy * LongueurMat + xx] + ForceString;
  //     }
  //     Error[i] = sum(pow(NouvImg - Img, 2)) / (LongueurMat * LargeurMat);
  //   }
  //   // Rcout << Error << "\n";
  //   // Find the pin and update the image
  //   // Pin1 = PinDep;
  //   GoodPin = which_min(Error);
  //   // PinArr = GoodPin;
  //   // Rcout << GoodPin << " / " << PinDep << " / " << PinArr << "\n";
  //   ErrorCandidate = Error[GoodPin];
  //   if (ErrorCandidate > ErrorMin) {
  //     EnCours = false;
  //   } else {
  //     Instructions(Index, _) = IntegerVector::create(PinDep, GoodPin);
  //     Index++;
  //     // if (PinDep < PinArr) {
  //     //   Pin1 = PinDep;
  //     //   Pin2 = PinArr;
  //     // } else {
  //     //   Pin1 = PinArr;
  //     //   Pin2 = PinDep;
  //     // }
  //     IntegerMatrix Pixels = ToutesLignes[PinDep * NPins + GoodPin];
  //     for (int k = 0; k < Pixels.nrow(); k ++) {
  //       int xx = Pixels(k, 0);
  //       int yy = Pixels(k, 1);
  //       ImgVide[yy * LongueurMat + xx] = ImgVide[yy * LongueurMat + xx] + ForceString;
  //     }
  //     ErrorMin = ErrorCandidate;
  //     PinDep = GoodPin;
  //   }
  //   
  //   
  //   
  // }
  // 
  // 
  // 
  // return Instructions(Range(0, Index), _);
  return List::create(Named("Instructions") = Instructions, Named("Img") = ImgVide);
  
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# PixLine(1, 5, 10, 1)
GetCoordPixels(1, 10, 5, 1)
GetCoordPixels2(1, 10, 5, 1)
# ComputeError(1:10, 3:12)
*/
