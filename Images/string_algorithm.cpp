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
IntegerMatrix GetCoordPixels(int X0, int Y0, int X1, int Y1, 
                             String Method = "bresenham") {
  
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
    
    Resultats = PixLine(Xg, Yg, Xd, Yd);
    if (EchangeCoord) {
      IntegerVector TmpCol = Resultats(_, 0);
      Resultats(_, 0) = Resultats(_, 1);
      Resultats(_, 1) = TmpCol;
    }
    
  }
  
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
        IntegerMatrix Ligne = GetCoordPixels(Xs[i], Ys[i], Xs[j], Ys[j]);
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
IntegerMatrix GenThread(NumericVector Img,
                        List ToutesLignes, int NPins,
                        int LargeurMat, int LongueurMat,
                        double ForceString, int MaxPins) {
  
  int NPixels = Img.length();
  // NumericVector ImgModif = clone(Img);
  NumericVector ImgVide (NPixels);
  int NLignes = ToutesLignes.length();
  NumericVector Error (NLignes, R_PosInf);
  IntegerMatrix Instructions(MaxPins, 2);
  double ErrorCandidate = R_PosInf;
  int Pin1;
  int Pin2;
  int PinDep;
  // int PinArr;
  
  // First string to put will check every possible configuration
  for (int i = 0; i < (NPins - 1); i++) {
    
    for (int j = (i + 1); j < NPins; j++) {
      
      NumericVector NouvImg = clone(ImgVide);
      IntegerMatrix Pixels = ToutesLignes[i * NPins + j];
      for (int k = 0; k < Pixels.nrow(); k ++) {
        int xx = Pixels(k, 0);
        int yy = Pixels(k, 1);
        NouvImg[yy * LongueurMat + xx] = NouvImg[yy * LongueurMat + xx] + ForceString;
      }
      Error[i * NPins + j] = sum(pow(NouvImg - Img, 2)) / (LongueurMat * LargeurMat);
      
    }
    
  }
  // Find the pin and update the image
  int GoodPin = which_min(Error);
  double ErrorMin = Error[GoodPin];
  Pin1 = GoodPin / NPins;
  Pin2 = GoodPin - NPins * Pin1;
  Instructions(0, _) = IntegerVector::create(Pin1, Pin2);
  IntegerMatrix Pixels = ToutesLignes[Pin1 * NPins + Pin2];
  for (int k = 0; k < Pixels.nrow(); k ++) {
    int xx = Pixels(k, 0);
    int yy = Pixels(k, 1);
    ImgVide[yy * LongueurMat + xx] = ImgVide[yy * LongueurMat + xx] + ForceString;
  }
  
  bool EnCours = true; 
  int Index = 1;
  // Reset the starting pin to previous ending pin and optimize again
  PinDep = Pin2;
  
  while (EnCours) {
    
    if (Index == MaxPins) {
      Rcout << "Number of iterations: " << Index << ". Total error: " << ErrorCandidate << ".\n";
      break;
    } else if (remainder(Index, 50) == 0) {
      Rcout << "Number of iterations: " << Index << ". Total error: " << ErrorCandidate << ".\n";
    }
    
    // Rcout << PinDep << " / " << PinArr << " / " << Pin1 << " / " << Pin2 << "\n";
    NumericVector Error (NPins, R_PosInf); 
    for (int i = 0; i < NPins; i++) {
      if (i == PinDep) {
        continue;
      } 
      // else if (i < PinDep) {
      //   Pin1 = i;
      //   Pin2 = PinDep;
      // } else {
      //   Pin1 = PinDep;
      //   Pin2 = i;
      // }
      NumericVector NouvImg = clone(ImgVide);
      IntegerMatrix Pixels = ToutesLignes[PinDep * NPins + i];
      for (int k = 0; k < Pixels.nrow(); k ++) {
        int xx = Pixels(k, 0);
        int yy = Pixels(k, 1);
        NouvImg[yy * LongueurMat + xx] = NouvImg[yy * LongueurMat + xx] + ForceString;
      }
      Error[i] = sum(pow(NouvImg - Img, 2)) / (LongueurMat * LargeurMat);
    }
    // Rcout << Error << "\n";
    // Find the pin and update the image
    // Pin1 = PinDep;
    GoodPin = which_min(Error);
    // PinArr = GoodPin;
    // Rcout << GoodPin << " / " << PinDep << " / " << PinArr << "\n";
    ErrorCandidate = Error[GoodPin];
    if (ErrorCandidate > ErrorMin) {
      EnCours = false;
    } else {
      Instructions(Index, _) = IntegerVector::create(PinDep, GoodPin);
      Index++;
      // if (PinDep < PinArr) {
      //   Pin1 = PinDep;
      //   Pin2 = PinArr;
      // } else {
      //   Pin1 = PinArr;
      //   Pin2 = PinDep;
      // }
      IntegerMatrix Pixels = ToutesLignes[PinDep * NPins + GoodPin];
      for (int k = 0; k < Pixels.nrow(); k ++) {
        int xx = Pixels(k, 0);
        int yy = Pixels(k, 1);
        ImgVide[yy * LongueurMat + xx] = ImgVide[yy * LongueurMat + xx] + ForceString;
      }
      ErrorMin = ErrorCandidate;
      PinDep = GoodPin;
    }
    
    
    
  }
  
  
  
  return Instructions(Range(0, Index), _);
  
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
PixLine(1, 5, 10, 1)
GetCoordPixels(1, 10, 5, 1)
*/
