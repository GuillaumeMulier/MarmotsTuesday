#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector PointSuivant(double XCurrent, double YCurrent,
                           double XPrec, double YPrec,
                           NumericVector XPoints, NumericVector YPoints,
                           int Methode,
                           NumericVector Poids, int Shift) {
  // Variables à stocker
  int n = XPoints.length();
  IntegerVector Point = sample(n, 1, false) - 1;
  int Pt = Point[0];
  double PtX = XPoints[Pt];
  double PtY = YPoints[Pt];
  double XSuiv;
  double YSuiv;

  if (Methode == 1) { // 1st method = we can choose without restriction

    XSuiv = (Poids[0] * XCurrent + Poids[1] * PtX) / (Poids[0] + Poids[1]);
    YSuiv = (Poids[0] * YCurrent + Poids[1] * PtY) / (Poids[0] + Poids[1]);

  } else if (Methode == 2) {

    PtX = XPoints[Pt];
    PtY = YPoints[Pt];
    while (PtX == XPrec & PtY == YPrec) {
      Point = Rcpp::sample(n, 1, false) - 1;
      Pt = Point[0];
      PtX = XPoints[Pt];
      PtY = YPoints[Pt];
    }
    XSuiv = (Poids[0] * XCurrent + Poids[1] * PtX) / (Poids[0] + Poids[1]);
    YSuiv = (Poids[0] * YCurrent + Poids[1] * PtY) / (Poids[0] + Poids[1]);

  } else if (Methode == 3) {

    if (PtX == XPrec & PtY == YPrec) {
      Pt = (Pt + Shift) % n;
      PtX = XPoints[Pt];
      PtY = YPoints[Pt];
    }
    XSuiv = (Poids[0] * XCurrent + Poids[1] * PtX) / (Poids[0] + Poids[1]);
    YSuiv = (Poids[0] * YCurrent + Poids[1] * PtY) / (Poids[0] + Poids[1]);

  } else if (Methode == 4) {

    if (Pt % Shift == 0) {
      XSuiv = (Poids[2] * XCurrent + Poids[3] * PtX) / (Poids[2] + Poids[3]);
      YSuiv = (Poids[2] * YCurrent + Poids[3] * PtY) / (Poids[2] + Poids[3]);
    } else {
      XSuiv = (Poids[0] * XCurrent + Poids[1] * PtX) / (Poids[0] + Poids[1]);
      YSuiv = (Poids[0] * YCurrent + Poids[1] * PtY) / (Poids[0] + Poids[1]);
    }


  }

  return NumericVector::create(XSuiv, YSuiv, PtX, PtY, Pt);
}


// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::export]]
DataFrame CreerPoints(int NPts, double XDepart, double YDepart, NumericVector XPoints, NumericVector YPoints,
                                            NumericVector Poids, int Methode, int Shift = 0) {

    // Progress bar
    Progress p(NPts - 1, true);

    // Storage vectors for points
    NumericVector Xs (NPts);
    NumericVector Ys (NPts);
    NumericVector Pts (NPts);
    Xs[0] = XDepart;
    Ys[0] = YDepart;
    Pts[0] = 0;
    double Xp = XDepart;
    double Yp = YDepart;
    double XPrec = XDepart;
    double YPrec = YDepart;

    NumericVector NvPt;

    for (int i = 1; i < NPts; i++) {
      NvPt = PointSuivant(Xp, Yp, XPrec, YPrec, XPoints, YPoints, Methode, Poids, Shift);
      Xs[i] = NvPt[0];
      Xp = NvPt[0];
      Ys[i] = NvPt[1];
      Yp = NvPt[1];
      XPrec = NvPt[2];
      YPrec = NvPt[3];
      Pts[i] = NvPt[4];
      p.increment();
    }

    return DataFrame::create(Named("X") = Xs, Named("Y") = Ys, Named("P") = Pts);

}


// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::export]]
List Rasterize(DataFrame DataPts,
                        int Largeur, int Longueur,
                        NumericVector RangeX, NumericVector RangeY,
                        NumericVector Rouges, NumericVector Vert, NumericVector Bleu) {

  // Precompute variables and storage
  IntegerMatrix ResultatsN(Longueur, Largeur);
  IntegerMatrix ResultatsR(Longueur, Largeur);
  IntegerMatrix ResultatsG(Longueur, Largeur);
  IntegerMatrix ResultatsB(Longueur, Largeur);
  int NLignes = DataPts.nrows();
  IntegerVector Ps = DataPts["P"];
  NumericVector Xs = DataPts["X"];
  NumericVector Ys = DataPts["Y"];
  IntegerVector TicksXInt = seq_len(Largeur);
  NumericVector TicksXNum = as<NumericVector>(TicksXInt);
  TicksXNum = (TicksXNum - 1) * (RangeX[1] - RangeX[0]) / Largeur + RangeX[0];
  IntegerVector TicksYInt = seq_len(Longueur);
  NumericVector TicksYNum = as<NumericVector>(TicksYInt);
  TicksYNum = (TicksYNum - 1) * (RangeY[1] - RangeY[0]) / Longueur + RangeY[0];

  // Progress bar
  Progress p(NLignes, true);

  // Loop over the data.frame to convert
  for (int i = 0; i < NLignes; i++) {
    int l = sum(Ys[i] <= TicksYNum) - 1;
    int c = sum(Xs[i] >= TicksXNum) - 1;
    ResultatsN(l, c)++;
    ResultatsR(l, c) = ResultatsR(l, c) + Rouges[Ps[i]];
    ResultatsG(l, c) = ResultatsG(l, c) + Vert[Ps[i]];
    ResultatsB(l, c) = ResultatsB(l, c) + Bleu[Ps[i]];
    p.increment();
  }

  return List::create(ResultatsN, ResultatsR, ResultatsG, ResultatsB);

}


