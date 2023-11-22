#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;

// set seed
// [[Rcpp::export]]
void set_seed(unsigned int seed) {
  Rcpp::Environment base_env("package:base");
  Rcpp::Function set_seed_r = base_env["set.seed"];
  set_seed_r(seed);
}

// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::export]]
int CountDeci(int nb, int somme, int seed) {
  int compte = 0;
  IntegerVector draws = IntegerVector::create(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  set_seed(seed);
  Progress p(nb, true);
  for (int i = 0; i < nb; i++) {
    int somme_deci = 0;
    while (somme_deci < somme) {
      IntegerVector deci = sample(draws, 1);
      somme_deci += deci[0];
    }
    p.increment();
    if (somme_deci == somme) {
      compte = compte + 1;
    }
  }
  return compte;
}

