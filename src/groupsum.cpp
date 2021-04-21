#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector groupsum(const NumericVector& x, int ng = 0, const IntegerVector& g = 0) {
  int l = x.size();
  if(l < 1) return x;
  if(g.size() != l) stop("length(g) must match nrow(X)");

  NumericVector sum(ng);
  int ngs = 0;
  for(int i = 0; i != l; ++i) {
    sum[g[i]-1] += x[i];
  }

  return sum;
}
