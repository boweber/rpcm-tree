#include <Rcpp.h>
#include <math.h>
#include <Rmath.h>
using namespace Rcpp;
using namespace R;

NumericMatrix na_matrix(int n, int l){
  NumericMatrix m(n,l) ;
  std::fill( m.begin(), m.end(), NumericVector::get_na() ) ;
  return m ;
}

//' Generates a matrix containing every possible score combination
//' where each row sum is equal to the provided esf order. 
//' 
//' @param p The esf order of the elementary symmetric function.
//' @param M The number of items
//' @return Every possible score combination as a matrix
//' @examples
//' poly_idx_cpp(5, 7)
// [[Rcpp::export]]
NumericMatrix poly_idx_cpp(int p, int M) {
  double P = R::choose(double (p + M)*1.0, double (M*1.0));
  NumericMatrix out = na_matrix(P, M);
  NumericVector tA (M);
  int l = 0;
  int pmax = pow((p + 1), M);
  for (int i = 1; i < (pmax + 1); i++) {
    int ri = i;
    for (int d = 0; d < M; d++) {
      int md = pow((p + 1), (M - d - 1));
      int val = floor(ri / md);
      tA[d] = val;
      ri = ri - val * md;
    }
    if (sum(tA) == p) {
      out(l, _ ) = tA;
      l = l + 1;
    }
  }
  return out;
}



