#include <Rcpp.h>
#include <math.h>
#include <Rmath.h>
using namespace Rcpp;
using namespace R;

NumericMatrix na_matrix(int n, int l)
{
  NumericMatrix m(n, l);
  std::fill(m.begin(), m.end(), NumericVector::get_na());
  return m;
}

long factorial(int n)
{
  if (n == 0)
  {
    return 1;
  }
  else
  {
    return (n * factorial(n - 1));
  }
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
NumericMatrix poly_idx_cpp(int p, int M)
{
  double P = R::choose(double(p + M) * 1.0, double(M * 1.0));
  NumericMatrix out = na_matrix(P, M);
  NumericVector tA(M);
  int l = 0;
  int pmax = pow((p + 1), M);
  for (int i = 1; i < (pmax + 1); i++)
  {
    int ri = i;
    for (int d = 0; d < M; d++)
    {
      int md = pow((p + 1), (M - d - 1));
      int val = floor(ri / md);
      tA[d] = val;
      ri = ri - val * md;
    }
    if (sum(tA) == p)
    {
      out(l, _) = tA;
      l = l + 1;
    }
  }
  return out;
}

// FIXME: Currently not working
List rpcm_esf_c(int rawScore,
                NumericVector itemDifficulties,
                NumericVector itemTimeLimits,
                int order,
                int rawScoreIndex)
{

  double first_order_result = 0;
  NumericMatrix possibilities = poly_idx_cpp(rawScore, itemTimeLimits.length());
  NumericVector itemParameters = itemDifficulties + itemTimeLimits;
  for (int p_index = 0; p_index < possibilities.nrow(); p_index++)
  {
    double p_col_result = 0;
    for (int p_col_index = 0; p_col_index < possibilities.ncol(); p_col_index++)
    {
      int y = possibilities(p_index, p_col_index);
      // log(exp(itemParameters[p_col_index])) == itemParameters[p_col_index]
      p_col_result = p_col_result + (y * itemParameters[p_col_index] - log(factorial(y)));
    }
    first_order_result = first_order_result + exp(p_col_result);
  }

  NumericVector first_order = {first_order_result};

  if (order == 0)
  {
    List result = List::create(first_order);
    return result;
  }
  else
  {
    NumericVector derivatives(itemDifficulties.length());
    for (int item_index = 0; item_index < itemDifficulties.length(); item_index++)
    {
      derivatives[item_index] = (possibilities(rawScoreIndex, item_index) / itemDifficulties[item_index]) * first_order_result;
    }

    List result = List::create(first_order, derivatives);
    return result;
  }
}
