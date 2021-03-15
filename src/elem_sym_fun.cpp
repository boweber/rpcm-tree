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
  int result = 1;
  for (int i = 1; i <= n; i++)
  {
    result = result * i;
  }
  return result;
}

double applyESFComputation(NumericMatrix matrix,
                           int factorIndex,
                           bool shouldUseIndex,
                           NumericVector itemDifficulties,
                           NumericVector itemTimeLimits)
{
  double result = 0;
  for (int row_index = 0; row_index < matrix.nrow(); row_index++)
  {
    double row_result = 0;
    for (int col_index = 0; col_index < matrix.ncol(); col_index++)
    {
      int y = matrix(row_index, col_index);
      double itemParameter = itemDifficulties[col_index] + itemTimeLimits[col_index];
      // exp(log(itemParameters[col_index])) = itemParameters[col_index]
      row_result = row_result + (y * itemParameter - log(factorial(y)));
    }
    row_result = exp(row_result);
    if (shouldUseIndex)
    {
      int matrixValue = matrix(row_index, factorIndex);
      row_result = row_result * matrixValue;
    }

    result = result + row_result;
  }
  return result;
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

// [[Rcpp::export]]
List rpcm_esf_c(int rawScore,
                NumericVector itemDifficulties,
                NumericVector itemTimeLimits,
                int order)
{
  NumericMatrix possibilities = poly_idx_cpp(rawScore, itemTimeLimits.length());
  // the 1 is just a dummy
  NumericVector first_order = {applyESFComputation(possibilities, 1, false, itemDifficulties, itemTimeLimits)};

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
      derivatives[item_index] = applyESFComputation(possibilities, item_index, true, itemDifficulties, itemTimeLimits);
    }

    List result = List::create(first_order, derivatives);
    return result;
  }
}

// [[Rcpp::export]]
double rpcm_log_likelihood_c(NumericVector itemDifficulties,
                             NumericVector colSums,
                             NumericVector rowSums,
                             NumericVector itemTimeLimits,
                             double factorialComponent)
{
  double esfResult = 0;
  for (int rowSumIndex = 0; rowSumIndex < rowSums.length(); rowSumIndex++)
  {
    List esfList = rpcm_esf_c(rowSums[rowSumIndex], itemDifficulties, itemTimeLimits, 0);
    NumericVector esfVector = esfList[0];
    double esf = esfVector[0];
    esfResult += log(esf);
  }
  double colSumResult = 0;
  for (int colSumIndex = 0; colSumIndex < colSums.length(); colSumIndex++)
  {
    double itemParameter = itemDifficulties[colSumIndex] + itemTimeLimits[colSumIndex];
    colSumResult += itemParameter * colSums[colSumIndex];
  }

  return colSumResult - factorialComponent - esfResult;
}