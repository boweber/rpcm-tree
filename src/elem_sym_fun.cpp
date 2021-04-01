#include <Rcpp.h>
#include <math.h>
#include <Rmath.h>
using namespace Rcpp;
using namespace R;

//' Generates a matrix containing every possible score combination
//' where each row sum is equal to the provided esf order.
//'
//' @param p The esf order of the elementary symmetric function.
//' @param M The number of items
//' @return Every possible score combination as a matrix
// [[Rcpp::export]]
NumericMatrix poly_idx_cpp(int p, int M)
{
  double P = R::choose(double(p + M) * 1.0, double(M * 1.0));
  NumericMatrix out(P, M);
  std::fill(out.begin(), out.end(), NumericVector::get_na());
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

// MARK: - Work in progress

// This part aims to handles the permutations which
// occur in a matrix containing all possibilities

long factorial(int n)
{
  int result = 1;
  for (int i = 1; i <= n; i++)
  {
    result = result * i;
  }
  return result;
}

long lfactorial(int n)
{
  return log(factorial(n));
}

std::tuple<double, double> handlePossibilityColumn(NumericVector possibilityColumn,
                                                   NumericVector itemDifficulties,
                                                   NumericVector itemTimeLimits,
                                                   bool shouldUseIndex,
                                                   int itemIndex)
{
  double result = 0;
  double firstOrderResult = 0;

  for (int index = 0; index < possibilityColumn.length(); index++)
  {
    double itemParameter = itemDifficulties[itemIndex] + itemTimeLimits[itemIndex];
    int y = possibilityColumn[index];
    result += y * itemParameter - lfactorial(y);
  }
  result = exp(result);
  if (shouldUseIndex)
  {
    firstOrderResult += result * possibilityColumn[itemIndex];
  }
  return std::make_tuple(result, firstOrderResult);
}

std::tuple<double, double> handlePermutationsInPossibilityColumn(NumericVector possibilityColumn,
                                                                 NumericVector itemDifficulties,
                                                                 NumericVector itemTimeLimits,
                                                                 bool shouldComputePermutations,
                                                                 bool shouldUseIndex,
                                                                 int itemIndex)
{

  if (shouldComputePermutations)
  {

    // To get all possibilities, the permutations are needed to be calculated
    // for each column.
    // E.g.: Currently a possibility matrix only contains the possibilities
    // independent of their order, like
    // y =      1   2
    //          3   0
    // Here, y is missing the following other possibilities (rowSum == 3):
    //          2   1
    //          0   3
    // Therefore, the permutations are required to get the desired output.

    // Note: Currently next_permutation mutates possibilityColumn -> copy possibilityColumn?!

    double result = 0;
    double firstOrderResult = 0;
    do
    {
      std::tuple<double, double> iterimResult = handlePossibilityColumn(possibilityColumn, itemDifficulties, itemTimeLimits, shouldUseIndex, itemIndex);
      result += std::get<0>(iterimResult);
      firstOrderResult += std::get<1>(iterimResult);
    } while (std::next_permutation(possibilityColumn.begin(), possibilityColumn.end()));

    return std::make_tuple(result, firstOrderResult);
  }
  else
  {
    return handlePossibilityColumn(possibilityColumn, itemDifficulties, itemTimeLimits, shouldUseIndex, itemIndex);
  }
}

std::tuple<double, double> handlePossibilityMatrix(NumericMatrix possibilities,
                                                   NumericVector itemDifficulties,
                                                   NumericVector itemTimeLimits,
                                                   bool shouldComputePermutations,
                                                   bool shouldUseIndex,
                                                   int itemIndex)
{
  double result = 0;
  double firstOrderResult = 0;
  for (int columnIndex = 0; columnIndex < possibilities.ncol(); columnIndex++)
  {
    NumericVector currentColumn = possibilities.column(columnIndex);
    std::tuple<double, double> iterimResult = handlePermutationsInPossibilityColumn(currentColumn, itemDifficulties, itemTimeLimits, shouldComputePermutations, shouldUseIndex, itemIndex);
    result += std::get<0>(iterimResult);
    firstOrderResult += std::get<1>(iterimResult);
  }

  return std::make_tuple(result, firstOrderResult);
}

List esf_c(NumericMatrix possibilities,
           NumericVector item_difficulties,
           NumericVector item_time_limits,
           bool should_compute_column_permutations,
           int order)
{
  // TODO: Transform matrix, if necessary

  if (order == 0)
  {
    // the 1 is just a dummy
    std::tuple<double, double> result = handlePossibilityMatrix(possibilities, item_difficulties, item_time_limits, should_compute_column_permutations, false, 1);
    NumericVector firstOrder = {std::get<0>(result)};
    List esfResult = List::create(firstOrder);
    return esfResult;
  }
  else
  {
    NumericVector derivatives(item_difficulties.length());
    double firstOrder = 0;
    for (int item_index = 0; item_index < item_difficulties.length(); item_index++)
    {
      std::tuple<double, double> possibilityResult = handlePossibilityMatrix(possibilities, item_difficulties, item_time_limits, should_compute_column_permutations, true, item_index);
      derivatives[item_index] = std::get<1>(possibilityResult);
      if (item_index == 0)
      {
        firstOrder = std::get<0>(possibilityResult);
      }
    }
    NumericVector firstOrderVector = {firstOrder};
    List firstOrderResult = List::create(firstOrderVector, derivatives);
    return firstOrderResult;
  }
}
