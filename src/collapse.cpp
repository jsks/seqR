#include <numeric>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

template <int RTYPE> LogicalVector lagged_compare_t(Vector<RTYPE> v) {
  LogicalVector out(v.size(), false);

  for (unsigned int i = v.size() - 1; i > 0; i--) {
    if (v[i] != v[i - 1])
      out[i] = true;
  }

  out[0] = true;
  return out;
}

LogicalVector lagged_compare(SEXP v) {
  // Only allow subset of vector types that support using `!=` op
  switch (TYPEOF(v)) {
    case REALSXP:
      return lagged_compare_t<REALSXP>(v);
    case INTSXP:
      return lagged_compare_t<INTSXP>(v);
    case STRSXP:
      return lagged_compare_t<STRSXP>(v);
    case LGLSXP:
      return lagged_compare_t<LGLSXP>(v);
    default:
      stop("Unsupported type in column");
  }
}


template <int RTYPE> Vector<RTYPE> subset_t(Vector<RTYPE> v, LogicalVector b) {
  return v[b];
}

SEXP subset(SEXP v, LogicalVector b) {
  RCPP_RETURN_VECTOR(subset_t, v, b);
}

//' Collapse a data frame
//'
//' Collapse unchanging consecutive observations for a given data frame.
//'
//' @param data A data.frame. Data to collapse 
//'
//' @details If the given data frame contains multiple columns, then
//'          an observation is only collapsed if all of the columns
//'          are unchanging.
//'   
//'          Row and column names will be preserved; however, other
//'          user defined attributes will be dropped.
//'
//' @section Warning: `collapse` currently only works with REALSXP,
//'           INTSXP, STRSXP, LGLSXP objects. This covers all of the
//'           common atomic R data types, including more complex types
//'           like `Date`. Unfortunately however, this means that
//'           `collapse` will return an error if a column has a class
//'           of `raw`. Bummer.
//'
//' @return A `data.frame` of row size: `0 < N <= nrow(data)`.
//'
//' @examples
//' x <- data.frame(x = c(1, 1, 1), y = c(1, 2, 2))
//' collapse(x)
//'
//' y <- data.frame(x = c(1, 1, 1), y = c("a", "a", "b"), stringsAsFactors = FALSE)
//' collapse(y)
//'
//' @export
// [[Rcpp::export]]
List collapse(DataFrame data) {
  int ncols = data.size();

  if (data.nrow() < 2)
    return data;
  
  vector<LogicalVector> col_bools(ncols);

  for (unsigned int i = 0; i < ncols; i++) {
    col_bools[i] = lagged_compare(data[i]);
  }

  auto f = [](LogicalVector m, LogicalVector n) { return m | n; };
  LogicalVector bools = accumulate(next(col_bools.begin()), col_bools.end(),
                                   col_bools[0], f);

  List out(ncols);
  for (unsigned int i = 0; i < ncols; i++) {
    out[i] = subset(data[i], bools);
  }

  out.attr("names") = data.attr("names");
  out.attr("row.names") = subset(data.attr("row.names"), bools);
  out.attr("class") = "data.frame";

  return out;
}


