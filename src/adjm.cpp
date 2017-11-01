#include <Rcpp.h>

using namespace Rcpp;

//' Adjacency Matrix
//'
//' Create an adjacency matrix from two ordinal variables.
//'
//' @param x `NumericVector`.
//' @param y `NumericVector`.
//' @param states Optional integer. Specify the max number of states
//'          between the two vectors. The outputed matrix will be of
//'          size `states ^ 2 x states ^ 2`. Default: `max(a, b)`.
//' @param direction Optional string. Specify "up" to create an
//'          adjacency matrix from only increasing transitions, or
//'          "down" for decreasing transitions.
//'
//' @section Warning: `adj_matrix` assumes that the target
//'            variables are indexed at 0.
//'
//' @details The adjacency matrix represents paired transitions for two
//'          variables. If we consider each possible unique pair as
//'          serialized on an ordinal scale (`1, 2, .., K`) then the
//'          column numbers of the adjacency matrix denote the start
//'          position of the transition (the observed pair at
//'          observation `i`) and the row numbers denote the end
//'          position of the transition (the observed pair at
//'          observation `i + 1`). This can be visualized graphically
//'          with `plot` or as an `igraph` adjacency graph with
//'          [igraph::graph.adjacency].
//'
//' @return A `S3` matrix of class `adj_mat`. Can be plotted with the
//'         normal `plot` function.
//'
//' @examples
//' x <- c(1, 2, 2)
//' y <- c(2, 1, 2)
//' adj_matrix(x, y) # `states` will be set to 3
//'
//' @export
// [[Rcpp::export]]
NumericMatrix adj_matrix(NumericVector x, NumericVector y,
                         Nullable<int> states = R_NilValue,
                         Nullable<String> direction = R_NilValue) {
  int x_len = x.size();
  int y_len = y.size();

  if (x_len != y_len)
    stop("Expects two numeric vectors of the same length");
  
  if (x_len < 2)
    stop("Expects two or more observations");

  String dir;
  if (direction.isNotNull()) {
    dir = as<String>(direction);

    if (dir != "up" && dir != "down")
      stop("Invalid direction argument");
  }

  int n;
  if (states.isNotNull()) {
    n = as<int>(states);
  } else {
    int x_max = max(na_omit(x)) + 1;
    int y_max = max(na_omit(y)) + 1;

    n = (x_max > y_max) ? x_max : y_max;
  }

  // TODO: index starting at 1
  int siz = pow(n, 2);

  // TODO: use a sparse matrix
  NumericMatrix out(siz, siz);

  LogicalVector na_v = is_na(x) | is_na(y);

  for (unsigned int i = 0; i < x_len - 1; i++) {
    if (na_v[i] || na_v[i + 1])
      continue;

    if (direction.isNotNull()) {
      int d = (x[i + 1] - x[i]) + (y[i + 1] - y[i]);

      if ((dir == "up" && d <= 0) ||
          (dir == "down" && d >= 0))
        continue;
    }

    int from = (n * x[i]) + y[i];
    int to = (n * x[i + 1]) + y[i + 1];

    out(from, to)++;
  }

  out.attr("class") = CharacterVector::create("adj_mat", "matrix");
  return out;
}
