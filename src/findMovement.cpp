#include <Rcpp.h>
#include <queue>

using namespace Rcpp;
using namespace std;

NumericVector reverse(NumericVector v) {
  if (all(is_na(v)).is_true())
    return v;
  
  NumericVector out = NumericVector(v.size(), NumericVector::get_na());
  int m = max(na_omit(v));

  for (int i = v.size() - 1; i >= 0; i--) {
    if (!NumericVector::is_na(v[i]))
      out[i] = m - v[i] + 1;
  }

  return NumericVector (rev(out));
}

//' Find subsequences based on movement
//'
//' Given a sequence of numbers, as a numeric vector, find the
//' increasing or decreasing subsequences.
//'
//' @param v A numeric vector.
//' @param direction Character string of either "up" to find
//'          increasing sequences or "down" to find decreasing
//'          sequences. Default: "up".
//' @param buffer Optional parameter indicating how many repeated
//'          elements to include to the left (prior to the start) and
//'          right (following the end) of a subsequence.
//' @param lbuffer Optional parameter indicating how many repeated
//'          elements to include to the left (prior to the start) of a
//'          subsequence. Takes precedence over the option
//'          `buffer`.
//' @param rbuffer Optional parameter indicating how many repeated
//'          elements to include to the right (following the end) of a
//'          subsequence. Takes precedence over the option
//'          `buffer`.
//' @param upper_lim Optional parameter that filters out
//'           subsequences which do not reach an upper lim.
//' @param lower_lim Optional parameter that filters out
//'           subsequences which do not begin below a given lim.
//'
//' @details `findMovement` finds the subsequences within a
//'           numeric vector based on either increasing or decreasing
//'           movement. An increasing subsequence is defined as a
//'           sequence of values where `V[n] >= V[n - 1]`. The
//'           start point is strict, meaning the first point where `V[n] <
//'           V[n + 1]`. The option `lbuffer` can be specified to
//'           include an arbitrary number, `k`, of constant
//'           elements prior to `V[n]` where `V[n - k] ==
//'           V[n]`. Likewise, the end point of a subsequence is the
//'           element where `V[n] > V[n + 1]` or `V[n + 1]` is
//'           NA. `rbuffer` will also include a constant number
//'           of elements after the end point. Alternatively, the
//'           option `buffer` can be used to specify equal
//'           buffering at both the start and end of subsequences.
//'
//' @return A numeric vector of the same length as `v` where
//'          subsequences are given values 1:N and elements falling
//'          outside of subsequences are set to `NA`.
//'
//'
//' @examples
//' x <- c(4, 4, 3, 1, 0, 1, 0, 1, 2, 2, 2, 3, 3)
//' findMovement(x)
//' findMovement(x, direction = "down")
//'
//' # Let's find buffered subsequences that increase starting at 2
//' findMovement(x, buffer = 1, lower_lim = 2)
//'
//' @export
// [[Rcpp::export]]
NumericVector findMovement(NumericVector v,
                           String direction = "up",
                           int buffer = 0,
                           Nullable<int> lbuffer = R_NilValue,
                           Nullable<int> rbuffer = R_NilValue,
                           Nullable<int> upper_lim = R_NilValue,
                           Nullable<int> lower_lim = R_NilValue) {
  if (direction != "down" && direction != "up")
    stop("Invalid direction argument");

  NumericVector vseq;
  int startbufsiz, stopbufsiz;
  if (direction == "down") {
    vseq = NumericVector(rev(v));

    startbufsiz = (rbuffer.isNotNull()) ? as<int>(rbuffer) : buffer;
    stopbufsiz = (lbuffer.isNotNull()) ? as<int>(lbuffer) : buffer;
  } else {
    vseq = v;

    startbufsiz = (lbuffer.isNotNull()) ? as<int>(lbuffer) : buffer;
    stopbufsiz = (rbuffer.isNotNull()) ? as<int>(rbuffer) : buffer;
  }

  if (startbufsiz < 0 || stopbufsiz < 0)
    stop("Buffer values cannot be less than 0");

  NumericVector out = NumericVector(vseq.size(), NumericVector::get_na());
  NumericVector d = diff(vseq);

  if (is_true(all(is_na(d) | d <= 0)))
    return out;

  queue<int> stack;
  int count = 1, stop = -1;

  size_t d_len = d.size();
  for (int i = 0; i < d_len; i++) {
    // Hooray, the start of a subsequence!
    if (d[i] > 0)
      stack.push(i);

    if (i == d_len - 1 || NumericVector::is_na(d[i]) || d[i] < 0) {
      // We haven't found a starting point yet, keep looking!
      if (stack.empty()) {
        stop = i;

        continue;
      }

      // TODO: pop stack instead of calling .size()
      int head = stack.front();
      int tail;
      if (stack.size() > 1)
        tail = stack.back() + 1;
      else
        tail = head + 1;

      // If we don't start below lower_lim or end above
      // upper_lim, flush queue and search for next seq
      if ((upper_lim.isNotNull() && v[tail] < as<int>(upper_lim)) ||
          (lower_lim.isNotNull() && v[head] > as<int>(lower_lim))) {
        stack = queue<int>();
        stop = i;
        
        continue;
      }
      
      // Buffer beginning of sequence
      if (startbufsiz != 0 && head != stop) {
        int b = min(head - stop - 1, startbufsiz);
        head -= b;
      }

      // Buffer end of sequence
      if (stopbufsiz != 0 && tail != i + 1) {
        // If we're at the end of seq and it's not a stop value, we need to offset by 1
        int end_offset = (i == d_len - 1 && d[i] >= 0) ? 1 : 0;

        int b = min(i - tail + end_offset, stopbufsiz);
        tail += b;
      }

      NumericVector sub = NumericVector(tail - head + 1);
      sub.fill(count);

      out[seq(head, tail)] = sub;

      stack = queue<int>();
      count++;
      stop = i;
    }
  }

  return (direction == "down") ? reverse(out) : out;
}
