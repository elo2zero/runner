#include <Rcpp.h>
using namespace Rcpp;

//' Length of running windows
//'
//' Number of elements in k-long window calculated on idx vector.
//' If idx is an `as.integer(date)` vector, then k=number of days in window -
//' then the result is number of observations within k days window.
//' @param k integer vector which specifies window length
//' @param lag integer vector which specifies window shift
//' @param idx an optional integer vector containing index of observations.
//' @examples
//' length_run(k = 3, idx = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5))
//' @export
// [[Rcpp::export]]
IntegerVector length_run(IntegerVector k = IntegerVector(1),
                         IntegerVector lag = IntegerVector(1),
                         IntegerVector idx = IntegerVector(0)) {
  int n = idx.size();
  if (n == 0) {
    stop("idx should be of length > 0");
  }

  if(k.size() != n and k.size() > 1){
    stop("length of k and length of idx differs. length(k) should be 1 or equal to idx");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }

  if(lag.size() != n and lag.size() > 1){
    stop("length of lag and length of lag differs. length(lag) should be 1 or equal to lag");
  } else if( Rcpp::any(Rcpp::is_na(lag)) ){
    stop("Function doesn't accept NA values in lag vector");
  }

  IntegerVector res(n);
  if ((k.size() == 1)) {
    for (int i = 0; i < n; i++) {
      for (int j = i; j >= 0; j--) {
        if ((idx(i) - idx(j)) > (k(0) - 1)) {
          res(i) = i - j;
          break;
        } else if (j == 0){
          res(i) = NA_INTEGER;
        }
      }
    }

  // IDX VARYING WINDOW -----------
  } else if((k.size() > 1)) {
    for (int i = 0; i < n; i++) {
      for (int j = i; j >= 0; j--) {
        if ((idx(i) - idx(j)) > (k(i) - 1)) {
          res(i) = i - j;
          break;
        } else if (j == 0) {
          res(i) = NA_INTEGER;
        }
      }
    }
  }
  return res;
}
