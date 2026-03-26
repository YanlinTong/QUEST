#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix calculate_dissimilarities_cpp(NumericMatrix x, NumericMatrix centers) {
  int n = x.nrow();
  int d = x.ncol();
  int k = centers.nrow();

  NumericMatrix distances(n, k);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      double sum = 0.0;
      for (int l = 0; l < d; l++) {
        double diff = x(i, l) - centers(j, l);
        sum += diff * diff;
      }
      distances(i, j) = std::sqrt(sum);
    }
  }

  return distances;
}

// [[Rcpp::export]]
NumericMatrix update_memberships_cpp(NumericMatrix d, double m) {
  int n = d.nrow();
  int k = d.ncol();

  NumericMatrix u(n, k);
  double p = 1.0 / (m - 1.0);

  for (int i = 0; i < n; i++) {
    double row_sum = 0.0;
    bool has_zero = false;

    for (int j = 0; j < k; j++) {
      if (d(i, j) == 0.0) {
        has_zero = true;
        u(i, j) = 1.0;
      } else {
        double val = 1.0 / std::pow(d(i, j), 2.0);
        u(i, j) = std::pow(val, p);
        row_sum += u(i, j);
      }
    }

    if (has_zero) {
      for (int j = 0; j < k; j++) {
        if (d(i, j) != 0.0) {
          u(i, j) = 0.0;
        }
      }
    } else if (row_sum > 0.0) {
      for (int j = 0; j < k; j++) {
        u(i, j) /= row_sum;
      }
    }
  }

  return u;
}

// [[Rcpp::export]]
NumericMatrix update_prototypes_cpp(NumericMatrix x, NumericMatrix u, double m) {
  int n = x.nrow();
  int d = x.ncol();
  int k = u.ncol();

  NumericMatrix centers(k, d);
  NumericVector u_m(n);

  for (int j = 0; j < k; j++) {
    double denominator = 0.0;

    for (int i = 0; i < n; i++) {
      u_m[i] = std::pow(u(i, j), m);
      denominator += u_m[i];
    }

    for (int l = 0; l < d; l++) {
      double numerator = 0.0;
      for (int i = 0; i < n; i++) {
        numerator += u_m[i] * x(i, l);
      }
      centers(j, l) = (denominator > 0.0) ? numerator / denominator : NA_REAL;
    }
  }

  return centers;
}

// [[Rcpp::export]]
double calculate_error_cpp(NumericMatrix u, NumericMatrix d, double m) {
  int n = u.nrow();
  int k = u.ncol();
  double error = 0.0;

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      double u_m = std::pow(u(i, j), m);
      double d_sq = d(i, j) * d(i, j);
      error += u_m * d_sq;
    }
  }

  return error;
}
