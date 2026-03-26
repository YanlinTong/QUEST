#include <Rcpp.h>
#include <cmath>
#include <limits>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix calculate_distancessq_cpp(NumericMatrix x, NumericMatrix v) {
  int n = x.nrow();
  int k = v.nrow();
  int d = x.ncol();

  NumericMatrix D(n, k);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      double dist = 0.0;
      for (int l = 0; l < d; l++) {
        double diff = x(i, l) - v(j, l);
        dist += diff * diff;
      }
      D(i, j) = dist;
    }
  }

  return D;
}

// [[Rcpp::export]]
double calculate_min_distance_cpp(NumericMatrix v) {
  int k = v.nrow();
  int d = v.ncol();

  double minv = std::numeric_limits<double>::infinity();

  for (int i = 0; i < k - 1; i++) {
    for (int j = i + 1; j < k; j++) {
      double dist = 0.0;
      for (int l = 0; l < d; l++) {
        double diff = v(i, l) - v(j, l);
        dist += diff * diff;
      }
      if (dist < minv) {
        minv = dist;
      }
    }
  }

  return minv;
}

// [[Rcpp::export]]
NumericVector calculate_vdiff_cpp(NumericMatrix v) {
  int k = v.nrow();
  int d = v.ncol();

  NumericVector vmean(d);
  for (int j = 0; j < d; j++) {
    for (int i = 0; i < k; i++) {
      vmean[j] += v(i, j);
    }
    vmean[j] /= k;
  }

  NumericVector vdiff(k);
  for (int i = 0; i < k; i++) {
    double dist = 0.0;
    for (int j = 0; j < d; j++) {
      double diff = v(i, j) - vmean[j];
      dist += diff * diff;
    }
    vdiff[i] = dist;
  }

  return vdiff;
}

// [[Rcpp::export]]
double calculate_finalxb_index_cpp(NumericMatrix u, NumericMatrix D, double minv, double m, int n) {
  int rows = u.nrow();
  int cols = u.ncol();
  double numerator = 0.0;

  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
      numerator += std::pow(u(i, j), m) * D(i, j);
    }
  }

  if (minv <= 0.0) {
    return NA_REAL;
  }

  return numerator / (n * minv);
}

// [[Rcpp::export]]
double calculate_finalkwon_index_cpp(NumericMatrix u, NumericMatrix D, NumericVector vdiff, double minv, double m, int k) {
  int n = u.nrow();
  int clusters = u.ncol();
  double numerator = 0.0;

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < clusters; j++) {
      numerator += std::pow(u(i, j), m) * D(i, j);
    }
  }

  numerator += sum(vdiff) / k;

  if (minv <= 0.0) {
    return NA_REAL;
  }

  return numerator / minv;
}
