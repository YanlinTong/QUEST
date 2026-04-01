#' Calculate the XB index
#'
#' Compute the Xie-Beni cluster validity index from data, a membership matrix,
#' and cluster centers.
#'
#' @param x A numeric matrix of dimension \eqn{n \times d}, where \eqn{n}
#'   is the number of observations and \eqn{d} is the number of features.
#' @param u A numeric matrix of dimension \eqn{n \times k}, where \eqn{n}
#'   is the number of observations and \eqn{k} is the number of clusters.
#'   Each row should represent a membership vector.
#' @param v A numeric matrix of dimension \eqn{k \times d}, where \eqn{k}
#'   is the number of clusters and \eqn{d} is the number of features.
#'   Each row should represent a cluster center.
#' @param m A numeric value greater than 1 specifying the fuzziness parameter.
#'
#' @return A numeric value representing the XB index.
#' @export
compute_xb <- function(x, u, v, m = 2) {
  x <- as.matrix(x)
  u <- as.matrix(u)
  v <- as.matrix(v)

  if (!is.numeric(x)) {
    stop("x must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (!is.numeric(u)) {
    stop("u must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (!is.numeric(v)) {
    stop("v must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (any(is.na(x))) {
    stop("x must not contain missing values.")
  }
  if (any(is.na(u))) {
    stop("u must not contain missing values.")
  }
  if (any(is.na(v))) {
    stop("v must not contain missing values.")
  }
  if (!is.numeric(m) || length(m) != 1 || is.na(m) || m <= 1) {
    stop("m must be a single numeric value greater than 1.")
  }

  if (nrow(x) != nrow(u)) {
    stop("The number of rows in x and u must be equal.")
  }
  if (ncol(x) != ncol(v)) {
    stop("The number of columns in x and v must be equal.")
  }
  if (ncol(u) != nrow(v)) {
    stop("The number of columns in u must equal the number of rows in v.")
  }

  n <- nrow(x)
  D <- calculate_distancessq_cpp(x, v)
  minv <- calculate_min_distance_cpp(v)

  calculate_finalxb_index_cpp(u, D, minv, m, n)
}


#' Calculate the Kwon index
#'
#' Compute the Kwon cluster validity index from data, a membership matrix,
#' and cluster centers.
#'
#' @param x A numeric matrix of dimension \eqn{n \times d}, where \eqn{n}
#'   is the number of observations and \eqn{d} is the number of features.
#' @param u A numeric matrix of dimension \eqn{n \times k}, where \eqn{n}
#'   is the number of observations and \eqn{k} is the number of clusters.
#'   Each row should represent a membership vector.
#' @param v A numeric matrix of dimension \eqn{k \times d}, where \eqn{k}
#'   is the number of clusters and \eqn{d} is the number of features.
#'   Each row should represent a cluster center.
#' @param m A numeric value greater than 1 specifying the fuzziness parameter.
#'
#' @return A numeric value representing the Kwon index.
#' @export
compute_kwon <- function(x, u, v, m = 2) {
  x <- as.matrix(x)
  u <- as.matrix(u)
  v <- as.matrix(v)

  if (!is.numeric(x)) {
    stop("x must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (!is.numeric(u)) {
    stop("u must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (!is.numeric(v)) {
    stop("v must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (any(is.na(x))) {
    stop("x must not contain missing values.")
  }
  if (any(is.na(u))) {
    stop("u must not contain missing values.")
  }
  if (any(is.na(v))) {
    stop("v must not contain missing values.")
  }
  if (!is.numeric(m) || length(m) != 1 || is.na(m) || m <= 1) {
    stop("m must be a single numeric value greater than 1.")
  }

  if (nrow(x) != nrow(u)) {
    stop("The number of rows in x and u must be equal.")
  }
  if (ncol(x) != ncol(v)) {
    stop("The number of columns in x and v must be equal.")
  }
  if (ncol(u) != nrow(v)) {
    stop("The number of columns in u must equal the number of rows in v.")
  }

  k <- ncol(u)

  D <- calculate_distancessq_cpp(x, v)
  vdiff <- calculate_vdiff_cpp(v)
  minv <- calculate_min_distance_cpp(v)

  calculate_finalkwon_index_cpp(u, D, vdiff, minv, m, k)
}
