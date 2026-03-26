#' Calculate the XB index
#'
#' Compute the Xie-Beni index from data, membership matrix, and cluster centers.
#'
#' @param x A numeric matrix of observations.
#' @param u Membership matrix.
#' @param v Cluster centers.
#' @param m Fuzziness parameter.
#'
#' @return A numeric value representing the XB index.
#' @export
compute_xb <- function(x, u, v, m = 2) {
  if (nrow(x) != nrow(u)) {
    stop("The number of rows in the data matrix and membership matrix must be equal.")
  }
  if (ncol(x) != ncol(v)) {
    stop("The number of columns in the data matrix and cluster center matrix must be equal.")
  }
  if (ncol(u) != nrow(v)) {
    stop("The number of columns in the membership matrix and the number of rows in the cluster center matrix must be equal.")
  }

  x <- as.matrix(x)
  u <- as.matrix(u)
  v <- as.matrix(v)

  n <- nrow(x)
  D <- calculate_distancessq_cpp(x, v)
  minv <- calculate_min_distance_cpp(v)

  calculate_finalxb_index_cpp(u, D, minv, m, n)
}

#' Calculate the Kwon index
#'
#' Compute the Kwon cluster validity index from data, membership matrix,
#' and cluster centers.
#'
#' @param x A numeric matrix of observations.
#' @param u Membership matrix.
#' @param v Cluster centers.
#' @param m Fuzziness parameter.
#'
#' @return A numeric value representing the Kwon index.
#' @export
compute_kwon <- function(x, u, v, m = 2) {
  x <- as.matrix(x)
  u <- as.matrix(u)
  v <- as.matrix(v)

  n <- nrow(x)
  k <- ncol(u)

  D <- calculate_distancessq_cpp(x, v)
  vdiff <- calculate_vdiff_cpp(v)
  minv <- calculate_min_distance_cpp(v)

  calculate_finalkwon_index_cpp(u, D, vdiff, minv, m, k)
}
