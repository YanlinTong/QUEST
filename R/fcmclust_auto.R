#' Fuzzy c-means clustering with automatic fuzziness parameter selection
#'
#' Run fuzzy c-means clustering over a grid of candidate fuzziness parameters,
#' select the best fuzziness parameter using the XB index, and return the
#' corresponding clustering result.
#'
#' This is the recommended default interface for most users.
#'
#' @param x A numeric matrix of dimension \eqn{n \times d}, where \eqn{n}
#'   is the number of observations and \eqn{d} is the number of features.
#' @param nclus An integer specifying the number of clusters.
#' @param vec_m A numeric vector specifying candidate fuzziness parameters.
#' @param nstart An integer specifying the number of random initializations.
#' @param iter_max An integer specifying the maximum number of iterations.
#' @param reltol A numeric value specifying the relative tolerance for convergence.
#' @param verbose A logical value indicating whether to print convergence messages.
#' @param return_all_fits A logical value indicating whether to return all fitted models over `vec_m`.
#'
#' @return An object of class `"fcmclust_auto"` (inheriting from `"fcmclust"`),
#'   corresponding to the selected fuzziness parameter, with additional components:
#' \itemize{
#'   \item best_m: The selected fuzziness parameter.
#'   \item xb: The XB index of the selected solution.
#'   \item vec_m: Candidate fuzziness parameters.
#'   \item xb_grid: XB index values corresponding to `vec_m`.
#'   \item all_fits: A list of fitted models for all candidate values of `m`
#'     if `return_all_fits = TRUE`, otherwise `NULL`.
#' }
#' @export
fcmclust_auto <- function(x,
                          nclus,
                          vec_m = seq(from = 1.1, to = 2.5, by = 0.1),
                          nstart = 10,
                          iter_max = 100,
                          reltol = 1e-4,
                          verbose = FALSE,
                          return_all_fits = FALSE) {
  x <- as.matrix(x)

  if (!is.numeric(x)) {
    stop("x must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (any(is.na(x))) {
    stop("x must not contain missing values.")
  }
  if (!is.numeric(nclus) || length(nclus) != 1 || is.na(nclus) ||
      nclus < 2 || nclus != as.integer(nclus)) {
    stop("nclus must be a single integer greater than or equal to 2.")
  }
  if (!is.numeric(vec_m) || length(vec_m) < 1 || any(is.na(vec_m))) {
    stop("vec_m must be a non-empty numeric vector with no missing values.")
  }
  if (any(vec_m <= 1)) {
    stop("All values in vec_m must be greater than 1.")
  }
  if (!is.numeric(nstart) || length(nstart) != 1 || is.na(nstart) ||
      nstart < 1 || nstart != as.integer(nstart)) {
    stop("nstart must be a single positive integer.")
  }
  if (!is.numeric(iter_max) || length(iter_max) != 1 || is.na(iter_max) ||
      iter_max < 1 || iter_max != as.integer(iter_max)) {
    stop("iter_max must be a single positive integer.")
  }
  if (!is.numeric(reltol) || length(reltol) != 1 || is.na(reltol) || reltol <= 0) {
    stop("reltol must be a single positive numeric value.")
  }
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("verbose must be a single non-missing logical value.")
  }
  if (!is.logical(return_all_fits) || length(return_all_fits) != 1 || is.na(return_all_fits)) {
    stop("return_all_fits must be a single non-missing logical value.")
  }

  nclus <- as.integer(nclus)
  nstart <- as.integer(nstart)
  iter_max <- as.integer(iter_max)
  vec_m <- sort(unique(vec_m))

  list_obj_fcm <- vector("list", length(vec_m))
  xb_grid <- rep(NA_real_, length(vec_m))

  for (idx in seq_along(vec_m)) {
    m <- vec_m[idx]

    obj_fcm <- fcmclust(
      x = x,
      nclus = nclus,
      m = m,
      nstart = nstart,
      iter_max = iter_max,
      reltol = reltol,
      verbose = FALSE
    )

    obj_fcm$xb <- compute_xb(
      x = x,
      u = obj_fcm$membership,
      v = obj_fcm$centers,
      m = 2
    )

    list_obj_fcm[[idx]] <- obj_fcm
    xb_grid[idx] <- obj_fcm$xb

    if (verbose) {
      message("Finished m = ", m, ", XB = ", signif(obj_fcm$xb, 4))
    }
  }

  best_idx <- which.min(xb_grid)
  best_fit <- list_obj_fcm[[best_idx]]

  best_fit$best_m <- vec_m[best_idx]
  best_fit$xb <- xb_grid[best_idx]
  best_fit$vec_m <- vec_m
  best_fit$xb_grid <- xb_grid
  best_fit$all_fits <- if (return_all_fits) list_obj_fcm else NULL

  class(best_fit) <- c("fcmclust_auto", "fcmclust")
  best_fit
}


#' Print an fcmclust_auto object
#'
#' @param x An object of class `"fcmclust_auto"`.
#' @param ... Additional arguments passed to `print()`.
#'
#' @return The input object, invisibly.
#' @export
print.fcmclust_auto <- function(x, ...) {
  cat("Fuzzy c-means clustering with automatic fuzziness selection\n")

  cat("\nSelected m:\n")
  print(x$best_m, ...)

  cat("\nXB index:\n")
  print(x$xb, ...)

  cat("\nCandidate m values:\n")
  print(x$vec_m, ...)

  cat("\nXB index over candidate m (first 10):\n")
  print(data.frame(
    m = utils::head(x$vec_m, 10),
    xb = utils::head(x$xb_grid, 10)
  ), row.names = FALSE, ...)

  if (length(x$xb_grid) > 10) {
    cat("... (truncated, use $xb_grid to view all values)\n")
  }

  cat("\nCorresponding clustering result:\n\n")
  print.fcmclust(x, ...)

  invisible(x)
}
