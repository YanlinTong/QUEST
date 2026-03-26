#' Fuzzy c-means clustering with automatic fuzziness parameter selection
#'
#' Run fuzzy c-means clustering over a grid of candidate fuzziness parameters,
#' select the best fuzziness parameter using the XB index, and return the
#' corresponding clustering result.
#'
#' This is the recommended default interface for most users.
#'
#' @param x A numeric matrix of observations.
#' @param nclus Number of clusters.
#' @param iter.max Maximum number of iterations for each initialization.
#' @param verbose Logical; whether to print progress.
#' @param vec_m A numeric vector of candidate fuzziness parameters.
#' @param reltol Relative tolerance for convergence.
#' @param initialnum Number of random initializations.
#' @param return_all_fits Logical; whether to return all fitted models over `vec_m`.
#'
#' @return An object of class `"fcmclust"` corresponding to the selected
#'   fuzziness parameter, with additional components:
#' \itemize{
#'   \item best_m: The selected fuzziness parameter.
#'   \item xb: The XB index of the selected solution.
#'   \item xb_grid: XB index values corresponding to `vec_m`.
#'   \item vec_m: Candidate fuzziness parameters.
#'   \item all_fits: A list of fitted models for all candidate values of `m`
#'     if `return_all_fits = TRUE`, otherwise `NULL`.
#' }
#' @export
fcmclust_auto <- function(x,
                          nclus,
                          iter.max = 100,
                          verbose = TRUE,
                          vec_m = seq(from = 1.1, to = 2.5, by = 0.1),
                          reltol = 1e-4,
                          initialnum = 10,
                          return_all_fits = FALSE) {
  x <- as.matrix(x)

  if (!is.numeric(x)) {
    stop("x must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (!is.numeric(vec_m) || length(vec_m) < 1) {
    stop("vec_m must be a non-empty numeric vector.")
  }
  if (any(vec_m <= 1)) {
    stop("All values in vec_m must be greater than 1.")
  }

  list_obj_fcm <- vector("list", length(vec_m))
  xb_grid <- rep(NA_real_, length(vec_m))

  for (idx in seq_along(vec_m)) {
    m <- vec_m[idx]

    obj_fcm <- fcmclust(
      x = x,
      nclus = nclus,
      iter.max = iter.max,
      verbose = verbose,
      m = m,
      reltol = reltol,
      initialnum = initialnum
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
      message("Finished m = ", m)
    }
  }

  best_idx <- which.min(xb_grid)
  best_fit <- list_obj_fcm[[best_idx]]

  best_fit$best_m <- vec_m[best_idx]
  best_fit$xb <- xb_grid[best_idx]
  best_fit$xb_grid <- xb_grid
  best_fit$vec_m <- vec_m
  best_fit$all_fits <- if (return_all_fits) list_obj_fcm else NULL

  best_fit
}
